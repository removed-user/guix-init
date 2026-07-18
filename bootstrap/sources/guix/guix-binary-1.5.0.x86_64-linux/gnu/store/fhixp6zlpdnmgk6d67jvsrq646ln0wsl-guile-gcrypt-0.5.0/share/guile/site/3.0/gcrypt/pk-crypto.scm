;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2013-2015, 2017, 2019-2020, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;;
;;; This file is part of guile-gcrypt.
;;;
;;; guile-gcrypt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; guile-gcrypt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with guile-gcrypt.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gcrypt pk-crypto)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt internal)
  #:use-module (gcrypt common)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (canonical-sexp?
            string->canonical-sexp
            canonical-sexp->string
            read-file-sexp
            number->canonical-sexp
            canonical-sexp-car
            canonical-sexp-cdr
            canonical-sexp-nth
            canonical-sexp-nth-data
            canonical-sexp-length
            canonical-sexp-null?
            canonical-sexp-list?
            bytevector->hash-data
            hash-data->bytevector
            key-type
            sign
            verify
            generate-key
            find-sexp-token
            canonical-sexp->sexp
            sexp->canonical-sexp)
  #:re-export (gcrypt-version))


;;; Commentary:
;;;
;;; Public key cryptographic routines from GNU Libgcrypt.
;;;;
;;; Libgcrypt uses "canonical s-expressions" to represent key material,
;;; parameters, and data.  We keep it as an opaque object to map them to
;;; Scheme s-expressions because (1) Libgcrypt sexps may be stored in secure
;;; memory, and (2) the read syntax is different.
;;;
;;; A 'canonical-sexp->sexp' procedure is provided nevertheless, for use in
;;; cases where it is safe to move data out of Libgcrypt---e.g., when
;;; processing ACL entries, public keys, etc.
;;;
;;; Canonical sexps were defined by Rivest et al. in the IETF draft at
;;; <http://people.csail.mit.edu/rivest/Sexp.txt> for the purposes of SPKI
;;; (see <http://www.ietf.org/rfc/rfc2693.txt>.)
;;;
;;; Code:

;; Libgcrypt "s-expressions".
(define-wrapped-pointer-type <canonical-sexp>
  canonical-sexp?
  naked-pointer->canonical-sexp
  canonical-sexp->pointer
  (lambda (obj port)
    ;; Don't print OBJ's external representation: we don't want key material
    ;; to leak in backtraces and such.
    (format port "#<canonical-sexp ~a | ~a>"
            (number->string (object-address obj) 16)
            (number->string (pointer-address (canonical-sexp->pointer obj))
                            16))))

(define finalize-canonical-sexp!
  (libgcrypt->pointer "gcry_sexp_release"))

(define-inlinable (pointer->canonical-sexp ptr)
  "Return a <canonical-sexp> that wraps PTR."
  (let* ((sexp (naked-pointer->canonical-sexp ptr))
         (ptr* (canonical-sexp->pointer sexp)))
    ;; Did we already have a <canonical-sexp> object for PTR?
    (when (equal? ptr ptr*)
      ;; No, so we can safely add a finalizer (in Guile 2.0.9
      ;; 'set-pointer-finalizer!' *adds* a finalizer rather than replacing the
      ;; existing one.)
      (set-pointer-finalizer! ptr finalize-canonical-sexp!))
    sexp))

(define string->canonical-sexp
  (let ((proc (libgcrypt->procedure int
                                    "gcry_sexp_new"
                                    `(* * ,size_t ,int))))
    (lambda (str)
      "Parse STR and return the corresponding gcrypt s-expression."

      ;; When STR comes from 'canonical-sexp->string', it may contain
      ;; characters that are really meant to be interpreted as bytes as in a C
      ;; 'char *'.  Thus, convert STR to ISO-8859-1 so the byte values of the
      ;; characters are preserved.
      (let* ((sexp (bytevector->pointer (make-bytevector (sizeof '*))))
             (err  (proc sexp (string->pointer str "ISO-8859-1") 0 1)))
        (if (= 0 err)
            (pointer->canonical-sexp (dereference-pointer sexp))
            (throw 'gcry-error 'string->canonical-sexp err))))))

(define-syntax GCRYSEXP_FMT_ADVANCED
  (identifier-syntax 3))

(define canonical-sexp->string
  (let ((proc (libgcrypt->procedure size_t
                                    "gcry_sexp_sprint"
                                    `(* ,int * ,size_t))))
    (lambda (sexp)
      "Return a textual representation of SEXP."
      (let loop ((len 1024))
        (let* ((buf  (bytevector->pointer (make-bytevector len)))
               (size (proc (canonical-sexp->pointer sexp)
                           GCRYSEXP_FMT_ADVANCED buf len)))
          (if (zero? size)
              (loop (* len 2))
              (pointer->string buf size "ISO-8859-1")))))))

(define (read-file-sexp file)
  "Return the canonical sexp read from FILE."
  (call-with-input-file file
    (compose string->canonical-sexp
             read-string)))

(define canonical-sexp-car
  (let ((proc (libgcrypt->procedure '* "gcry_sexp_car" '(*))))
    (lambda (lst)
      "Return the first element of LST, an sexp, if that element is a list;
return #f if LST or its first element is not a list (this is different from
the usual Lisp 'car'.)"
      (let ((result (proc (canonical-sexp->pointer lst))))
        (if (null-pointer? result)
            #f
            (pointer->canonical-sexp result))))))

(define canonical-sexp-cdr
  (let ((proc (libgcrypt->procedure '* "gcry_sexp_cdr" '(*))))
    (lambda (lst)
      "Return the tail of LST, an sexp, or #f if LST is not a list."
      (let ((result (proc (canonical-sexp->pointer lst))))
        (if (null-pointer? result)
            #f
            (pointer->canonical-sexp result))))))

(define canonical-sexp-nth
  (let ((proc (libgcrypt->procedure '* "gcry_sexp_nth" `(* ,int))))
    (lambda (lst index)
      "Return the INDEXth nested element of LST, an s-expression.  Return #f
if that element does not exist, or if it's an atom.  (Note: this is obviously
different from Scheme's 'list-ref'.)"
      (let ((result (proc (canonical-sexp->pointer lst) index)))
        (if (null-pointer? result)
            #f
            (pointer->canonical-sexp result))))))

(define (dereference-size_t p)
  "Return the size_t value pointed to by P."
  (bytevector-uint-ref (pointer->bytevector p (sizeof size_t))
                       0 (native-endianness)
                       (sizeof size_t)))

(define canonical-sexp-length
  (let ((proc (libgcrypt->procedure int "gcry_sexp_length" '(*))))
    (lambda (sexp)
      "Return the length of SEXP if it's a list (including the empty list);
return zero if SEXP is an atom."
      (proc (canonical-sexp->pointer sexp)))))

(define token-string?
  (let ((token-cs (char-set-intersection
                   ;; The Sexp specification defines alphabetic and
                   ;; digits in terms of US-ASCII, so limit the Guile
                   ;; character sets to the subset of ASCII.
                   char-set:ascii
                   (char-set-union char-set:digit
                                   char-set:letter
                                   (char-set #\- #\. #\/ #\_
                                             #\: #\* #\+ #\=)))))
    (lambda (str)
      "Return #t if STR is a token as per Section 4.3 of
<http://people.csail.mit.edu/rivest/Sexp.txt>."
      (and (not (string-null? str))
           (string-every token-cs str)
           (not (char-set-contains? char-set:digit (string-ref str 0)))))))

(define canonical-sexp-nth-data
  (let ((proc (libgcrypt->procedure '* "gcry_sexp_nth_data" `(* ,int *))))
    (lambda* (lst index #:key (possibly-token? #t))
      "Return as a symbol (for \"sexp tokens\", and if POSSIBLY-TOKEN? is
true) or a bytevector (for any other \"octet string\") the INDEXth data
element (atom) of LST, an s-expression.  Return #f if that element does not
exist, or if it's a list."
      (let* ((size*  (bytevector->pointer (make-bytevector (sizeof '*))))
             (result (proc (canonical-sexp->pointer lst) index size*)))
        (if (null-pointer? result)
            #f
            (let* ((len (dereference-size_t size*))
                   (str (pointer->string result len "ISO-8859-1")))
              ;; The sexp spec speaks of "tokens" and "octet strings".
              ;; Sometimes these octet strings are actual strings (text),
              ;; sometimes they're bytevectors, and sometimes they're
              ;; multi-precision integers (MPIs).  Only the application knows.
              ;; However, for convenience, we return a symbol when a token is
              ;; encountered since tokens are frequent (at least in the 'car'
              ;; of each sexp.)  See 'suitable_encoding' in libgcrypt for how
              ;; the function determines whether to return a token or not.
              (if (and possibly-token? (token-string? str))
                  (string->symbol str)   ; an sexp "token"
                  (bytevector-copy       ; application data, textual or binary
                   (pointer->bytevector result len)))))))))

(define (number->canonical-sexp number)
  "Return an s-expression representing NUMBER."
  (let ((hex-number
         (match (number->string number 16)
           ;; Append a 0 if necessary.  For whatever reason gcrypt
           ;; rejects hex numbers that don't have an even number of
           ;; digits.
           ((? (lambda (s) (odd? (string-length s))) odd-string)
            (string-append "0" odd-string))
           (even-str even-str))))
    (string->canonical-sexp (string-append "#" hex-number "#"))))

(define* (bytevector->hash-data bv
                                #:optional
                                (hash-algo "sha256")
                                #:key (key-type 'ecc))
  "Given BV, a bytevector containing a hash of type HASH-ALGO, return an
s-expression suitable for use as the 'data' argument for 'sign'.  KEY-TYPE
must be a symbol: 'dsa, 'ecc, or 'rsa."
  (string->canonical-sexp
   (format #f "(data (flags ~a) (hash \"~a\" #~a#))"
           (case key-type
             ((ecc dsa) "rfc6979")
             ((rsa)     "pkcs1")
             (else (error "unknown key type" key-type)))
           hash-algo
           (bytevector->base16-string bv))))

(define (key-type sexp)
  "Return a symbol denoting the type of public or private key represented by
SEXP--e.g., 'rsa', 'ecc'--or #f if SEXP does not denote a valid key."
  (case (canonical-sexp-nth-data sexp 0)
    ((public-key private-key)
     (canonical-sexp-nth-data (canonical-sexp-nth sexp 1) 0))
    (else #f)))

(define* (hash-data->bytevector data)
  "Return two values: the hash value (a bytevector), and the hash algorithm (a
string) extracted from DATA, an sexp as returned by 'bytevector->hash-data'.
Return #f if DATA does not conform."
  (let ((hash (find-sexp-token data 'hash)))
    (if hash
        (let ((algo  (canonical-sexp-nth-data hash 1))
              (value (canonical-sexp-nth-data hash 2 #:possibly-token? #f)))
          (values value (symbol->string algo)))
        (values #f #f))))

(define sign
  (let ((proc (libgcrypt->procedure int "gcry_pk_sign" '(* * *))))
    (lambda (data secret-key)
      "Sign DATA, a canonical s-expression representing a suitable hash, with
SECRET-KEY (a canonical s-expression whose car is 'private-key'.)  Note that
DATA must be a 'data' s-expression, as returned by
'bytevector->hash-data' (info \"(gcrypt) Cryptographic Functions\")."
      (let* ((sig (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (proc sig (canonical-sexp->pointer data)
                        (canonical-sexp->pointer secret-key))))
        (if (= 0 err)
            (pointer->canonical-sexp (dereference-pointer sig))
            (throw 'gcry-error 'sign err))))))

(define verify
  (let ((proc (libgcrypt->procedure int "gcry_pk_verify" '(* * *))))
    (lambda (signature data public-key)
      "Verify that SIGNATURE is a signature of DATA with PUBLIC-KEY, all of
which are gcrypt s-expressions; return #t if the verification was successful,
#f otherwise.  Raise an error if, for example, one of the given s-expressions
is invalid."
      (let ((err (proc (canonical-sexp->pointer signature)
                       (canonical-sexp->pointer data)
                       (canonical-sexp->pointer public-key))))
        (cond ((zero? err) #t)
              ((error-code=? error/bad-signature err) #f)
              (else (throw 'gcry-error 'verify err)))))))

(define generate-key
  (let ((proc (libgcrypt->procedure int "gcry_pk_genkey" '(* *))))
    (lambda (params)
      "Return as an s-expression a new key pair for PARAMS.  PARAMS must be an
s-expression like: (genkey (rsa (nbits 4:2048)))."
      (let* ((key (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (proc key (canonical-sexp->pointer params))))
        (if (zero? err)
            (pointer->canonical-sexp (dereference-pointer key))
            (throw 'gcry-error 'generate-key err))))))

(define find-sexp-token
  (let ((proc (libgcrypt->procedure '*
                                    "gcry_sexp_find_token"
                                    `(* * ,size_t))))
    (lambda (sexp token)
      "Find in SEXP the first element whose 'car' is TOKEN and return it;
return #f if not found."
      (let* ((token (string->pointer (symbol->string token)))
             (res   (proc (canonical-sexp->pointer sexp) token 0)))
        (if (null-pointer? res)
            #f
            (pointer->canonical-sexp res))))))

(define-inlinable (canonical-sexp-null? sexp)
  "Return #t if SEXP is the empty-list sexp."
  (null-pointer? (canonical-sexp->pointer sexp)))

(define (canonical-sexp-list? sexp)
  "Return #t if SEXP is a list."
  (or (canonical-sexp-null? sexp)
      (> (canonical-sexp-length sexp) 0)))

(define (canonical-sexp-fold proc seed sexp)
  "Fold PROC (as per SRFI-1) over SEXP, a canonical sexp."
  (if (canonical-sexp-list? sexp)
      (let ((len (canonical-sexp-length sexp)))
        (let loop ((index  0)
                   (result seed))
          (if (= index len)
              result
              (loop (+ 1 index)
                    ;; XXX: Call 'nth-data' *before* 'nth' to work around
                    ;; <https://dev.gnupg.org/T1594>, which
                    ;; affects 1.6.0 and earlier versions.
                    (proc (or (canonical-sexp-nth-data sexp index)
                              (canonical-sexp-nth sexp index))
                          result)))))
      (error "sexp is not a list" sexp)))

(define (canonical-sexp->sexp sexp)
  "Return a Scheme sexp corresponding to SEXP.  This is particularly useful to
compare sexps (since Libgcrypt does not provide an 'equal?' procedure), or to
use pattern matching."
  (if (canonical-sexp-list? sexp)
      (reverse
       (canonical-sexp-fold (lambda (item result)
                              (cons (if (canonical-sexp? item)
                                        (canonical-sexp->sexp item)
                                        item)
                                    result))
                            '()
                            sexp))

      ;; As of Libgcrypt 1.6.0, there's no function to extract the buffer of a
      ;; non-list sexp (!), so we first enlist SEXP, then get at its buffer.
      (let ((sexp (string->canonical-sexp
                   (string-append "(" (canonical-sexp->string sexp)
                                  ")"))))
        (or (canonical-sexp-nth-data sexp 0)
            (canonical-sexp-nth sexp 0)))))

(define (sexp->canonical-sexp sexp)
  "Return a canonical sexp equivalent to SEXP, a Scheme sexp as returned by
'canonical-sexp->sexp'."
  (define (string-hex-pad str)
    (if (odd? (string-length str))
        (string-append "0" str)
        str))

  ;; XXX: This is inefficient, but the Libgcrypt API doesn't allow us to do
  ;; much better.
  (string->canonical-sexp
    (call-with-output-string
     (lambda (port)
       (define (write item)
         (cond ((list? item)
                (display "(" port)
                (for-each write item)
                (display ")" port))
               ((symbol? item)
                (format port " ~a" item))
               ((bytevector? item)
                (format port " #~a#"
                        (bytevector->base16-string item)))
               ((integer? item)
                (format port " #~a#"
                        (string-hex-pad (number->string item 16))))
               (else
                (error "unsupported sexp item type" item))))

       (write sexp)))))

;;; pk-crypto.scm ends here
