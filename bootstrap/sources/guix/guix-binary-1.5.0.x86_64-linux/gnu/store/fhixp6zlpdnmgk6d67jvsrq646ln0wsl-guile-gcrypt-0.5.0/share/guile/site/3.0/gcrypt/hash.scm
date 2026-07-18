;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2012-2016, 2019-2020, 2022, 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gcrypt hash)
  #:use-module (gcrypt utils)
  #:use-module (gcrypt internal)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (system foreign)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (hash-algorithm
            lookup-hash-algorithm
            hash-algorithm-name
            hash-size

            bytevector-hash
            open-hash-port
            port-hash
            file-hash
            open-hash-input-port

            open-sha256-port
            port-sha256
            file-sha256
            open-sha256-input-port))

;;; Commentary:
;;;
;;; Cryptographic hashes.
;;;
;;; Code:


;;;
;;; Hash.
;;;

(define-syntax-rule (define-hash-algorithms name->integer
                      symbol->integer integer->symbol hash-size
                      (name id size) ...)
  "Define hash algorithms with their NAME, numerical ID, and SIZE in bytes."
  (begin
    ;; Make sure NAME is bound to follow best practices for syntax matching
    ;; (info "(guile) Syntax Rules").  As a bonus, it provides convenient
    ;; shorthand procedures.
    (define-public name
      (cut bytevector-hash <> id))
    ...

    (define-enumerate-type name->integer symbol->integer integer->symbol
      (name id) ...)

    (define-lookup-procedure hash-size
      "Return the size in bytes of a digest of the given hash algorithm."
      (id size) ...)))

(define %hash-size
  ;; This procedure was used to double-check the hash sizes below.  (We
  ;; cannot use it at macro-expansion time because it wouldn't work when
  ;; cross-compiling.)
  (libgcrypt->procedure unsigned-int
                        "gcry_md_get_algo_dlen"
                        (list int)))

;; 'GCRY_MD_' values as of Libgcrypt 1.11.0.
(define-hash-algorithms hash-algorithm
  lookup-hash-algorithm hash-algorithm-name
  hash-size

  (md5 1 16)
  (sha1 2 20)
  (rmd160 3 20)
  ;; (md2 5 0)
  (tiger 6 24)   ;TIGER/192 as used by gpg <= 1.3.2
  (haval 7 20)   ;HAVAL, 5 pass, 160 bit
  (sha256 8 32)
  (sha384 9 48)
  (sha512 10 64)
  (sha224 11 28)

  (md4 301 16)
  (crc32 302 4)
  (crc32-rfc1510 303 4)
  (crc24-rfc2440 304 3)
  (whirlpool 305 64)
  (tiger1 306 24) ;TIGER fixed
  (tiger2 307 24) ;TIGER2 variant
  (gostr3411-94 308 32) ;GOST R 34.11-94
  (stribog256 309 32) ;GOST R 34.11-2012, 256 bit
  (stribog512 310 64) ;GOST R 34.11-2012, 512 bit
  (gostr3411-cp 311 32) ;GOST R 34.11-94 with CryptoPro-A S-Box
  (sha3-224 312 28)
  (sha3-256 313 32)
  (sha3-384 314 48)
  (sha3-512 315 64)
  ;; (shake128 316 0)
  ;; (shake256 317 0)
  (blake2b-512 318 64)
  (blake2b-384 319 48)
  (blake2b-256 320 32)
  (blake2b-160 321 20)
  (blake2s-256 322 32)
  (blake2s-224 323 28)
  (blake2s-160 324 20)
  (blake2s-128 325 16)
  (sm3         326 32)
  (sha512-256  327 32)
  (sha512-224  328 28)
  (cshake128   329 32)
  (cshake256   330 64))


(define bytevector-hash
  (let ((proc (libgcrypt->procedure void
                                    "gcry_md_hash_buffer"
                                    `(,int * * ,size_t))))
    (lambda (bv algorithm)
      "Return the hash ALGORITHM of BV as a bytevector."
      (let ((digest (make-bytevector (hash-size algorithm))))
        (proc algorithm (bytevector->pointer digest)
              (bytevector->pointer bv) (bytevector-length bv))
        digest))))

(define open-md
  (let ((proc (libgcrypt->procedure int
                                    "gcry_md_open"
                                    `(* ,int ,unsigned-int))))
    (lambda (algorithm)
      (let* ((md  (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (proc md algorithm 0)))
        (if (zero? err)
            (dereference-pointer md)
            (throw 'gcrypt-error err))))))

(define md-write
  (libgcrypt->procedure void "gcry_md_write" `(* * ,size_t)))

(define md-read
  (libgcrypt->procedure '* "gcry_md_read" `(* ,int)))

(define md-close
  (libgcrypt->procedure void "gcry_md_close" '(*)))

(define (open-hash-port algorithm)
  "Return two values: an output port, and a thunk.  When the thunk is called,
it returns the hash (a bytevector) for ALGORITHM of all the data written to the
output port."
  (define md
    (open-md algorithm))

  (define md-size
    (hash-size algorithm))

  (define digest #f)
  (define position 0)

  (define (finalize!)
    (let ((ptr (md-read md 0)))
      (set! digest (bytevector-copy (pointer->bytevector ptr md-size)))
      (md-close md)))

  (define (write! bv offset len)
    (if (zero? len)
        (begin
          (finalize!)
          0)
        (let ((ptr (bytevector->pointer bv offset)))
          (md-write md ptr len)
          (set! position (+ position len))
          len)))

  (define (get-position)
    position)

  (define (close)
    (unless digest
      (finalize!)))

  (values (make-custom-binary-output-port "hash"
                                          write! get-position #f
                                          close)
          (lambda ()
            (unless digest
              (finalize!))
            digest)))

(define (port-hash algorithm port)
  "Return the ALGORITHM hash (a bytevector) of all the data drained from
PORT."
  (let-values (((out get)
                (open-hash-port algorithm)))
    (dump-port port out)
    (close-port out)
    (get)))

(define (file-hash algorithm file)
  "Return the ALGORITHM hash (a bytevector) of FILE."
  (call-with-input-file file
    (cut port-hash algorithm <>)))

(define (open-hash-input-port algorithm port)
  "Return an input port that wraps PORT and a thunk to get the hash of all the
data read from PORT.  The thunk always returns the same value."
  (define md
    (open-md algorithm))

  (define md-size
    (hash-size algorithm))

  (define (read! bv start count)
    (let ((n (get-bytevector-n! port bv start count)))
      (if (eof-object? n)
          0
          (begin
            (unless digest
              (let ((ptr (bytevector->pointer bv start)))
                (md-write md ptr n)))
            n))))

  (define digest #f)

  (define (finalize!)
    (let ((ptr (md-read md 0)))
      (set! digest (bytevector-copy (pointer->bytevector ptr md-size)))
      (md-close md)))

  (define (get-hash)
    (unless digest
      (finalize!))
    digest)

  (define (unbuffered port)
    ;; Guile <= 2.0.9 does not support 'setvbuf' on custom binary input ports.
    ;; If you get a wrong-type-arg error here, the fix is to upgrade Guile.  :-)
    (setvbuf port
             (cond-expand ((and guile-2 (not guile-2.2)) _IONBF)
                          (else 'none)))
    port)

  (values (unbuffered (make-custom-binary-input-port "hash-input"
                                                     read! #f #f #f))
          get-hash))

(define open-sha256-port
  (cut open-hash-port (hash-algorithm sha256)))
(define port-sha256
  (cut port-hash (hash-algorithm sha256) <>))
(define file-sha256
  (cut file-hash (hash-algorithm sha256) <>))
(define open-sha256-input-port
  (cut open-hash-input-port (hash-algorithm sha256) <>))

;;; hash.scm ends here
