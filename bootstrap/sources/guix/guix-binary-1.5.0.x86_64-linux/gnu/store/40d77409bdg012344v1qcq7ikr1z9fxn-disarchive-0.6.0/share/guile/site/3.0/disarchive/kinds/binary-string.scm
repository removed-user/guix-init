;;; Disarchive
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020, 2021 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Disarchive.
;;;
;;; Disarchive is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Disarchive is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Disarchive.  If not, see <http://www.gnu.org/licenses/>.

(define-module (disarchive kinds binary-string)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (gcrypt base64)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (binary-string?
            valid-binary-string?
            no-null-binary-string?
            decode-binary-string
            encode-binary-string
            -binary-string-
            binary-string-length
            binary-string-append))

;;; Commentary:
;;;
;;; A binary string is a representation of a sequence of bytes that is
;;; opportunistically decoded as UTF-8.  What this means is that any
;;; sequence of bytes that is valid UTF-8 will treated as UTF-8 (even
;;; if it isn't).  However, a sequence of bytes that is not valid
;;; UTF-8 will be preserved as a bytevector.
;;;
;;; Code:

(define (binary-string? obj)
  "Check if OBJ is a \"binary string\" (either a string or a
bytevector)."
  (or (string? obj) (bytevector? obj)))

(define (valid-binary-string? str)
  "Check that STR satisfies the constraints of a binary string."
  ;; In the case that STR is a bytevector, we must check that it is
  ;; not valid UTF-8.  Otherwise, it should be a string.
  (define (utf8? bv) (false-if-exception (utf8->string bv)))
  (match str
    ((or (? string?)
         (and (? bytevector?)
              (? (negate utf8?))))
     #t)
    (_ #f)))

(define (no-null-binary-string? str)
  "Check that STR does not contain any nulls ('#\nul' for strings and
'0' for bytevectors)."
  (match str
    ((? string?) (not (string-any #\nul str)))
    ((? bytevector?) (let loop ((k 0))
                       (if (>= k (bytevector-length str))
                           #t
                           (if (zero? (bytevector-u8-ref str k))
                               #f
                               (loop (1+ k))))))
    (_ (scm-error 'wrong-type-arg 'no-null-binary-string
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting binary-string): ~A")
                  (list str) (list str)))))

(define decode-binary-string
  (case-lambda
    "Decode the contents of the bytevector BV as a binary string.
Optionally, START and END indexes can be provided to decode only a
part of BV."
    ((bv) (or (false-if-exception (utf8->string bv)) bv))
    ((bv start) (decode-binary-string bv start (bytevector-length bv)))
    ((bv start end) (decode-binary-string (sub-bytevector bv start end)))))

(define* encode-binary-string
  (case-lambda
    "Encode the binary string STR.  If BV is set, the result will be
written into BV.  Otherwise, the result will be written into a new
bytevector.  If you are providing a bytevector, you can also provide
START and END indexes to control where the result is written."
    ((str)
     (match str
       ((? string?) (string->utf8 str))
       ((? bytevector?) str)
       (_ (scm-error 'wrong-type-arg 'encode-binary-string
                     (string-append "Wrong type argument in position 1 "
                                    "(expecting binary-string): ~A")
                     (list str) (list str)))))
    ((str bv)
     (encode-binary-string str bv 0 (bytevector-length bv)))
    ((str bv start)
     (encode-binary-string str bv start (bytevector-length bv)))
    ((str bv start end)
     (let* ((bstr (encode-binary-string str))
            (bstr-len (bytevector-length bstr))
            (space (- end start))
            (leftover-space (- space bstr-len)))
       (bytevector-copy! bstr 0 bv start (min bstr-len (- end start)))
       (when (positive? leftover-space)
         (bytevector-fill!* bv 0 end leftover-space))))))

(define (binary-string->sexp str)
  (match str
    ((? string?) str)
    ((? bytevector?) `(%base64 ,(base64-encode str)))
    (_ (scm-error 'wrong-type-arg 'binary-string->sexp
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting binary-string): ~A")
                  (list str) (list str)))))

(define (sexp->binary-string obj)
  (match obj
    ((? string?) obj)
    (('%base64 (? string? str)) (base64-decode str))
    (_ (scm-error 'misc-error 'sexp->binary-string
                  (string-append "Invalid binary string S-exp: ~A")
                  (list obj) (list obj)))))

(define -binary-string-
  (make-serializer
   (lambda (str _) (list (binary-string->sexp str)))
   (lambda (obj _) (sexp->binary-string (car obj)))))

(define (binary-string-length str)
  "Return the length (in bytes) of the binary representation of STR."
  (match str
    ((? string?) (string-utf8-length str))
    ((? bytevector?) (bytevector-length str))
    (_ (scm-error 'wrong-type-arg 'binary-string-length
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting binary-string): ~A")
                  (list str) (list str)))))

(define (binary-string-append . strs)
  (if (every string? strs)
      (string-concatenate strs)
      (let* ((len (reduce + 0 (map binary-string-length strs)))
             (result (make-bytevector len)))
        (let loop ((strs strs) (k 0))
          (match strs
            (() result)
            (((? string? str) . rest)
             (loop (cons (string->utf8 str) rest) k))
            (((? bytevector? bv) . rest)
             (bytevector-copy! bv 0 result k (bytevector-length bv))
             (loop rest (+ k (bytevector-length bv)))))))))
