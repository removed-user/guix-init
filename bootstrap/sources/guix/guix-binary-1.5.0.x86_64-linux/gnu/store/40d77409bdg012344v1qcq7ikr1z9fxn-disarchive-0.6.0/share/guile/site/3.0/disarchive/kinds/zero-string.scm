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

(define-module (disarchive kinds zero-string)
  #:use-module (disarchive kinds binary-string)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9 gnu)
  #:export (<zero-string>
            make-zero-string
            zero-string?
            zero-string-value
            zero-string-trailer
            valid-zero-string?
            decode-zero-string
            encode-zero-string
            -zero-string-))

;;; Commentary:
;;;
;;; A zero string represents a fixed-length, null-terminated binary
;;; string.  It does this with two fields, "value" and "trailer".  The
;;; "value" field is a binary string made up of the bytes before the
;;; first null byte (or all the bytes if there is no null byte).  The
;;; "trailer" field is a binary string made up of the null byte and
;;; all of the bytes after it.  If the trailer field is entirely null
;;; bytes, it is represented as the null string ("").
;;;
;;; Code:

(define-immutable-record-type <zero-string>
  (make-zero-string value trailer)
  zero-string?
  (value zero-string-value)
  (trailer zero-string-trailer))

(define (valid-zero-string? zstr)
  "Check that ZSTR satisfies the constraints of a zero string."
  ;; The value field must not contain any zeros ('#\nul' for strings
  ;; and '0' for bytevectors).
  (match zstr
    (($ <zero-string>
        (and (? valid-binary-string?)
             (? no-null-binary-string?))
        (? valid-binary-string?))
     #t)
    (_ #f)))

(define* (decode-zero-string bv #:optional (start 0)
                             (end (bytevector-length bv)))
  "Decode the contents of the bytevector BV as a zero string.
Optionally, START and END indexes can be provided to decode only a
part of BV."
  (let* ((k (or (bytevector-index bv 0 start end) end))
         (trailer (if (bytevector-zero? bv k end)
                      ""
                      (decode-binary-string bv (1+ k) end))))
    (make-zero-string (decode-binary-string bv start k)
                      trailer)))

(define* (encode-zero-string zstr #:optional bv (start 0) end)
  "Encode the zero string ZSTR.  If BV is set, the result will be
written into BV.  Otherwise, the result will be written into a new
bytevector.  If you are providing a bytevector, you can also provide
START and END indexes to control where the result is written."
  (match zstr
    (($ <zero-string> str trailer)
     (let* ((str-len (binary-string-length str))
            (trailer-start (+ start str-len 1))
            (trailer-len (binary-string-length trailer))
            (bv (or bv (make-bytevector (+ str-len 1 trailer-len))))
            (end (or end (bytevector-length bv))))
       (encode-binary-string str bv start end)
       ;; Note that 'encode-binary-string' zeros out the rest of the
       ;; bytevector up to the end index.  This means that we can
       ;; ignore null trailers, since the zeros are already there.
       (unless (or (zero? trailer-len) (>= trailer-start end))
         (encode-binary-string trailer bv trailer-start end))
       bv))
    (_ (scm-error 'wrong-type-arg 'encode-zero-string
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting zero-string): ~A")
                  (list zstr) (list zstr)))))

(define -zero-string-
  (make-record-serializer
   make-zero-string
   `((value ,zero-string-value ,-binary-string-)
     (trailer ,zero-string-trailer ,-binary-string-))
   #:elide-first-field? #t))
