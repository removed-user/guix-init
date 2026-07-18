;;; Disarchive
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive binary-filenames)
  #:use-module (disarchive config)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (lstat/binary
            lstat/utf8
            open-input-file/binary
            open-input-file/utf8))

;;; Commentary:
;;;
;;; This module enables opening files with binary filenames.  Normally
;;; Guile uses the current locale to encode strings before treating
;;; them as filenames.  This module avoids this by calling 'open' from
;;; Libc directly, making use of either 'bytevector->pointer' or the
;;; 'encoding' parameter of 'string->pointer'.
;;;
;;; Code:

(define libc (dynamic-link))

(define %open
  (let* ((fptr (dynamic-func "open" libc))
         (f (pointer->procedure int fptr `(* ,int ,unsigned-int)
                                #:return-errno? #t)))
    (lambda (filename pointer flags mode)
      (call-with-values
          (lambda ()
            (f pointer flags mode))
        (lambda (result errno)
          (unless (> result -1)
            (scm-error 'system-error '%open "~A: ~S"
                       (list (strerror errno) filename)
                       (list errno)))
          result)))))

(define (binary-string->pointer bv)
  (let ((bvz (make-bytevector (1+ (bytevector-length bv)) 0)))
    (bytevector-copy! bv 0 bv 0 (bytevector-length bv))
    bvz))

(define (lstat/pointer filename pointer)
  (let* ((fd (%open filename pointer (logior O_RDONLY O_NOFOLLOW) 0))
         (st (stat fd)))
    (close-fdes fd)
    st))

(define (lstat/binary filename)
  (lstat/pointer filename (binary-string->pointer filename)))

(define (lstat/utf8 filename)
  (lstat/pointer filename (string->pointer filename "UTF-8")))

(define* (open-input-file/pointer filename pointer #:key binary?)
  (let ((fd (%open filename pointer O_RDONLY 0)))
    (fdopen fd (if binary? "rb" "r"))))

(define* (open-input-file/binary filename #:key binary?)
  (open-input-file/pointer filename (binary-string->pointer filename)
                           #:binary? binary?))

(define* (open-input-file/utf8 filename #:key binary?)
  (open-input-file/pointer filename (string->pointer filename "UTF-8")
                           #:binary? binary?))
