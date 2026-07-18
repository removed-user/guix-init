;;; Disarchive
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

(define-module (disarchive kinds octal)
  #:use-module (disarchive kinds binary-string)
  #:use-module (disarchive kinds zero-string)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-71)
  #:export (<padded-octal>
            make-padded-octal
            padded-octal?
            padded-octal-value
            padded-octal-width
            padded-octal-padding
            padded-octal-trailer

            <unstructured-octal>
            make-unstructured-octal
            unstructured-octal?
            unstructured-octal-value
            unstructured-octal-source

            octal?
            valid-octal?
            octal-value
            set-octal-value
            decode-octal
            encode-octal
            -octal-))

;;; Commentary:
;;;
;;; A formatted octal value represents a number that comes from a
;;; sequence of octal digits with a specific format.  For example,
;;; "00010" would have the value 8 with width 5 and padding "0".
;;;
;;; Code:

(define-immutable-record-type <padded-octal>
  (make-padded-octal value width padding trailer)
  padded-octal?
  (value padded-octal-value set-padded-octal-value)
  (width padded-octal-width)
  (padding padded-octal-padding)
  (trailer padded-octal-trailer))

(define (natural? n)
  (and (exact? n) (integer? n) (not (negative? n))))

(define (valid-padded-octal? octal)
  ;; We check three properties here.  First, the padding character
  ;; must not be a nonzero octal digit.  Second, the width must be
  ;; large enough to accomodate the value.  Third, if the padding
  ;; character is not used (because the size of the value is the same
  ;; as the width) it must be '#\0'.
  (and (match octal
         (($ <padded-octal>
             (? natural?)
             (? natural?)
             (and (? char?)
                  (? (lambda (x)
                       (not (char-set-contains? char-set:octal-nonzero x)))))
             (? valid-binary-string?))
          #t)
         (_ #f))
       (let* ((value (padded-octal-value octal))
              (width (padded-octal-width octal))
              (padding (padded-octal-padding octal))
              (size (string-length (number->string value 8))))
         (and (<= size width)
              (or (char=? padding #\0)
                  (< size width))))))

(define-immutable-record-type <unstructured-octal>
  (make-unstructured-octal value source)
  unstructured-octal?
  (value unstructured-octal-value set-unstructured-octal-value)
  (source unstructured-octal-source))

(define (extract-octal str)
  (match (string-index str char-set:octal)
    (#f #f)
    (start (let ((end (or (string-index str char-set:non-octal start)
                          (string-length str))))
             (string->number (substring str start end) 8)))))

(define (valid-unstructured-octal? octal)
  (and (match octal
         (($ <unstructured-octal>
             (? natural?)
             (? valid-zero-string?))
          #t)
         (_ #f))
       ;; Check that we are dealing with an unstructured octal and not
       ;; something that would be better represented as a padded octal.
       (let* ((zstr (unstructured-octal-source octal))
              (str (zero-string-value zstr))
              (trailer (zero-string-trailer zstr)))
         (or (not (string? str))
             (not (string->padded-octal str trailer))))
       ;; Check that the value corresponds to the source.
       (match (zero-string-value (unstructured-octal-source octal))
         ((? string? str) (= (or (extract-octal str) 0)
                             (unstructured-octal-value octal)))
         (_ (zero? (unstructured-octal-value octal))))))

(define (octal? obj)
  "Check if OBJ is a formatted octal value."
  (match obj
    ((? padded-octal?) #t)
    ((? unstructured-octal?) #t)
    (_ #f)))

(define (valid-octal? octal)
  "Check that OCTAL satisfies the constraints of a formatted octal
value."
  (or (valid-padded-octal? octal)
      (valid-unstructured-octal? octal)))

(define (octal-value octal)
  (match octal
    ((? padded-octal?) (padded-octal-value octal))
    ((? unstructured-octal?) (unstructured-octal-value octal))
    (_ (scm-error 'wrong-type-arg 'octal-value
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting octal): ~A")
                  (list octal) (list octal)))))

(define (set-octal-value octal value)
  (match octal
    ((? padded-octal?) (set-padded-octal-value octal value))
    ((? unstructured-octal?) (set-unstructured-octal-value octal value))
    (_ (scm-error 'wrong-type-arg 'set-octal-value
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting octal): ~A")
                  (list octal) (list octal)))))

(define (string-first str)
  "Get the first character in STR."
  (string-ref str 0))

(define char-set:octal (string->char-set "01234567"))
(define char-set:octal-nonzero (string->char-set "1234567"))
(define char-set:non-octal (char-set-complement char-set:octal))

(define* (string->padded-octal str #:optional (trailer ""))
  (define width (string-length str))
  (match (string-index str char-set:octal-nonzero)
    (#f (and (not (string-null? str))
             (char=? (string-ref str (1- (string-length str))) #\0)
             (string-every (string-first str) str 0 (1- (string-length str)))
             (make-padded-octal 0 width (string-first str) trailer)))
    (start (cond
            ((string-index str char-set:non-octal start) #f)
            ((zero? start) (make-padded-octal (string->number str 8)
                                              width #\0 trailer))
            ((string-every (string-first str) str 0 start)
             (make-padded-octal (string->number (substring str start) 8)
                                width (string-first str) trailer))
            (else #f)))))

(define (string->unstructured-octal str)
  (match (string-index str char-set:octal)
    (#f (make-unstructured-octal 0 str))))

(define (zero-string->octal zstr)
  "Convert the zero string ZSTR into an octal value."
  (match zstr
    (($ <zero-string> (? string? str) trailer)
     (or (string->padded-octal str trailer)
         (make-unstructured-octal (or (extract-octal str) 0) zstr)))
    (($ <zero-string> (? bytevector? bv) trailer)
     (make-unstructured-octal 0 zstr))))

(define* (decode-octal bv #:optional (start 0)
                                 (end (bytevector-length bv)))
  "Decode the contents of the bytevector BV as a formatted octal value.
Optionally, START and END indexes can be provided to decode only a
part of BV."
  (zero-string->octal (decode-zero-string bv start end)))

(define (padded-octal->zero-string octal)
  (match-let* ((($ <padded-octal> value width padding trailer) octal)
               (str (number->string value 8))
               (size (max 0 (- width (string-length str))))
               (padding-str (make-string size padding)))
    (make-zero-string (string-append padding-str str) trailer)))

(define* (encode-octal octal #:optional bv (start 0) end)
  "Encode the octal value OCTAL.  If BV is set, the result will be
written into BV.  Otherwise, the result will be written into a new
bytevector.  If you are providing a bytevector, you can also provide
START and END indexes to control where the result is written."
  (let ((zstr (match octal
                ((? padded-octal?) (padded-octal->zero-string octal))
                ((? unstructured-octal?) (unstructured-octal-source octal)))))
    (encode-zero-string zstr bv start end)))

(define -padded-octal-
  (make-record-serializer
   make-padded-octal
   `((value ,padded-octal-value #f)
     (width ,padded-octal-width #f)
     (padding ,padded-octal-padding #f)
     (trailer ,padded-octal-trailer ,-binary-string-))
   #:elide-first-field? #t))

(define -unstructured-octal-
  (make-record-serializer
   make-unstructured-octal
   `((value ,unstructured-octal-value #f)
     (source ,unstructured-octal-source ,-zero-string-))
   #:elide-first-field? #t))

(define* (octal->sexp octal #:optional defaults)
  (match octal
    ((? padded-octal?)
     (serialize -padded-octal- octal
                (and (padded-octal? defaults) defaults)))
    ((? unstructured-octal?)
     (serialize -unstructured-octal- octal #f))
    (_ (scm-error 'wrong-type-arg 'octal->sexp
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting octal): ~A")
                  (list octal) (list octal)))))

(define* (sexp->octal obj #:optional defaults)
  (if (assoc-ref obj 'source)
      (deserialize -unstructured-octal- obj #f)
      (deserialize -padded-octal- obj
                   (and (padded-octal? defaults) defaults))))

(define -octal- (make-serializer octal->sexp sexp->octal))
