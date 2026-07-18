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

(define-module (disarchive serialization)
  #:use-module (gcrypt base64)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<serializer>
            make-serializer
            make-record-serializer
            serialize
            deserialize
            serdeser))

;;; Commentary:
;;;
;;; This module provides a simple declaritive interface for make
;;; record serializers and deserializers.
;;;
;;; Code:

(define-record-type <serializer>
  (make-serializer serialize deserialize)
  serializer?
  (serialize serializer-serialize)
  (deserialize serializer-deserialize))

(define (resolve-serializer serializer)
  "If SERIALIZER is a promise, force it; otherwise, return it as-is.
This is useful for recursive data structures."
  (if (promise? serializer) (force serializer) serializer))

(define (serialize serializer obj defaults)
  "Serialize OBJ using SERIALIZER.  If any component of OBJ matches
its counterpart in DEFAULTS, it will be omitted from the result."
  (let ((serializer (resolve-serializer serializer)))
    (if (and obj serializer)
        ((serializer-serialize serializer) obj defaults)
        (list obj))))

(define (deserialize serializer sexp defaults)
  "Deserialize SEXP using SERIALIZER.  Any missing component will be
filled in from DEFAULTS."
  (let ((serializer (resolve-serializer serializer)))
    (match sexp
      ((#f) #f)
      (_ (if (and sexp serializer)
             ((serializer-deserialize serializer) sexp defaults)
             (car sexp))))))

(define* (serdeser serializer obj #:optional defaults)
  "Serialize and then deserialize OBJ using SERIALIZER with
DEFAULTS."
  (deserialize serializer (serialize serializer obj defaults) defaults))

(define* (make-record-serializer constructor specs
                                 #:key elide-first-field?)
  "Create a record serializer for a record type with constructor
CONSTRUCTOR according to SPECS, which provides a specification for
each field of the record.  A field specification is a three-element
list containing a name, accessor, and serializer.  The value of SPECS
must be a list of field specifications.  If ELIDE-FIRST-FIELD? is set,
then the first field will be serialized without a name if possible."
  (make-serializer
   (lambda (rec defaults)
     (let loop ((specs specs) (acc '()) (first? #t))
       (match specs
         (() (reverse acc))
         (((name accessor serializer) . specs-rest)
          (let ((value (accessor rec))
                (default (and defaults (accessor defaults))))
            (if (equal? value default)
                (loop specs-rest acc #f)
                (let* ((serial-value (serialize serializer value default))
                       (field (if (and elide-first-field? first?
                                       (match serial-value
                                         (((? (negate pair?))) #t)
                                         (_ #f)))
                                  (car serial-value)
                                  (cons name serial-value))))
                  (loop specs-rest (cons field acc) #f))))))))
   (lambda (sexp defaults)
     (let loop ((sexp sexp) (specs specs) (acc '()) (first? #t))
       (match specs
         (() (apply constructor (reverse acc)))
         (((name accessor serializer) . specs-rest)
          (match sexp
            ((((? (cut eq? <> name)) . serial-value) . sexp-rest)
             (let* ((default (and defaults (accessor defaults)))
                    (value (deserialize serializer serial-value default)))
               (loop sexp-rest specs-rest (cons value acc) #f)))
            ((and (? (const elide-first-field?))
                  (? (const first?))
                  ((? (negate pair?) serial-value) . sexp-rest))
             (let ((value (deserialize serializer (list serial-value)
                                       (and defaults (accessor defaults)))))
               (loop sexp-rest specs-rest (cons value acc) #f)))
            (_ (loop sexp specs-rest
                     (cons (and defaults (accessor defaults)) acc)
                     #f)))))))))
