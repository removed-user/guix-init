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

(define-module (disarchive kinds tar-extension)
  #:use-module (disarchive kinds binary-string)
  #:use-module (disarchive kinds tar-header) ; recursive
  #:use-module (disarchive kinds zero-string)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (gcrypt base64)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs io ports) #:select (port-eof?))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-71)
  #:export (<tar-extension>
            make-tar-extension
            tar-extension?
            tar-extension-header
            tar-extension-content
            tar-extension-padding
            -tar-extension-
            valid-pax-records?
            decode-pax-records
            encode-pax-records
            -pax-records-
            valid-gnu-path?
            decode-gnu-path
            encode-gnu-path
            -gnu-path-
            valid-gnu-linkpath?
            decode-gnu-linkpath
            encode-gnu-linkpath
            -gnu-link-path-
            typeflag-validator
            typeflag-decoder
            typeflag-encoder
            typeflag-serializer))

;;; Commentary:
;;;
;;; Certain tarball headers are "extension headers" and they may be
;;; followed by one or more records.  This module contains a record type
;;; for representing extensions as well as procedures for encoding and
;;; decoding them.
;;;
;;; Code:

(define ascii-lf 10)
(define ascii-space 32)
(define ascii-= 61)

(define-record-type <tar-extension>
  (make-tar-extension header content)
  tar-extension?
  ;; (or (? tar-header?) #f)
  (header tar-extension-header)
  ;; This outer "or" distinguishes between GNU records and pax records.
  ;; (or (((? string?) . (? zero-string?)))
  ;;     ((or ((? binary-string?) . (? binary-string?))
  ;;          (? bytevector?)) ...))
  (content tar-extension-content))


;; Validators

(define (valid-pax-records? records)
  (define (no-=-binary-string? str)
    (match str
      ((? string?) (not (string-index str #\=)))
      ((? bytevector?) (not (let loop ((k 0))
                              (and (< k (bytevector-length str))
                                   (or (= (bytevector-u8-ref str k) ascii-=)
                                       (loop (1+ k)))))))
      (_ #f)))
  (define (key? obj)
    (and (valid-binary-string? obj)
         (no-=-binary-string? obj)))
  (define (key+value? obj)
    (match obj
      (((? key?) . (? valid-binary-string?)) #t)
      (_ #f)))
  (match records
    (((or (? key+value?) (? bytevector?)) ...) #t)
    (_ #f)))

(define (make-gnu-validator name)
  (define (name? x) (and (string? x) (string=? x name)))
  (match-lambda
    ((((? name?) . (? valid-zero-string?))) #t)
    (_ #f)))

(define valid-gnu-path? (make-gnu-validator "path"))
(define valid-gnu-linkpath? (make-gnu-validator "linkpath"))


;; Extension records in pax format

(define* (get-pax-length bv #:optional (start 0)
                         (end (bytevector-length bv)))
  "Read a pax record length from BV and return two values: the index
where the length ends and the length itself.  Optionally, START and END
indexes can be provided to read from only a part of BV."
  (define (ascii-number? b)
    (and (<= 48 b) (<= b 57)))

  (define (blank? b)
    (= b 32))

  (define (decimal-list->number xs)
    (let loop ((xs xs) (k 0) (acc 0))
      (match xs
        (() acc)
        ((x . xs) (loop xs (1+ k) (+ acc (* (- x 48) (expt 10 k))))))))

  (let loop ((k start) (acc '()))
    (if (>= k end)
        (values k (decimal-list->number acc))
        (match (bytevector-u8-ref bv k)
          ((? ascii-number? b) (loop (1+ k) (cons b acc)))
          ((? blank? b) (values (1+ k) (decimal-list->number acc)))
          (_ (values start #f))))))

(define* (get-pax-key+value bv #:optional (start 0)
                            (end (bytevector-length bv)))
  "Read a pax record key-value-pair from BV.  Optionally, START and END
indexes can be provided to read from only a part of BV."
  (and (> end start)
       (= (bytevector-u8-ref bv (1- end)) ascii-lf)
       (match (bytevector-index bv ascii-= start end)
         (#f #f)
         (idx (cons (decode-binary-string bv start idx)
                    (decode-binary-string bv (1+ idx) (1- end)))))))

(define* (get-pax-record bv #:optional (start 0)
                         (end (bytevector-length bv)))
  "Read a pax record from BV and return two values: the index where the
record ends and the record itself.  Optionally, START and END indexes
can be provided to read from only a part of BV."
  (let* ((rstart length (get-pax-length bv start end))
         (rend (and length (+ start length))))
    (if (and rend (<= rstart rend end))
        (values rend
                (or (get-pax-key+value bv rstart rend)
                    (sub-bytevector bv start rend)))
        (values end (sub-bytevector bv start end)))))

(define* (decode-pax-records bv #:optional (start 0)
                             (end (bytevector-length bv)))
  "Decode the contents of the bytevector BV as a list of pax extension
records.  Optionally, START and END indexes can be provided to decode
only a part of BV."
  (let loop ((k start) (acc '()))
    (if (>= k end)
        (reverse acc)
        (let ((next-k record (get-pax-record bv k end)))
          (loop next-k (cons record acc))))))

(define (pax-record->bytevector record)
  "Convert the pax extension record"
  (define digit-count (compose inexact->exact 1+ floor log10))
  (match record
    ((key . value)
     (let* ((bkey (encode-binary-string key))
            (bvalue (encode-binary-string value))
            ;; There are three delimiters to account for.
            (n (+ 3 (bytevector-length bkey) (bytevector-length bvalue)))
            ;; We have to include the length of the length, too.
            (len (+ n (digit-count (+ n (digit-count n))))))
       (bytevector-append (string->utf8 (number->string len))
                          #vu8(32) bkey #vu8(61) bvalue #vu8(10))))
    ((? bytevector?) record)
    (_ (scm-error 'misc-error 'pax-record->bytevector
                  (string-append "Invalid pax extension record: ~A")
                  (list record) (list record)))))

(define encode-pax-records
  (case-lambda
    "Encode the pax extension records RECORDS.  If BV is set, the result
will be written into BV.  Otherwise, the result will be written into a
new bytevector.  If you are providing a bytevector, you can also provide
START and END indexes to control where the result is written."
    ((records)
     (apply bytevector-append (map pax-record->bytevector records)))
    ((records bv)
     (encode-pax-records records bv 0 (bytevector-length bv)))
    ((records bv start)
     (encode-pax-records records bv start (bytevector-length bv)))
    ((records bv start end)
     (let* ((brecords (encode-pax-records records))
            (brecords-len (bytevector-length brecords))
            (space (- end start))
            (leftover-space (- brecords-len space)))
       (bytevector-copy! brecords 0 bv start (min brecords-len space))
       (when (positive? leftover-space)
         (bytevector-fill!* bv 0 end leftover-space))))))


;; Extension records in GNU format

(define (make-gnu-decoder name)
  "Create a decoder procedure for decoding GNU extension records with
field name NAME."
  (lambda* (bv #:optional (start 0) (end (bytevector-length bv)))
    `((,name . ,(decode-zero-string bv start end)))))

(define (make-gnu-encoder name)
  "Create an encoder procedure for encoding GNU extension records with
field name NAME."
  (define (name? x) (string=? name x))
  (lambda* (records #:optional bv (start 0) end)
    (match records
      ((((? name?) . value))
       (encode-zero-string value bv start end))
      (_ (scm-error 'misc-error 'make-gnu-encoder
                    (string-append "Invalid tar extension records: ~A")
                    (list records) (list records))))))

(define decode-gnu-path (make-gnu-decoder "path"))
(define encode-gnu-path (make-gnu-encoder "path"))

(define decode-gnu-linkpath (make-gnu-decoder "linkpath"))
(define encode-gnu-linkpath (make-gnu-encoder "linkpath"))


;; Codec lookup

(define (typeflag-decoder typeflag)
  "Find a decoder for the tarball typeflag TYPEFLAG."
  (cond
   ((or (= typeflag (char->integer #\g))
        (= typeflag (char->integer #\x)))
    decode-pax-records)
   ((= typeflag (char->integer #\L))
    decode-gnu-path)
   ((= typeflag (char->integer #\K))
    decode-gnu-linkpath)))

(define (typeflag-encoder typeflag)
  "Find an encoder for the tarball typeflag TYPEFLAG."
  (cond
   ((or (= typeflag (char->integer #\g))
        (= typeflag (char->integer #\x)))
    encode-pax-records)
   ((= typeflag (char->integer #\L))
    encode-gnu-path)
   ((= typeflag (char->integer #\K))
    encode-gnu-linkpath)))


;; Serialization

(define (pax-records->sexp records)
  (map (match-lambda
         (((? binary-string? key) . (? binary-string? value))
          (cons (serialize -binary-string- key #f)
                (serialize -binary-string- value #f)))
         ((? bytevector? bv)
          (base64-encode bv)))
       records))

(define (sexp->pax-records obj)
  (map (match-lambda
         ((key . value) (cons (deserialize -binary-string- key #f)
                              (deserialize -binary-string- value #f)))
         (b64 (base64-decode b64)))
       obj))

(define -pax-records-
  (make-serializer
   (lambda (records _) (pax-records->sexp records))
   (lambda (obj _) (sexp->pax-records obj))))

(define (make-gnu-serializer name)
  (define (name? x) (string=? x name))
  (make-serializer
   (lambda (records _)
     (match records
       ((((? name? key) . (? zero-string? value)))
        `((,key . ,(serialize -zero-string- value #f))))))
   (lambda (obj _)
     (match obj
       ((((? name? key) . value))
        `((,key . ,(deserialize -zero-string- value #f))))))))

(define -gnu-path- (make-gnu-serializer "path"))
(define -gnu-linkpath- (make-gnu-serializer "linkpath"))

(define (typeflag-serializer typeflag)
  "Find a serializer for the tarball typeflag TYPEFLAG."
  (cond
   ((or (= typeflag (char->integer #\g))
        (= typeflag (char->integer #\x)))
    -pax-records-)
   ((= typeflag (char->integer #\L))
    -gnu-path-)
   ((= typeflag (char->integer #\K))
    -gnu-linkpath-)))

(define (tar-extension->sexp ext)
  (match ext
    (($ <tar-extension> header content)
     (let ((-content- (if header
                          (typeflag-serializer (tar-header-typeflag header))
                          ;; XXX: Here, we assume that no header means a
                          ;; pax global extension.  If we ever move
                          ;; beyond pax and GNU, this may be a bad idea.
                          -pax-records-)))
       `((header . ,(serialize -tar-header- header #f))
         (content . ,(serialize -content- content #f)))))
    (_ (scm-error 'wrong-type-arg 'tar-extension->sexp
                  (string-append "Wrong type argument in position 1 "
                                 "(expecting tar-extension): ~A")
                  (list ext) (list ext)))))

(define (sexp->tar-extension obj)
  (match obj
    ((('header . header-obj)
      ('content . content-obj))
     (let* ((header (and header-obj (deserialize -tar-header- header-obj #f)))
            (-content- (if header
                           (typeflag-serializer (tar-header-typeflag header))
                           ;; XXX: See comment in 'tar-extension->sexp'
                           ;; for why this is dubious.
                           -pax-records-)))
       (make-tar-extension header
                           (deserialize -content- content-obj #f))))
    (_ (scm-error 'misc-error 'sexp->tar-extension
                  (string-append "Invalid tar extension S-exp: ~A")
                  (list obj) (list obj)))))

(define -tar-extension-
  (make-serializer
   (lambda (ext _) (tar-extension->sexp ext))
   (lambda (obj _) (sexp->tar-extension obj))))
