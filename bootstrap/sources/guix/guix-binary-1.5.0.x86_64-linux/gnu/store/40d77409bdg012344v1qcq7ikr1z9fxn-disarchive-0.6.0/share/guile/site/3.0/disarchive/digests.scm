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

(define-module (disarchive digests)
  #:use-module (disarchive git-hash)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (<digest>
            make-digest
            digest?
            digest-algorithm
            digest-value
            digest-algorithm-name
            digest->sexp
            sexp->digest
            digest->filename
            file-digest
            file-digest?))

;;; Commentary:
;;;
;;; This module provides a representation of digests (or hashes).  A
;;; digest is a binary hash and the algorithm used to produce it.
;;;
;;; Code:

(define-record-type <digest>
  (make-digest algorithm value)
  digest?
  (algorithm digest-algorithm)
  (value digest-value))

(define digest-algorithm-name
  (compose hash-algorithm-name digest-algorithm))

(define (digest->sexp digest)
  (match-let ((($ <digest> algorithm value) digest))
    `(,(hash-algorithm-name algorithm)
      ,(bytevector->base16-string value))))

(define (sexp->digest sexp)
  (match sexp
    ((algorithm-symbol value-string)
     (let ((algorithm (lookup-hash-algorithm algorithm-symbol))
           (value (base16-string->bytevector value-string)))
       (unless algorithm
         (error "unknown digest algorithm" algorithm-symbol))
       (make-digest algorithm value)))))

(define* (digest->filename digest #:optional (base ""))
  "Convert DIGEST into a filename (using its algorithm name as the
directory name and its base16 hash as the base name).  If BASE is
set, prepend it with a delimiting slash to the resulting filename."
  (string-append (if (string-null? base)
                     ""
                     (string-append base "/"))
                 (symbol->string (digest-algorithm-name digest))
                 "/" (bytevector->base16-string (digest-value digest))))

(define* (file-digest filename #:optional
                      (algorithm (hash-algorithm sha256)))
  "Compute the digest of FILENAME using ALGORITHM.  If ALGORITHM is
unspecified, use SHA-256."
  (define hash
    (and=> (stat filename #f)
           (lambda (st)
             (case (stat:type st)
               ((regular)
                (file-hash algorithm filename))
               ((directory)
                (git-hash-directory filename algorithm))))))
  (and hash (make-digest algorithm hash)))

(define (file-digest? filename digest)
  "Check if DIGEST matches the digest of FILENAME."
  (equal? digest (file-digest filename (digest-algorithm digest))))
