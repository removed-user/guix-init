;;; Disarchive
;;; Copyright © 2023, 2024 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive kinds bzip2)
  #:use-module (disarchive utils)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-bzip2-stream-header
            bzip2-stream-header?
            bzip2-stream-header-level
            bzip2-stream-header-level-value
            encode-bzip2-stream-header
            decode-bzip2-stream-header))

;; Because we only work with bzip2 version, we treat the version as a
;; magic byte.
(define magic-stream-header-bytes #vu8(#x42 #x5a #x68))

(define* (magic-bytes? ref bv #:optional (start 0))
  (let loop ((k 0) (j start))
    (if (>= k (bytevector-length ref))
        #t
        (and (= (bytevector-u8-ref ref k)
                (bytevector-u8-ref bv j))
             (loop (1+ k) (1+ j))))))

(define-immutable-record-type <bzip2-stream-header>
  (make-bzip2-stream-header level)
  bzip2-stream-header?
  ;; A byte.  This should be an ASCII character between '1' and '9'.
  (level bzip2-stream-header-level))

(define (bzip2-stream-header-level-value header)
  (let ((v (- (bzip2-stream-header-level header) #x30)))
    (and (<= 1 v 9) v)))

(define (bzip2-stream-header->bytevector strm-head)
  (define bv (make-bytevector 4))
  (match-let* ((($ <bzip2-stream-header> level) strm-head))
    (bytevector-copy! magic-stream-header-bytes 0 bv 0 3)
    (bytevector-u8-set! bv 3 level)
    bv))

(define encode-bzip2-stream-header
  (make-thing-encoder bzip2-stream-header->bytevector))

(define* (decode-bzip2-stream-header bv #:optional (start 0)
                                     (end (bytevector-length bv)))
  "Decode the contents of the bytevector BV as a bzip2 stream header.
Optionally, START and END indexes can be provided to decode only a part
of BV."
  (unless (= (- end start) 4)
    (error "Invalid bzip2 stream header size."))
  (unless (magic-bytes? magic-stream-header-bytes bv start)
    (error "Invalid bzip2 magic bytes."))
  (let ((level (bytevector-u8-ref bv 3)))
    (make-bzip2-stream-header level)))
