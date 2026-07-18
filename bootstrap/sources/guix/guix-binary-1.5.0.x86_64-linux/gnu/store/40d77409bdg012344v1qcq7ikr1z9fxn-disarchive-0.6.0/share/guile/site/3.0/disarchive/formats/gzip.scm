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

(define-module (disarchive formats gzip)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-60) #:select (list->integer))
  #:export (<gzip-header>
            make-gzip-header
            gzip-header?
            gzip-header-text?
            gzip-header-reserved-flags
            gzip-header-mtime
            gzip-header-extra-flags
            gzip-header-os
            gzip-header-extra-field
            gzip-header-filename
            gzip-header-comment
            gzip-header-crc
            read-gzip-header
            write-gzip-header

            <gzip-footer>
            make-gzip-footer
            gzip-footer?
            gzip-footer-crc
            gzip-footer-isize
            read-gzip-footer
            write-gzip-footer

            strip-gzip-metadata))

;;; Commentary:
;;;
;;; This module provides procedures reading and writing Gzip files.
;;;
;;; Code:

(define (require-bytevector-n port count error-message)
  "Read exactly COUNT bytes from PORT.  If COUNT bytes cannot be read,
raise an error with message ERROR-MESSAGE."
  (match (get-bytevector-n port count)
    ((? eof-object?) (error error-message))
    (bv (unless (= (bytevector-length bv) count)
          (error error-message))
        bv)))

(define (read-extra-field port)
  "Read a Gzip header \"extra field\" from PORT."
  (define error-message "could not read extra field in Gzip header")
  (let* ((count-bv (require-bytevector-n port 2 error-message))
         (count (bytevector-u16-ref count-bv 0 'little)))
    (require-bytevector-n port count error-message)))

(define (read-latin1z port field)
  "Read a null-terminated ISO-8859-1 string from PORT.  If the string
cannot be read, raise an error indicating that FIELD could not be
read."
  (define error-message
    (string-append "could not read " field " in Gzip header"))
  (let loop ((acc '()))
    (match (get-u8 port)
      ((? eof-object?) (error error-message))
      (0 (bytevector->string (list->u8vector (reverse acc)) "ISO-8859-1"))
      (b (loop (cons b acc))))))

(define-record-type <gzip-header>
  (make-gzip-header text? reserved-flags mtime extra-flags os
                    extra-field filename comment crc)
  gzip-header?
  (text? gzip-header-text?)
  (reserved-flags gzip-header-reserved-flags)
  (mtime gzip-header-mtime)
  (extra-flags gzip-header-extra-flags)
  (os gzip-header-os)
  (extra-field gzip-header-extra-field)
  (filename gzip-header-filename)
  (comment gzip-header-comment)
  (crc gzip-header-crc))

(define (read-gzip-header port)
  "Read a Gzip header from PORT."
  (define header-error-message "could not read Gzip header")
  (define crc-error-message "could not read CRC in Gzip header")
  (let* ((header-bv (require-bytevector-n port 10 header-error-message))
         (magic (bytevector-u16-ref header-bv 0 'big))
         (method (bytevector-u8-ref header-bv 2)))
    (unless (and (= magic #x1f8b) (= method 8))
      (error "not a Gzip header"))
    (let* ((flags (bytevector-u8-ref header-bv 3))
           (text? (logbit? 0 flags))
           (crc? (logbit? 1 flags))
           (extra-field? (logbit? 2 flags))
           (filename? (logbit? 3 flags))
           (comment? (logbit? 4 flags))
           (reserved-flags (bit-extract flags 5 8))
           (mtime (bytevector-u32-ref header-bv 4 'little))
           (extra-flags (bytevector-u8-ref header-bv 8))
           (os (bytevector-u8-ref header-bv 9))
           (extra-field (and extra-field? (read-extra-field port)))
           (filename (and filename? (read-latin1z port "filename")))
           (comment (and comment? (read-latin1z port "comment")))
           (crc-bv (and crc? (require-bytevector-n port 2 crc-error-message)))
           (crc (and crc? (bytevector-u16-ref crc-bv 0 'little))))
      (make-gzip-header text? reserved-flags mtime extra-flags os
                        extra-field filename comment crc))))

(define (write-gzip-header port header)
  "Write the Gzip header HEADER to PORT."
  (define bv (make-bytevector 4))
  (match-let ((($ <gzip-header> text? reserved-flags mtime extra-flags os
                                extra-field filename comment crc) header))
    (put-u8 port #x1f)
    (put-u8 port #x8b)
    (put-u8 port 8)
    (let ((flags (logior (ash reserved-flags 5)
                         (list->integer
                          (list comment filename extra-field crc text?)))))
      (put-u8 port flags))
    (bytevector-u32-set! bv 0 mtime 'little)
    (put-bytevector port bv)
    (put-u8 port extra-flags)
    (put-u8 port os)
    (when extra-field
      (bytevector-u16-set! bv 0 (bytevector-length extra-field) 'little)
      (put-bytevector port bv 0 2)
      (put-bytevector port extra-field))
    (when filename
      (put-bytevector port
                      (string->bytevector filename "ISO-8859-1" 'escape))
      (put-u8 port 0))
    (when comment
      (put-bytevector port
                      (string->bytevector comment "ISO-8859-1" 'escape))
      (put-u8 port 0))
    (when crc
      (bytevector-u16-set! bv 0 crc 'little)
      (put-bytevector port bv 0 2))))

(define-record-type <gzip-footer>
  (make-gzip-footer crc isize)
  gzip-footer?
  (crc gzip-footer-crc)
  (isize gzip-footer-isize))

(define* (read-gzip-footer port #:optional (preamble #vu8()))
  "Read a Gzip footer from PORT.  If the bytevector PREAMBLE is set,
read from it first."
  (let* ((count 8)
         (bv (make-bytevector count))
         (preamble-count (min 8 (bytevector-length preamble)))
         (port-count (- 8 preamble-count)))
    (bytevector-copy! preamble 0 bv 0 (min 8 (bytevector-length preamble)))
    (let ((n (get-bytevector-n! port bv preamble-count port-count)))
      (unless (equal? n port-count)
        (error "could not read Gzip footer")))
    (make-gzip-footer (bytevector-u32-ref bv 0 'little)
                      (bytevector-u32-ref bv 4 'little))))

(define (write-gzip-footer port footer)
  "Write the Gzip footer FOOTER to PORT."
  (define bv (make-bytevector 8))
  (match-let ((($ <gzip-footer> crc isize) footer))
    (bytevector-u32-set! bv 0 crc 'little)
    (bytevector-u32-set! bv 4 isize 'little)
    (put-bytevector port bv)))

(define* (port-drop-right/8 port #:optional close)
  "Wrap PORT so that it ends eight bytes sooner."
  (define buf-len (* 64 1024))
  (define buf (make-bytevector buf-len))

  (define (read! bv start count)
    (match (get-bytevector-n! port buf 8 (min (- buf-len 8) count))
      ((? eof-object?) 0)
      (n (bytevector-copy! buf 0 bv start n)
         (bytevector-copy! buf n buf 0 8)
         n)))

  (let loop ((k 0))
    (unless (>= k 8)
      (let ((b (get-u8 port)))
        (unless (eof-object? b)
          (bytevector-u8-set! buf k b)
          (loop (1+ k))))))

  (make-custom-binary-input-port "port-drop-right/8" read! #f #f close))

(define* (strip-gzip-metadata port #:optional close)
  "Return an input port that wraps PORT, but skips Gzip metadata.  The
data available from PORT must represent a single Gzip member."
  (read-gzip-header port)
  (port-drop-right/8 port close))
