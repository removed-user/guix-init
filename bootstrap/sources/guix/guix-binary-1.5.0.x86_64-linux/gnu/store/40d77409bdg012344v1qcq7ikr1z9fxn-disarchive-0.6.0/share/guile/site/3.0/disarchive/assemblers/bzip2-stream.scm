;;; Disarchive
;;; Copyright © 2023 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive assemblers bzip2-stream)
  #:use-module (bzip2)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive kinds bzip2)
  #:use-module (disarchive logging)
  #:use-module (disarchive utils)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-bzip2-stream
            bzip2-stream?
            bzip2-stream-name
            bzip2-stream-input
            bzip2-stream-level
            bzip2-stream-digest

            serialize-bzip2-stream
            serialized-bzip2-stream?
            deserialize-bzip2-stream

            bzip2-stream-file?
            disassemble-bzip2-stream

            bzip2-stream-assembler
            bzip2-stream-disassembler))

;;; Commentary:
;;;
;;; This module provides procedures for taking apart and reassembling
;;; bzip2-compressed files.  The idea is to store a small amount of
;;; metadata that allows recreating the bzip2 file bit-for-bit given the
;;; uncompressed data.
;;;
;;; Currently, only bzip2 files with a single stream are supported.
;;; This means that bzip2 files compressed with parallel compressors
;;; (e.g., pbzip2) will fail.  In order to disassemble an entire file,
;;; we would need to decode the Huffman-coded data to determine the
;;; stream boundaries.
;;;
;;; Code:


;; Data

(define-immutable-record-type <bzip2-stream>
  (make-bzip2-stream name input level digest)
  bzip2-stream?
  (name bzip2-stream-name)
  (input bzip2-stream-input)
  (level bzip2-stream-level)
  (digest bzip2-stream-digest))

(define (serialize-bzip2-stream member)
  (match-let ((($ <bzip2-stream> name input level digest) member))
    `(bzip2-stream
      (name ,name)
      (digest ,(digest->sexp digest))
      ,@(if (= level 9) '() `((level ,level)))
      (input ,(serialize-blueprint input)))))

(define (serialized-bzip2-stream? sexp)
  (match sexp
    (('bzip2-stream _ ...) #t)
    (_ #f)))

(define (assrq-ref arlist key)
  (and=> (assq-ref arlist key) car))

(define (deserialize-bzip2-stream sexp)
  (match sexp
    (('bzip2-stream . fields)
     (and-let* ((name (assrq-ref fields 'name))
                (input-sexp (assrq-ref fields 'input))
                (level (or (assrq-ref fields 'level) 9))
                (digest-sexp (assrq-ref fields 'digest)))
       (make-bzip2-stream
        name
        (deserialize-blueprint input-sexp)
        level
        (sexp->digest digest-sexp))))
    (_ #f)))


;; Assembly

(define (assemble-bzip2-stream member workspace)
  (match-let* ((($ <bzip2-stream> name input-blueprint level digest) member)
               (input-digest (blueprint-digest input-blueprint))
               (input (digest->filename input-digest workspace))
               (output (digest->filename digest workspace)))
    (message "Assembling the bzip2 stream ~a" name)
    (mkdir-p (dirname output))
    (call-with-output-file output
      (lambda (out)
        (call-with-bzip2-input-port/compressed
            (open-input-file input #:binary #t)
          (lambda (in)
            (dump-port-all in out))
          #:level level)))))


;; Disassembly

(define (bzip2-stream-file? filename st)
  (and (eq? (stat:type st) 'regular)
       (call-with-input-file filename
         (lambda (port)
           (equal? (get-bytevector-n port 3) #vu8(#x42 #x5a #x68))))))

(define (read-bzip2-level port)
  (let* ((bv (get-bytevector-n port 4))
         (header (decode-bzip2-stream-header bv)))
    (bzip2-stream-header-level-value header)))

(define* (disassemble-bzip2-stream filename #:optional
                                   (algorithm (hash-algorithm sha256))
                                   #:key (name (basename filename)))
  "Disassemble FILENAME into a bzip2-stream blueprint object.  The file
at FILENAME must be a bzip2 file containing a single stream.  If
ALGORITHM is set, use it for computing digests."
  (message "Disassembling the bzip2 stream ~a" name)
  (call-with-temporary-output-file
   (lambda (tmpname tmp)
     (with-output-to-port tmp
       (lambda ()
         (message "Decompressing the bzip2 file ~a" name)
         (invoke %bzip2 "-d" "-c" filename)))
     (close-port tmp)
     (let* ((level (call-with-input-file filename read-bzip2-level))
            (input (disassemble tmpname algorithm
                                #:name (basename name ".bz2"))))
       (make-bzip2-stream name input level
                          (file-digest filename algorithm))))))


;; Interfaces

(define bzip2-stream-assembler
  (make-assembler bzip2-stream?
                  bzip2-stream-name
                  bzip2-stream-digest
                  (compose list bzip2-stream-input)
                  serialize-bzip2-stream
                  serialized-bzip2-stream?
                  deserialize-bzip2-stream
                  assemble-bzip2-stream))

(define bzip2-stream-disassembler
  (make-disassembler bzip2-stream-file?
                     disassemble-bzip2-stream))
