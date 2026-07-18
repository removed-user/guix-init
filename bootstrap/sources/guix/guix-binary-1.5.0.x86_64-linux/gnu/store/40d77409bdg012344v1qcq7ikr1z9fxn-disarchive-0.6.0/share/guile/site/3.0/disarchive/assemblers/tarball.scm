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

(define-module (disarchive assemblers tarball)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive binary-filenames)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive kinds binary-string)
  #:use-module (disarchive kinds octal)
  #:use-module (disarchive kinds tar-header)
  #:use-module (disarchive kinds zero-string)
  #:use-module (disarchive logging)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (<tarball>
            make-tarball
            tarball?
            tarball-name
            tarball-input
            tarball-headers
            tarball-padding
            tarball-digest

            serialize-tarball
            serialized-tarball?
            deserialize-tarball

            tarball-file?
            disassemble-tarball

            tarball-assembler
            tarball-disassembler))

;;; Commentary:
;;;
;;; This module provides procedures for taking apart and reassembling
;;; tarball files.  The idea is to store metadata that allows
;;; recreating the tarball file bit-for-bit given the original files.
;;;
;;; Code:


;; Data

(define-record-type <tarball>
  (make-tarball name input headers padding digest)
  tarball?
  (name tarball-name)        ; string
  (input tarball-input)      ; blueprint
  (headers tarball-headers)  ; list of <tar-header>
  (padding tarball-padding)  ; number or bytevector
  (digest tarball-digest))   ; <digest>

(define (tarball-inputs tarball)
  (list (tarball-input tarball)))

(define (serialize-tarball tarball)
  (match-let* ((($ <tarball> name input headers padding digest) tarball)
               (defaults (default-tar-header headers)))
    `(tarball
      (name ,name)
      (digest ,(digest->sexp digest))
      (default-header ,@(serialize -tar-header- defaults
                                   %default-default-tar-header))
      (headers ,@(map (lambda (header)
                        (serialize -tar-header- header defaults))
                      headers))
      (padding ,padding)
      (input ,(serialize-blueprint input)))))

(define (serialized-tarball? sexp)
  (match sexp
    (('tarball _ ...) #t)
    (_ #f)))

(define (deserialize-tarball sexp)
  (match sexp
    (('tarball
      ('name name)
      ('digest digest-sexp)
      ('default-header . defaults-sexp)
      ('headers . header-sexps)
      ('padding padding)
      ('input input-sexp))
     (make-tarball
      name
      (deserialize-blueprint input-sexp)
      (let ((defaults (deserialize -tar-header- defaults-sexp
                                   %default-default-tar-header)))
        (map (lambda (sexp)
               (deserialize -tar-header- sexp defaults))
             header-sexps))
      padding
      (sexp->digest digest-sexp)))
    (_ #f)))


;; Assembly

(define (regular-file/fixed? filename)
  (define %lstat/fixed
    (match filename
      ((? string?) lstat/utf8)
      ((? bytevector?) lstat/binary)
      (_ "Invalid string" filename)))
  (and=> (false-if-exception (%lstat/fixed filename))
         (lambda (st)
           (eq? (stat:type st) 'regular))))

(define* (open-input-file/fixed filename #:key binary?)
  (define %open-input-file/fixed
    (match filename
      ((? string?) open-input-file/utf8)
      ((? bytevector?) open-input-file/binary)
      (_ "Invalid string" filename)))
  (%open-input-file/fixed filename #:binary? binary?))

(define (write-data-padding data-padding size port)
  (let* ((remainder (modulo size 512))
         (len (if (zero? remainder) 0 (- 512 remainder)))
         (bv (make-bytevector len)))
    (encode-binary-string data-padding bv)
    (put-bytevector port bv)))

(define (assemble-tarball tarball workspace)
  (match-let* ((($ <tarball> name input-blueprint
                             headers padding digest) tarball)
               (input-digest (blueprint-digest input-blueprint))
               (input (digest->filename input-digest workspace))
               (output (digest->filename digest workspace)))
    (message "Assembling the tarball ~a" name)
    (call-with-output-file output
      (lambda (out)
        (for-each (lambda (header)
                    (let* ((path (tar-header-path header))
                           (size (tar-header-size header))
                           (source (string-append input "/" path))
                           (data-padding (tar-header-data-padding header)))
                      (write-tar-header out header)
                      (when (and (not (zero? size))
                                 (regular-file/fixed? source))
                        (let ((in (open-input-file/fixed source
                                                         #:binary? #t)))
                          (dump-port-all in out)
                          (close-port in))
                        (write-data-padding data-padding size out))
                      (unless (or (zero? size)
                                  (regular-file/fixed? source))
                        (message "WARNING: Ignoring irregular file: ~a"
                                 source))))
                  headers)
        (let ((zeros (make-bytevector 512 0)))
          (put-bytevector out zeros)
          (put-bytevector out zeros))
        (put-bytevector out (if (number? padding)
                                (make-bytevector padding 0)
                                padding))))))


;; Disassembly

(define (tarball-file? filename st)
  "Check if FILENAME is a tar file."
  (and (eq? (stat:type st) 'regular)
       (call-with-input-file filename
         (lambda (in)
           (define bv (get-bytevector-n in 512))
           (and (bytevector? bv)
                (= (bytevector-length bv) 512)
                (let* ((header (bytevector->tar-header bv))
                       (name (tar-header-name header))
                       (expected-chksum (tar-header-chksum header)))
                  (bytevector-copy! (make-bytevector 8 #x20) 0 bv 148 8)
                  (let ((actual-chksum
                         (let lp ((k 0) (sum 0))
                           (if (< k 512)
                               (lp (1+ k) (+ sum (bytevector-u8-ref bv k)))
                               sum))))
                    (= expected-chksum actual-chksum))))))))

(define (consumer port)
  "Return a procedure that consumes or skips the given number of bytes from
PORT."
  (if (false-if-exception (seek port 0 SEEK_CUR))
      (lambda (len)
        (seek port len SEEK_CUR))
      (lambda (len)
        (define bv (make-bytevector 8192))
        (let loop ((len len))
          (define block (min len (bytevector-length bv)))
          (unless (or (zero? block)
                      (eof-object? (get-bytevector-n! port bv 0 block)))
            (loop (- len block)))))))

(define (read-headers port)
  (define skip
    (consumer port))

  (define (read-data-padding port count)
    (let ((padding (get-bytevector-n port count)))
      (if (bytevector-zero? padding) "" (decode-binary-string padding))))

  (let loop ((result '()))
    (define header (read-tar-header port))
    (if (end-of-tarball-object? header)
        (reverse! result)
        (let* ((size (tar-header-size header))
               (padding-size (modulo (- 512 (modulo size 512)) 512)))
          (if (= (tar-header-typeflag header) (char->integer #\g))
              (loop (cons header result))
              (begin
                (skip size)
                (let ((padding (read-data-padding port padding-size)))
                  (loop (cons (set-tar-header-data-padding header padding)
                              result)))))))))

(define (read-headers-from-file filename)
  (define (read-file-padding port)
    (let ((padding (get-bytevector-all port)))
      (match padding
        ((? eof-object?) 0)
        (_ (if (bytevector-zero? padding)
               (bytevector-length padding)
               (decode-binary-string padding))))))

  (call-with-input-file filename
    (lambda (port)
      (values
       (read-headers port)
       (read-file-padding port)))))

(define* (disassemble-tarball filename #:optional
                              (algorithm (hash-algorithm sha256))
                              #:key (name (basename filename)))
  (message "Disassembling the tarball ~a" name)
  (call-with-values (lambda () (read-headers-from-file filename))
    (lambda (headers padding)
      (message "Read ~a headers" (length headers))
      (let ((input (call-with-temporary-directory
                    (lambda (directory)
                      (message "Extracting the tarball ~a" name)
                      (invoke %tar "-C" directory "-xf" filename)
                      (disassemble directory algorithm
                                   #:name (basename name ".tar"))))))
        (make-tarball
         name
         input
         headers
         padding
         (file-digest filename algorithm))))))


;; Interfaces

(define tarball-assembler
  (make-assembler tarball?
                  tarball-name
                  tarball-digest
                  (compose list tarball-input)
                  serialize-tarball
                  serialized-tarball?
                  deserialize-tarball
                  assemble-tarball))

(define tarball-disassembler
  (make-disassembler tarball-file?
                     disassemble-tarball))
