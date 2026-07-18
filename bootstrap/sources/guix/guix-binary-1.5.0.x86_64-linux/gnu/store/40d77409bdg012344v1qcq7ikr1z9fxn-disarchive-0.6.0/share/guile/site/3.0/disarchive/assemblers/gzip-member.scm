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

(define-module (disarchive assemblers gzip-member)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive formats gzip)
  #:use-module (disarchive logging)
  #:use-module (disarchive utils)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module ((rnrs io ports) #:select (call-with-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (<gzip-member>
            make-gzip-member
            gzip-member?
            gzip-member-name
            gzip-member-input
            gzip-member-header
            gzip-member-footer
            gzip-member-compressor
            gzip-member-digest

            serialize-gzip-member
            serialized-gzip-member?
            deserialize-gzip-member

            gzip-member-file?
            disassemble-gzip-member

            gzip-member-assembler
            gzip-member-disassembler))

;;; Commentary:
;;;
;;; This module provides procedures for taking apart and reassembling
;;; Gzip-compressed files.  The idea is to store a small amount of
;;; metadata that allows recreating the Gzip file bit-for-bit given
;;; the uncompressed data.
;;;
;;; Code:


;; Data

(define-immutable-record-type <gzip-member>
  (make-gzip-member name input header footer compressor digest)
  gzip-member?
  (name gzip-member-name)
  (input gzip-member-input)
  (header gzip-member-header)
  (footer gzip-member-footer)
  (compressor gzip-member-compressor set-gzip-member-compressor)
  (digest gzip-member-digest))

(define (gzip-header->sexp header)
  (match-let ((($ <gzip-header> text? reserved-flags mtime extra-flags os
                                extra-field filename comment crc) header))
    `(,@(if text? '((text? #t)) '())
      ,@(if (zero? reserved-flags) '() `((reserved-flags ,reserved-flags)))
      (mtime ,mtime)
      (extra-flags ,extra-flags)
      (os ,os)
      ,@(if extra-field `((extra-field ,(base64-encode extra-field))) '())
      ,@(if filename `((filename ,filename)) '())
      ,@(if comment `((comment ,comment)) '())
      ,@(if crc `((header-crc ,crc)) '()))))

(define (gzip-footer->sexp footer)
  (match-let ((($ <gzip-footer> crc isize) footer))
    `((crc ,crc)
      (isize ,isize))))

(define (serialize-gzip-member member)
  (match-let ((($ <gzip-member> name input header footer
                                compressor digest) member))
    `(gzip-member
      (name ,name)
      (digest ,(digest->sexp digest))
      (header ,@(gzip-header->sexp header))
      (footer ,@(gzip-footer->sexp footer))
      (compressor ,compressor)
      (input ,(serialize-blueprint input)))))

(define (assrq-ref arlist key)
  (and=> (assq-ref arlist key) car))

(define (sexp->gzip-header sexp)
  (make-gzip-header
   (eq? (assrq-ref sexp 'text?) #t)
   (or (assrq-ref sexp 'reserved-flags) 0)
   (or (assrq-ref sexp 'mtime) 0)
   (or (assrq-ref sexp 'extra-flags) 0)
   (or (assrq-ref sexp 'os) 255)
   (and=> (assrq-ref sexp 'extra-field) base64-decode)
   (assrq-ref sexp 'filename)
   (assrq-ref sexp 'comment)
   (assrq-ref sexp 'header-crc)))

(define (sexp->gzip-footer sexp)
  (make-gzip-footer
   (assrq-ref sexp 'crc)
   (assrq-ref sexp 'isize)))

(define (serialized-gzip-member? sexp)
  (match sexp
    (('gzip-member _ ...) #t)
    (_ #f)))

(define (deserialize-gzip-member sexp)
  (match sexp
    (('gzip-member
      ('name name)
      ('digest digest-sexp)
      ('header header-sexp ...)
      ('footer footer-sexp ...)
      ('compressor compressor)
      ('input input-sexp))
     (make-gzip-member
      name
      (deserialize-blueprint input-sexp)
      (sexp->gzip-header header-sexp)
      (sexp->gzip-footer footer-sexp)
      compressor
      (sexp->digest digest-sexp)))
    (_ #f)))


;; Assembly

(define* (gnu-gzip speed rsync? input)
  (let* ((args (append '("--gnu")
                       (if speed (list (format #f "-~a" speed)) '())
                       (if rsync? '("--rsyncable") '())
                       '("-c"))))
    (with-input-from-file input
      (lambda ()
        (apply open-pipe* OPEN_READ (%zgz) args)))))

(define* (pristine-gnu-gzip speed rsync input)
  (let* ((args (append '("--gnu")
                       (if speed (list (format #f "-~a" speed)) '())
                       (if rsync (list rsync) '())
                       '("-c"))))
    (with-input-from-file input
      (lambda ()
        (apply open-pipe* OPEN_READ (%zgz) args)))))

(define* (zlib-gzip speed perl-style? input)
  ;; The order of the arguments matter!  It looks like the speed has
  ;; to come after the quirk.
  (let* ((args (append (if perl-style? '("--quirk" "perl") '())
                       (if speed (list (format #f "-~a" speed)) '())
                       '("-c"))))
    (with-input-from-file input
      (lambda ()
        (apply open-pipe* OPEN_READ (%zgz) args)))))

(define %compressors
  `((gnu-best . ,(cut gnu-gzip 9 #f <>))
    (gnu-best-rsync . ,(cut gnu-gzip 9 #t <>))
    (gnu . ,(cut gnu-gzip #f #f <>))
    (gnu-rsync . ,(cut gnu-gzip #f #t <>))
    (gnu-fast . ,(cut gnu-gzip 1 #f <>))
    (gnu-fast-rsync . ,(cut gnu-gzip 1 #t <>))
    (zlib-best . ,(cut zlib-gzip 9 #f <>))
    (zlib . ,(cut zlib-gzip #f #f <>))
    (zlib-fast . ,(cut zlib-gzip 1 #f <>))
    (zlib-best-perl . ,(cut zlib-gzip 9 #t <>))
    (zlib-perl . ,(cut zlib-gzip #f #t <>))
    (zlib-fast-perl . ,(cut zlib-gzip 1 #t <>))
    (gnu-best-rsync-1.4 . ,(cut pristine-gnu-gzip 9 "--new-rsyncable" <>))
    (gnu-rsync-1.4 . ,(cut pristine-gnu-gzip #f "--new-rsyncable" <>))
    (gnu-fast-rsync-1.4 . ,(cut pristine-gnu-gzip 1 "--new-rsyncable" <>))))

(define (compressor-pipe compressor input)
  ((assq-ref %compressors compressor) input))

(define (call-with-metadataless-compressor-pipe compressor input proc)
  "Run COMPRESSOR on INPUT and call PROC with its output port."
  (let ((raw-in (compressor-pipe compressor input)))
    (dynamic-wind
      noop
      (lambda ()
        (call-with-port (strip-gzip-metadata raw-in) proc))
      (lambda ()
        (let* ((status (close-pipe raw-in))
               (exit-val (status:exit-val status))
               (term-sig (status:term-sig status)))
          (unless (or (and exit-val (zero? exit-val))
                      (and term-sig (= term-sig SIGPIPE)))
            (error "unexpected exit status" compressor)))))))

(define (assemble-gzip-member member workspace)
  (match-let* ((($ <gzip-member> name input-blueprint header footer
                                 compressor digest) member)
               (input-digest (blueprint-digest input-blueprint))
               (input (digest->filename input-digest workspace))
               (output (digest->filename digest workspace)))
    (message "Assembling the Gzip file ~a" name)
    (mkdir-p (dirname output))
    (call-with-output-file output
      (lambda (out)
        (write-gzip-header out header)
        (call-with-metadataless-compressor-pipe compressor input
          (lambda (in)
            (dump-port-all in out)))
        (write-gzip-footer out footer)))))


;; Disassemblly

(define (gzip-member-file? filename st)
  (and (eq? (stat:type st) 'regular)
       (call-with-input-file filename
         (lambda (port)
           (equal? (get-bytevector-n port 2) #vu8(#x1f #x8b))))))

#;(

 ;; This is how to extract a single Gzip member.  It is very slow
 ;; because it relies on a Scheme implementation of inflate.

 (define-crc crc-32)

 (define (inflate/crc-32 in out)
   (inflate in out crc-32-init crc-32-update crc-32-finish))

 (define* (extract-gzip-member in out #:optional
                               (algorithm (hash-algorithm sha256)))
   "Extract one Gzip member from IN and write it to OUT, returning its
metadata."
   (let* ((header (read-gzip-header in))
          (chash-port get-chash (open-hash-input-port algorithm in))
          (actual-crc size buf (inflate/crc-32 chash-port out))
          (footer (read-gzip-footer in buf)))
     (values header (make-digest algorithm (get-chash)) footer)))

)

(define (read-gzip-metadata port)
  "Extract Gzip metadata from PORT and return two values: a
Gzip-header and a Gzip-footer.  Note that PORT must be a file port
that yields a single Gzip member."
  (let ((header (read-gzip-header port)))
    (seek port -8 SEEK_END)
    (values header (read-gzip-footer port))))

(define (try-assembling-gzip-member member workspace)
  (assemble member workspace #:verify? #f)
  (let* ((digest (gzip-member-digest member))
         (out (digest->filename digest workspace))
         (actual-digest (file-digest out (digest-algorithm digest))))
    (equal? digest actual-digest)))

(define (call-with-sigpipe thunk)
  "Call THUNK with the SIGPIPE handler set to SIG_DFL, restoring the
handler afterwards."
  (let ((handler #f)
        (flags #f))
    (dynamic-wind
      (lambda ()
        (match-let (((handler* . flags*) (sigaction SIGPIPE)))
          (unless handler*
            (error "could not save SIGPIPE handler"))
          (set! handler handler*)
          (set! flags flags*))
        (sigaction SIGPIPE SIG_DFL))
      thunk
      (lambda ()
        (sigaction SIGPIPE handler flags)))))

(define (file-compressor? inflated deflated compressor)
  "Check if COMPRESSOR was used on INFLATED to create DEFLATED."
  (define (port=? port1 port2)
    (let loop ()
      (define b1 (get-u8 port1))
      (define b2 (get-u8 port2))
      (cond
       ((and (eof-object? b1) (eof-object? b2)) #t)
       ((equal? b1 b2) (loop))
       (else #f))))

  (call-with-input-file deflated
    (lambda (raw-port1)
      (call-with-port (strip-gzip-metadata raw-port1)
        (lambda (port1)
          (call-with-sigpipe
           (lambda ()
             (call-with-metadataless-compressor-pipe compressor inflated
               (cut port=? port1 <>)))))))))

(define (find-compressor inflated deflated)
  "Find the compressor used on INFLATED to create DEFLATED."
  (message "Trying up to ~a compressors" (length %compressors))
  (find (lambda (compressor)
          (start-message "  ~a... " compressor)
          (if (file-compressor? inflated deflated compressor)
              (begin (message "yes!") #t)
              (begin (message "no") #f)))
        (map car %compressors)))

(define* (disassemble-gzip-member filename #:optional
                                  (algorithm (hash-algorithm sha256))
                                  #:key (name (basename filename)))
  "Disassemble FILENAME into a Gzip-member blueprint object.  The file
at FILENAME must be a Gzip file containing a single member.  If
ALGORITHM is set, use it for computing digests."
  (message "Disassembling the Gzip file ~a" name)
  (call-with-temporary-output-file
   (lambda (tmpname tmp)
     (with-output-to-port tmp
       (lambda ()
         (message "Decompressing the Gzip file ~a" name)
         (invoke %gzip "-d" "-c" filename)))
     (close-port tmp)
     (let* ((compressor (or (find-compressor tmpname filename)
                            (error "could not find Gzip compressor")))
            (input (disassemble tmpname algorithm
                                #:name (basename name ".gz"))))
       (call-with-values (lambda () (call-with-input-file filename
                                      read-gzip-metadata))
         (lambda (header footer)
           (make-gzip-member name input header footer compressor
                             (file-digest filename algorithm))))))))


;; Interfaces

(define gzip-member-assembler
  (make-assembler gzip-member?
                  gzip-member-name
                  gzip-member-digest
                  (compose list gzip-member-input)
                  serialize-gzip-member
                  serialized-gzip-member?
                  deserialize-gzip-member
                  assemble-gzip-member))

(define gzip-member-disassembler
  (make-disassembler gzip-member-file?
                     disassemble-gzip-member))
