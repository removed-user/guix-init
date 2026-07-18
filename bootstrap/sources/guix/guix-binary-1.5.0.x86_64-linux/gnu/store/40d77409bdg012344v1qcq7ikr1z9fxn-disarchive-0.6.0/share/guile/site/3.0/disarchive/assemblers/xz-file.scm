;;; Disarchive
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive assemblers xz-file)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive kinds xz)
  #:use-module (disarchive logging)
  #:use-module (disarchive utils)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (lzma)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:export (make-xz-file
            xz-file?
            xz-file-name
            xz-file-input
            xz-file-compressor
            xz-file-digest

            serialize-xz-file
            serialized-xz-file?
            deserialize-xz-file

            xz-file-file?
            disassemble-xz

            xz-file-assembler
            xz-file-disassembler))


;; Data

(define (get-serialized-value key dflt fields)
  "Lookup KEY in the serialized fields FIELDS, returning DFLT if
KEY is not found."
  (match (assq key fields)
    (#f dflt)
    ((key value) value)))

(define-immutable-record-type <xz-block>
  (make-xz-block inflated-size deflated-size
                 header-sizes? head-padding level extreme?)
  xz-block?
  (inflated-size xz-block-inflated-size)
  (deflated-size xz-block-deflated-size)
  (header-sizes? xz-block-header-sizes?)
  (head-padding xz-block-head-padding
                  set-xz-block-head-padding)
  (level xz-block-level)
  (extreme? xz-block-extreme?))

(define (xz-block-default-head-padding block)
  (let* ((inflated-size (xz-block-inflated-size block))
         (deflated-size (xz-block-deflated-size block))
         (header-sizes? (xz-block-header-sizes? block))
         (header-sizes-size (if header-sizes?
                                (+ (xz-integer-length inflated-size)
                                   (xz-integer-length deflated-size))
                                0))
         (base (+ 1 1 4 header-sizes-size 3))
         (remainder (modulo base 4)))
    (if (zero? remainder) 0 (- 4 remainder))))

(define (set-xz-block-default-head-padding block)
  (set-xz-block-head-padding block
                             (xz-block-default-head-padding block)))

(define (serialize-xz-block xzb)
  (match-let ((($ <xz-block> inflated-size deflated-size
                  header-sizes? head-padding level extreme?) xzb))
    `(block
      (inflated-size ,inflated-size)
      (deflated-size ,deflated-size)
      ,@(if header-sizes? `((header-sizes? #t)) '())
      ,@(if (= (xz-block-head-padding xzb)
               (xz-block-default-head-padding xzb))
            '()
            `((head-padding ,(xz-block-head-padding xzb))))
      ,@(if (= level 6) '() `((level ,level)))
      ,@(if extreme? `((extreme? #t)) '()))))

(define (deserialize-xz-block sexp)
  (match sexp
    (('block . fields)
     (let* ((i-size (get-serialized-value 'inflated-size #f fields))
            (d-size (get-serialized-value 'deflated-size #f fields))
            (xzb (make-xz-block
                  (or i-size (error "XZ block is missing inflated size"))
                  (or d-size (error "XZ block is missing deflated size"))
                  (get-serialized-value 'header-sizes? #f fields)
                  (get-serialized-value 'head-padding #f fields)
                  (get-serialized-value 'level 6 fields)
                  (get-serialized-value 'extreme? #f fields))))
       (if (xz-block-head-padding xzb)
           xzb
           (set-xz-block-default-head-padding xzb))))))

(define-immutable-record-type <xz-stream-blueprint>
  (make-xz-stream-blueprint check blocks)
  xz-stream-blueprint?
  (check xz-stream-blueprint-check)
  (blocks xz-stream-blueprint-blocks))

(define (serialize-xz-stream-blueprint xzsb)
  (match-let ((($ <xz-stream-blueprint> check blocks) xzsb))
    `(stream
      ,@(if (= check LZMA_CHECK_CRC64) '() `((check ,check)))
      ,@(if (null? blocks) '()
            `((blocks ,(map serialize-xz-block blocks)))))))

(define (deserialize-xz-stream-blueprint sexp)
  (match sexp
    (('stream . fields)
     (make-xz-stream-blueprint
      (get-serialized-value 'check LZMA_CHECK_CRC64 fields)
      (map deserialize-xz-block
           (get-serialized-value 'blocks '() fields))))))

(define-immutable-record-type <xz-file>
  (make-xz-file name input streams digest)
  xz-file?
  (name xz-file-name)
  (input xz-file-input)
  (streams xz-file-streams)  ; list of <xz-stream-blueprint>
  (digest xz-file-digest))

(define (serialize-xz-file xzf)
  (match-let ((($ <xz-file> name input streams digest) xzf))
    `(xz-file
      (name ,name)
      (digest ,(digest->sexp digest))
      (streams ,(map serialize-xz-stream-blueprint streams))
      (input ,(serialize-blueprint input)))))

(define (serialized-xz-file? sexp)
  (match sexp
    (('xz-file _ ...) #t)
    (_ #f)))

(define (deserialize-xz-file sexp)
  (match sexp
    (('xz-file
      ('name name)
      ('digest digest-sexp)
      ('streams streams)
      ('input input-sexp))
     (make-xz-file
      name
      (deserialize-blueprint input-sexp)
      (map deserialize-xz-stream-blueprint streams)
      (sexp->digest digest-sexp)))
    (_ #f)))


;; Helpers

;; This is lifted from the XZ source code.
(define %dictionary-size-levels
  (map (lambda (x k) (cons (expt 2 x) k))
       '(18 20 21 22 22 23 23 24 25 26)
       (iota 10)))

(define (check-size check)
  (cond
   ((= check LZMA_CHECK_NONE) 0)
   ((= check LZMA_CHECK_CRC32) 4)
   ((= check LZMA_CHECK_CRC64) 8)
   ((= check LZMA_CHECK_SHA256) 32)
   (else (error "Unknown XZ check type" check))))

(define (call-with-truncated-port port count proc)
  (define remaining count)
  (define (read! bv start count)
    (let ((n (min remaining count)))
      (match (get-bytevector-n! port bv start n)
        ((? eof-object?) 0)
        (m (begin (set! remaining (- remaining m)) m)))))
  (call-with-port
      (make-custom-binary-input-port "truncated" read! #f #f
                                     (lambda () (close-port port)))
    proc))

(define (call-with-input-file-part filename offset size proc)
  (call-with-input-file filename
    (lambda (port)
      (seek port offset SEEK_SET)
      (call-with-truncated-port port size proc))))

(define (call-with-xz-input-block filename i-offset i-size
                                  level extreme? check proc)
  (define (skip-xz-block-header port)
    (define size (* (1+ (get-u8 port)) 4))
    (get-bytevector-n port (1- size))
    size)

  (call-with-input-file-part filename i-offset i-size
    (lambda (raw-in)
      (call-with-xz-input-port/compressed raw-in
        (lambda (xz-in)
          (get-bytevector-n xz-in 12)
          (skip-xz-block-header xz-in)
          (proc xz-in))
        #:level level
        #:extreme? extreme?
        #:check check))))


;; Assembly

(define (level->xz-filter-flags level)
  "Convert LEVEL to a list XZ filter flags."
  (define (encode-dictionary-size size)
    (if (= size (1- (expt 2 32)))
        40
        (let* ((exponent (integer-length size))
               (base (* (- exponent 13) 2)))
          (match (logcount size)
            (1 base)
            (2 (1+ base))
            (_ (error "Invalid dictionary size"))))))

  (let* ((size (any (match-lambda ((s . l) (and (= level l) s)))
                    %dictionary-size-levels))
         (props (make-bytevector 1 (encode-dictionary-size size))))
    (list (make-xz-filter-flags #x21 props))))

(define (xz-block-xz-block-header xzb)
  (let* ((reserved 0)
         (d-size (and (xz-block-header-sizes? xzb)
                      (xz-block-deflated-size xzb)))
         (i-size (and (xz-block-header-sizes? xzb)
                      (xz-block-inflated-size xzb)))
         (flags (level->xz-filter-flags (xz-block-level xzb)))
         (padding (xz-block-head-padding xzb)))
    (make-xz-block-header reserved d-size i-size
                          flags padding #f)))

(define (write-xz-block-header xzb port)
  (let ((bh (xz-block-xz-block-header xzb)))
    (put-bytevector port (encode-xz-block-header bh))))

(define (assemble-xz-block xzb check inflated offset port)
  (let* ((i-size (xz-block-inflated-size xzb))
         (d-size (xz-block-deflated-size xzb))
         (level (xz-block-level xzb))
         (extreme? (xz-block-extreme? xzb))
         (remainder (modulo d-size 4))
         (padding (if (zero? remainder) 0 (- 4 remainder))))
    (write-xz-block-header xzb port)
    (call-with-xz-input-block inflated offset i-size level extreme? check
      (lambda (in)
        (dump-port-n in port (+ d-size padding (check-size check)))))))

(define (xz-block->xz-index-record xzb check)
  (let* ((d-size (xz-block-deflated-size xzb))
         (bh (xz-block-xz-block-header xzb))
         (header-size (xz-block-header-size bh)))
    (make-xz-index-record (+ header-size d-size (check-size check))
                          (xz-block-inflated-size xzb))))

(define (assemble-xz-stream xzsb inflated offset port)
  (define check (xz-stream-blueprint-check xzsb))
  (define xzbs (xz-stream-blueprint-blocks xzsb))
  (let ((head (make-xz-stream-header check '(0 0) #f)))
    (put-bytevector port (encode-xz-stream-header head)))
  (let loop ((xzbs xzbs) (offset offset))
    (match xzbs
      (() *unspecified*)
      ((xzb . rest)
       (assemble-xz-block xzb check inflated offset port)
       (loop rest (+ offset (xz-block-inflated-size xzb))))))
  (let* ((idx (make-xz-index (map (lambda (xzb)
                                    (xz-block->xz-index-record xzb check))
                                  xzbs)
                             #f))
         (foot (make-xz-stream-footer check '(0 0) (xz-index-size idx) #f)))
    (put-bytevector port (encode-xz-index idx))
    (put-bytevector port (encode-xz-stream-footer foot))))

(define (assemble-xz-streams streams inflated port)
  (define (stream-size stream)
    (reduce + 0 (map xz-block-inflated-size
                     (xz-stream-blueprint-blocks stream))))

  (let loop ((streams streams) (offset 0))
    (match streams
      (() *unspecified*)
      ((stream . rest)
       (assemble-xz-stream stream inflated offset port)
       (loop rest (+ offset (stream-size stream)))))))

(define (assemble-xz-file xzf workspace)
  (match-let* ((($ <xz-file> name input-blueprint streams digest) xzf)
               (input-digest (blueprint-digest input-blueprint))
               (input (digest->filename input-digest workspace))
               (output (digest->filename digest workspace)))
    (message "Assembling the XZ file ~a" name)
    (mkdir-p (dirname output))
    (call-with-output-file output
      (lambda (out)
        (assemble-xz-streams streams input out)))))


;; Disassemblly

(define (xz-file-file? filename st)
  (and (eq? (stat:type st) 'regular)
       (call-with-input-file filename
         (lambda (port)
           (equal? (get-bytevector-n port 6)
                   #vu8(#xfd #x37 #x7a #x58 #x5a #x00))))))

(define (xz-filters->levels filters)
  "Find a list of candidate compression levels based on the XZ filter
flags FILTERS."
  (define (decode-dictionary-size bits)
    (if (= bits 40)
        (1- (expt 2 32))
        (ash (logior 2 (logand bits 1))
             (+ (quotient bits 2) 11))))

  (let ((filter (last filters)))
    (if (= (xz-filter-flags-id filter) #x21)
        (let* ((props (xz-filter-flags-properties filter))
               (rawds (bit-extract (bytevector-u8-ref props 0) 0 6))
               (ds (decode-dictionary-size rawds)))
          (filter-map (match-lambda
                        ((size . level) (and (= ds size) level)))
                      %dictionary-size-levels))
        '())))

(define (disassemble-block block d-offset d-size i-offset i-size
                           deflated inflated)
  "Disassable the XZ block header (<xz-block-header>) BLOCK into an XZ
block (<xz-block>).  The block must start at D-OFFSET in the file
named DEFLATED, and be D-SIZE bytes long.  It also must be the
compressed counterpart to the I-SIZE bytes starting at I-OFFSET in the
file named INFLATED."
  (define* (port=? port1 port2 #:optional count)
    (let loop ((k 0))
      (define b1 (get-u8 port1))
      (define b2 (get-u8 port2))
      (cond
       ((or (and count (>= k count))
            (and (eof-object? b1) (eof-object? b2))) #t)
       ((equal? b1 b2) (loop (1+ k)))
       (else #f))))

  (define (block-compressor? level extreme?)
    (call-with-input-file deflated
      (lambda (in1)
        (seek in1 d-offset SEEK_SET)
        (seek in1 (xz-block-header-size block) SEEK_CUR)
        ;; Note that the CHECK argument doesn't matter since we don't
        ;; compare the checksums.
        (call-with-xz-input-block
         inflated i-offset i-size level extreme? LZMA_CHECK_CRC64
         (lambda (in2)
           (port=? in1 in2 d-size))))))

  (define header-sizes? (and (xz-block-header-compressed-size block)
                             (xz-block-header-uncompressed-size block)
                             #t))

  (message "Disassembling XZ block at ~d (~d bytes)" d-offset d-size)
  (message "In the inflated file, this is ~d and ~d" i-offset i-size)
  (let ((levels (xz-filters->levels (xz-block-header-filters block))))
    (message "Trying up to ~a compressors" (* (length levels) 2))
    (or (any (match-lambda
               ((level . extreme?)
                (start-message "  Level ~a~a... " level
                               (if extreme? " extreme!" ""))
                (if (block-compressor? level extreme?)
                    (begin (message "yes!")
                           (make-xz-block i-size d-size header-sizes?
                                          (xz-block-header-padding block)
                                          level extreme?))
                    (begin (message "no") #f))))
             (append-map (lambda (x) `((,x . #f) (,x . #t))) levels))
        (error "Could not find XZ compressor"))))

(define (disassemble-stream strm d-offset i-offset deflated inflated)
  "Disassemble the XZ stream STRM into a list of XZ
blocks (<xz-block>).  The stream must start at D-OFFSET in the file
named DEFLATED, and must be the compressed counterpart to the bytes
starting at I-OFFSET in the file named INFLATED."
  (define check (xz-stream-header-check-type (xz-stream-header strm)))
  (let loop ((blocks (xz-stream-blocks strm))
             (records (xz-index-records (xz-stream-index strm)))
             (d-offset (+ d-offset 12)) ; skip the stream header
             (i-offset i-offset)
             (acc '()))
    (match blocks
      (() (match records
            (() (make-xz-stream-blueprint check (reverse acc)))
            (_ (error "more XZ index records than blocks"))))
      ((block . blocks-rest)
       (match records
         (() (error "more XZ blocks than index records"))
         ((record . records-rest)
          (let ((d-size (- (xz-index-record-unpadded-size record)
                           (xz-block-header-size block) (check-size check)))
                (d-size* (xz-index-record-block-size record))
                (i-size (xz-index-record-uncompressed-size record)))
            (loop blocks-rest records-rest
                  (+ d-offset d-size*) (+ i-offset i-size)
                  (cons (disassemble-block block d-offset d-size
                                           i-offset i-size
                                           deflated inflated)
                        acc)))))))))

(define (disassemble-streams deflated inflated)
  "Disassemble the file named DEFLATED into a list of lists of XZ
blocks (<xz-block>).  The file named INFLATED must be uncompressed
counterpart of DEFLATED."
  (let loop ((streams (call-with-input-file deflated read-xz-streams))
             (d-offset 0)
             (i-offset 0)
             (acc '()))
    (match streams
      (() (reverse acc))
      ((strm . rest)
       (message "Disassembling XZ stream at ~d" d-offset)
       (loop rest
             (+ d-offset (xz-stream-size strm))
             (+ i-offset (xz-stream-uncompressed-size strm))
             (cons (disassemble-stream strm d-offset i-offset
                                       deflated inflated)
                   acc))))))

(define* (disassemble-xz-file filename #:optional
                              (algorithm (hash-algorithm sha256))
                              #:key (name (basename filename)))
  "Disassemble FILENAME into a XZ file blueprint object.  If ALGORITHM
is set, use it for computing digests."
  (message "Disassembling the XZ file ~a" name)
  (call-with-temporary-output-file
    (lambda (tmpname tmp)
      (with-output-to-port tmp
        (lambda ()
          (message "Decompressing the XZ file ~a" name)
          (invoke %xz "-d" "-c" filename)))
      (close-port tmp)
      (let* ((streams (disassemble-streams filename tmpname))
             (input (disassemble tmpname algorithm
                                 #:name (basename name ".xz"))))
        (make-xz-file name input streams
                      (file-digest filename algorithm))))))


;; Interfaces

(define xz-file-assembler
  (make-assembler xz-file?
                  xz-file-name
                  xz-file-digest
                  (compose list xz-file-input)
                  serialize-xz-file
                  serialized-xz-file?
                  deserialize-xz-file
                  assemble-xz-file))

(define xz-file-disassembler
  (make-disassembler xz-file-file?
                     disassemble-xz-file))

