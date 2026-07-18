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

(define-module (disarchive kinds xz)
  #:use-module (disarchive utils)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43)
  #:export ((bytevector-crc32 . bytevector-xz-crc32)
            xz-integer-length
            encode-xz-integer
            decode-xz-integer

            make-xz-stream-header
            xz-stream-header?
            xz-stream-header-check-type
            xz-stream-header-reserved
            xz-stream-header-crc32
            decode-xz-stream-header
            encode-xz-stream-header

            make-xz-stream-footer
            xz-stream-footer?
            xz-stream-footer-check-type
            xz-stream-footer-reserved
            xz-stream-footer-backward-size
            xz-stream-footer-crc32
            decode-xz-stream-footer
            encode-xz-stream-footer

            make-xz-filter-flags
            xz-filter-flags?
            xz-filter-flags-id
            xz-filter-flags-properties
            decode-xz-filter-flags
            encode-xz-filter-flags

            make-xz-block-header
            xz-block-header?
            xz-block-header-reserved
            xz-block-header-compressed-size
            xz-block-header-uncompressed-size
            xz-block-header-filters
            xz-block-header-padding
            set-xz-block-header-padding
            xz-block-header-crc32
            xz-block-header-size
            decode-xz-block-header
            encode-xz-block-header

            make-xz-index-record
            xz-index-record?
            xz-index-record-unpadded-size
            xz-index-record-uncompressed-size
            xz-index-record-block-size
            decode-xz-index-record
            encode-xz-index-record

            make-xz-index
            xz-index?
            xz-index-records
            xz-index-crc32
            xz-index-size
            decode-xz-index
            encode-xz-index

            make-xz-stream
            xz-stream?
            xz-stream-header
            xz-stream-blocks
            xz-stream-index
            xz-stream-footer
            xz-stream-size
            xz-stream-uncompressed-size

            xz-stream-fold-right
            read-xz-streams))

(define magic-header-bytes #vu8(#xfd #x37 #x7a #x58 #x5a #x00))
(define magic-footer-bytes #vu8(#x59 #x5a))

(define* (magic-bytes? ref bv #:optional (start 0))
  (let loop ((k 0) (j start))
    (if (>= k (bytevector-length ref))
        #t
        (and (= (bytevector-u8-ref ref k)
                (bytevector-u8-ref bv j))
             (loop (1+ k) (1+ j))))))

(define bytevector-crc32
  (let ((table (vector-unfold (lambda (k)
                                (fold (lambda (_ x)
                                        (if (odd? x)
                                            (logxor (ash x -1) #xedb88320)
                                            (ash x -1)))
                                      k
                                      (iota 8)))
                              256)))
    (lambda* (bv #:optional (start 0) (end (bytevector-length bv)))
      "Calculate the 32-bit CRC (Cyclic Redundancy Check) of BV.  The
optional parameters START and END may be set to calculate the check
over a specific part of BV (rather than the whole thing)."
      (define inverted
        (fold (lambda (k crc)
                (let* ((byte (bytevector-u8-ref bv k))
                       (index (logxor byte (bit-extract crc 0 8))))
                  (logxor (vector-ref table index) (ash crc -8))))
              #xffffffff
              (iota (- end start) start)))
      (bit-extract (lognot inverted) 0 32))))

(define* (bytevector-add-crc32! bv #:optional crc32
                                (target (- (bytevector-length bv) 4))
                                (start 0)
                                (end (- (bytevector-length bv) 4)))
  "Write a 32-bit CRC (Cyclic Redundancy Check) to the last four bytes
of BV.  If CRC32 is set, write that value.  Otherwise, compute the
32-bit CRC over all but the last 4 bytes of BV."
  (let* ((x (or crc32 (bytevector-crc32 bv start end))))
    (bytevector-u32-set! bv target x 'little)))

(define* (find-xz-integer-end bv #:optional (start 0)
                              (end (bytevector-length bv)))
  (let loop ((k start))
    (cond
     ((>= k end) #f)
     ((< (bytevector-u8-ref bv k) 128) (1+ k))
     (else (loop (1+ k))))))

(define* (decode-xz-integer bv #:optional (start 0)
                            (end (bytevector-length bv)))
  (let loop ((k start) (shift 0) (acc 0))
    (when (>= k end)
      (error "Invalid multibyte integer."))
    (let ((b (bytevector-u8-ref bv k)))
      (if (< b 128)
          (begin
            (when (or (and (> k start) (zero? b))
                      (not (= (1+ k) end)))
              (error "Invalid multibyte integer."))
            (logior (ash b shift) acc))
          (loop (1+ k) (+ shift 7)
                (logior (ash (bit-extract b 0 7) shift) acc))))))

(define (xz-integer-length n)
  (1+ (quotient (1- (integer-length n)) 7)))

(define (xz-integer->bytevector n)
  (define bv (make-bytevector (xz-integer-length n)))
  (let loop ((n n) (k 0))
    (cond
     ((< n 128) (bytevector-u8-set! bv k n) bv)
     (else (let ((byte (logior #x80 (bit-extract n 0 7))))
             (bytevector-u8-set! bv k byte)
             (loop (ash n -7) (1+ k)))))))

(define encode-xz-integer
  (make-thing-encoder xz-integer->bytevector))

(define-immutable-record-type <xz-stream-header>
  (make-xz-stream-header check-type reserved crc32)
  xz-stream-header?
  ;; A number from 0 to 15.
  (check-type xz-stream-header-check-type)
  ;; The "reserved" part of the stream flags.  This is a list
  ;; consisting of the reserved byte before the check type and the
  ;; reserved nibble after it.
  (reserved xz-stream-header-reserved)
  ;; A four-byte number or #f.
  (crc32 xz-stream-header-crc32))

(define* (decode-xz-stream-header bv #:optional (start 0)
                                  (end (bytevector-length bv)))
  "Decode the contents of the bytevector BV as an XZ stream header.
Optionally, START and END indexes can be provided to decode only a
part of BV."
  (unless (= (- end start) 12)
    (error "Invalid XZ stream header size."))
  (unless (magic-bytes? magic-header-bytes bv start)
    (error "Invalid XZ magic bytes."))
  (let* ((flags (bytevector-u8-ref bv (+ start 7)))
         (check-type (bit-extract flags 0 4))
         (reserved (list (bytevector-u8-ref bv (+ start 6))
                         (bit-extract flags 4 8)))
         (crc32* (bytevector-u32-ref bv (+ start 8) 'little))
         (crc32 (if (= (bytevector-crc32 bv (+ start 6) (- end 4)) crc32*)
                    #f
                    crc32*)))
    (make-xz-stream-header check-type reserved crc32)))

(define (xz-stream-header->bytevector strm-head)
  (define bv (make-bytevector 12))
  (match-let* ((($ <xz-stream-header> check-type reserved crc32) strm-head)
               ((reserved-byte reserved-nibble) reserved)
               (byte7 (logior (ash reserved-nibble 4) check-type)))
    (bytevector-copy! magic-header-bytes 0 bv 0 6)
    (bytevector-u8-set! bv 6 reserved-byte)
    (bytevector-u8-set! bv 7 byte7)
    (bytevector-add-crc32! bv crc32 8 6 8)
    bv))

(define encode-xz-stream-header
  (make-thing-encoder xz-stream-header->bytevector))

(define-immutable-record-type <xz-stream-footer>
  (make-xz-stream-footer check-type reserved backward-size crc32)
  xz-stream-footer?
  ;; A number from 0 to 15.
  (check-type xz-stream-footer-check-type)
  ;; The "reserved" part of the stream flags.  This is a list
  ;; consisting of the reserved byte before the check type and the
  ;; reserved nibble after it.
  (reserved xz-stream-footer-reserved)
  ;; A four-byte number.
  (backward-size xz-stream-footer-backward-size)
  ;; A four-byte number.
  (crc32 xz-stream-footer-crc32))

(define* (decode-xz-stream-footer bv #:optional (start 0)
                                  (end (bytevector-length bv)))
  (unless (= (- end start) 12)
    (error "Invalid XZ stream footer size."))
  (unless (magic-bytes? magic-footer-bytes bv (+ start 10))
    (error "Invalid XZ stream footer magic bytes."))
  (let* ((crc32* (bytevector-u32-ref bv start 'little))
         (crc32 (if (= (bytevector-crc32 bv (+ start 4) (- end 2)) crc32*)
                    #f
                    crc32*))
         (raw-backward-size (bytevector-u32-ref bv (+ start 4) 'little))
         (backward-size (* (1+ raw-backward-size) 4))
         (flags (bytevector-u8-ref bv (+ start 9)))
         (check-type (bit-extract flags 0 4))
         (reserved (list (bytevector-u8-ref bv (+ start 8))
                         (bit-extract flags 4 8))))
    (make-xz-stream-footer check-type reserved backward-size crc32)))

(define (xz-stream-footer->bytevector foot)
  (define bv (make-bytevector 12))
  (match-let* ((($ <xz-stream-footer> check-type reserved
                   backward-size crc32) foot)
               ((reserved-byte reserved-nibble) reserved)
               (byte9 (logior (ash reserved-nibble 4) check-type)))
    (bytevector-copy! magic-footer-bytes 0 bv 10 2)
    (bytevector-u8-set! bv 8 reserved-byte)
    (bytevector-u8-set! bv 9 byte9)
    (bytevector-u32-set! bv 4 (1- (quotient backward-size 4)) 'little)
    (bytevector-add-crc32! bv crc32 0 4 10)
    bv))

(define encode-xz-stream-footer
  (make-thing-encoder xz-stream-footer->bytevector))

(define-immutable-record-type <xz-filter-flags>
  (make-xz-filter-flags id properties)
  xz-filter-flags?
  ;; An (XZ) integer.
  (id xz-filter-flags-id)
  ;; A bytevector.
  (properties xz-filter-flags-properties))

(define (xz-filter-flags-size flags)
  (let ((id (xz-filter-flags-id flags))
        (properties (xz-filter-flags-properties flags)))
    (+ (xz-integer-length id)
       (xz-integer-length (bytevector-length properties))
       (bytevector-length properties))))

(define* (read-xz-filter-flags bv #:optional (start 0)
                               (end (bytevector-length bv)))
  (let* ((id-end (find-xz-integer-end bv start end))
         (id (decode-xz-integer bv start id-end))
         (ps-end (find-xz-integer-end bv id-end end))
         (properties-size (decode-xz-integer bv id-end ps-end)))
    (unless (>= end (+ ps-end properties-size))
      (error "Invalid XZ filter flags."))
    (make-xz-filter-flags id (sub-bytevector bv ps-end
                                             (+ ps-end properties-size)))))

(define* (decode-xz-filter-flags bv #:optional (start 0)
                                 (end (bytevector-length bv)))
  (let ((filter (read-xz-filter-flags bv start end)))
    (unless (= (- end start) (xz-filter-flags-size filter))
      (error "Invalid XZ filter flags."))
    filter))

(define (xz-filter-flags->bytevector flags)
  (let* ((size (xz-filter-flags-size flags))
         (bv (make-bytevector size))
         (id-bv (xz-integer->bytevector (xz-filter-flags-id flags)))
         (props (xz-filter-flags-properties flags))
         (len-bv (xz-integer->bytevector (bytevector-length props))))
    (bytevector-copy! id-bv 0 bv 0 (bytevector-length id-bv))
    (bytevector-copy! len-bv 0 bv
                      (bytevector-length id-bv)
                      (bytevector-length len-bv))
    (bytevector-copy! props 0 bv
                      (+ (bytevector-length id-bv)
                         (bytevector-length len-bv))
                      (bytevector-length props))
    bv))

(define encode-xz-filter-flags
  (make-thing-encoder xz-filter-flags->bytevector))

(define-immutable-record-type <xz-block-header>
  (make-xz-block-header reserved compressed-size uncompressed-size
                        filters padding crc32)
  xz-block-header?
  ;; A reserved nibble.  It should always be zero.
  (reserved xz-block-header-reserved)
  ;; An (XZ) integer.
  (compressed-size xz-block-header-compressed-size)
  ;; An (XZ) integer.
  (uncompressed-size xz-block-header-uncompressed-size)
  ;; A list of <xz-filter-flags>.
  (filters xz-block-header-filters)
  ;; The number of padding bytes.
  (padding xz-block-header-padding set-xz-block-header-padding)
  ;; A four-byte number or #f.
  (crc32 xz-block-header-crc32))

(define (xz-block-header-size bh)
  (let* ((c-size (xz-block-header-compressed-size bh))
         (u-size (xz-block-header-uncompressed-size bh))
         (padding (xz-block-header-padding bh))
         (filters (xz-block-header-filters bh)))
    (apply + 1 1 4                      ; size, flags, and crc32
           padding
           (if c-size (xz-integer-length c-size) 0)
           (if u-size (xz-integer-length u-size) 0)
           (map xz-filter-flags-size filters))))

(define* (decode-xz-block-header bv #:optional (start 0)
                                 (end (bytevector-length bv)))
  (when (< (- end start) 6)
    (error "Invalid XZ block header size."))
  (let* ((raw-size (bytevector-u8-ref bv start))
         (size (* (1+ raw-size) 4)))
    (unless (= (- end start) size)
      (error "Invalid XZ block header size."))
    (let* ((flags (bytevector-u8-ref bv (1+ start)))
           (filter-count (1+ (bit-extract flags 0 2)))
           (reserved (bit-extract flags 2 6))
           (compressed-size? (not (zero? (bit-extract flags 6 7))))
           (uncompressed-size? (not (zero? (bit-extract flags 7 8))))
           (cs-end (if compressed-size?
                       (find-xz-integer-end bv (+ start 2) end)
                       (+ start 2)))
           (compressed-size (and compressed-size?
                                 (decode-xz-integer bv (+ start 2) cs-end)))
           (us-end (if uncompressed-size?
                       (find-xz-integer-end bv cs-end end)
                       cs-end))
           (uncompressed-size (and uncompressed-size?
                                   (decode-xz-integer bv cs-end us-end)))
           (filters (let loop ((k us-end) (j 0) (acc '()))
                      (if (>= j filter-count)
                          (reverse acc)
                          (let ((flags (read-xz-filter-flags bv k end)))
                            (loop (+ k (xz-filter-flags-size flags))
                                  (1+ j)
                                  (cons flags acc))))))
           (f-end (+ us-end (reduce + 0 (map xz-filter-flags-size filters))))
           (padding (- size 4 (- f-end start)))
           (p-end (if (or (< padding 0))
                      (error "Invalid block header padding.")
                      (+ f-end padding)))
           (crc32* (bytevector-u32-ref bv p-end 'little))
           (crc32 (if (= (bytevector-crc32 bv start (- end 4)) crc32*)
                      #f
                      crc32*)))
      (unless (bytevector-zero? bv f-end p-end)
        (error "Invalid block header padding."))
      (make-xz-block-header reserved compressed-size uncompressed-size
                            filters padding crc32))))

(define (xz-block-header->bytevector bh)
  (let* ((reserved (xz-block-header-reserved bh))
         (c-size (xz-block-header-compressed-size bh))
         (u-size (xz-block-header-uncompressed-size bh))
         (filters (xz-block-header-filters bh))
         (crc32 (xz-block-header-crc32 bh))
         (size (xz-block-header-size bh))
         (bv (make-bytevector size 0))
         (raw-size (1- (quotient size 4)))
         (flags (logior (1- (length filters))
                        (ash reserved 2)
                        (ash (if c-size 1 0) 6)
                        (ash (if u-size 1 0) 7)))
         (c-size-start 2)
         (u-size-start (+ c-size-start
                          (if c-size (xz-integer-length c-size) 0)))
         (filters-start (+ u-size-start
                           (if u-size (xz-integer-length u-size) 0))))
    (bytevector-u8-set! bv 0 raw-size)
    (bytevector-u8-set! bv 1 flags)
    (when c-size
      (encode-xz-integer c-size bv c-size-start))
    (when u-size
      (encode-xz-integer u-size bv u-size-start))
    (let loop ((filters filters) (k filters-start))
      (match filters
        (() #t)
        ((filter . rest)
         (encode-xz-filter-flags filter bv k)
         (loop rest (+ k (xz-filter-flags-size filter))))))
    (bytevector-add-crc32! bv crc32)
    bv))

(define encode-xz-block-header
  (make-thing-encoder xz-block-header->bytevector))

(define-immutable-record-type <xz-index-record>
  (make-xz-index-record unpadded-size uncompressed-size)
  xz-index-record?
  ;; An (XZ) integer.
  (unpadded-size xz-index-record-unpadded-size)
  ;; An (XZ) integer.
  (uncompressed-size xz-index-record-uncompressed-size))

(define (xz-index-record-size rd)
  (+ (xz-integer-length (xz-index-record-unpadded-size rd))
     (xz-integer-length (xz-index-record-uncompressed-size rd))))

(define (xz-index-record-block-size record)
  (let ((up-size (xz-index-record-unpadded-size record)))
    (+ up-size (padding-delta up-size 4))))

(define (xz-index-records->blocks-size rds)
  "Compute the sum of the block sizes from the XZ index records RDS."
  (define block-sizes
    (map (lambda (rd)
           (let ((s (xz-index-record-unpadded-size rd)))
             (+ s (padding-delta s 4))))
         rds))
  (reduce + 0 block-sizes))

(define* (decode-xz-index-record bv #:optional (start 0)
                                 (end (bytevector-length bv)))
  (let ((middle (find-xz-integer-end bv start end)))
    (make-xz-index-record
     (decode-xz-integer bv start middle)
     (decode-xz-integer bv middle end))))

(define (xz-index-record->bytevector rd)
  (let ((up-size (xz-index-record-unpadded-size rd))
        (uc-size (xz-index-record-uncompressed-size rd)))
    (bytevector-append (encode-xz-integer up-size)
                       (encode-xz-integer uc-size))))

(define encode-xz-index-record
  (make-thing-encoder xz-index-record->bytevector))

(define-immutable-record-type <xz-index>
  (make-xz-index records crc32)
  xz-index?
  ;; A list of <xz-index-record>.
  (records xz-index-records)
  ;; A four-byte number or #f.
  (crc32 xz-index-crc32))

(define (xz-index-size idx)
  (let* ((records (xz-index-records idx))
         (base (+ 5 (xz-integer-length (length records))
                  (reduce + 0 (map xz-index-record-size records)))))
    (+ base (padding-delta base 4))))

(define (padding-delta n padding)
  (let ((r (modulo n padding)))
    (if (zero? r) 0 (- padding r))))

(define* (decode-xz-index bv #:optional (start 0)
                          (end (bytevector-length bv)))
  (unless (zero? (bytevector-u8-ref bv start))
    (error "Invalid XZ index indicator."))
  (let* ((c-end (find-xz-integer-end bv (1+ start) end))
         (count (decode-xz-integer bv (1+ start) c-end)))
    (call-with-values
        (lambda ()
          (let loop ((k c-end) (j 0) (acc '()))
            (if (>= j count)
                (values k (reverse acc))
                (let* ((ir-mid (find-xz-integer-end bv k end))
                       (ir-end (find-xz-integer-end bv ir-mid end))
                       (ir (decode-xz-index-record bv k ir-end)))
                  (loop ir-end (1+ j) (cons ir acc))))))
      (lambda (rs-end records)
        (define p-end (+ rs-end (padding-delta rs-end 4)))
        (unless (= p-end (- end 4))
          (error "Invalid XZ index size."))
        (let* ((crc32* (bytevector-u32-ref bv p-end 'little))
               (crc32 (if (= (bytevector-crc32 bv start (- end 4)) crc32*)
                          #f
                          crc32*)))
          (unless (bytevector-zero? bv rs-end p-end)
            (error "Invalid XZ index padding."))
          (make-xz-index records crc32))))))

(define (xz-index->bytevector idx)
  (let* ((records (xz-index-records idx))
         (count (length records))
         (count-size (xz-integer-length count))
         (records-size (reduce + 0 (map xz-index-record-size records)))
         (raw-size (+ 6 records-size))
         (size (+ raw-size (padding-delta raw-size 4)))
         (bv (make-bytevector size 0)))
    (encode-xz-integer count bv 1)
    (let loop ((records records) (k (1+ count-size)))
      (match records
        (() *unspecified*)
        ((rd . rest)
         (encode-xz-index-record rd bv k)
         (loop rest (+ k (xz-index-record-size rd))))))
    (bytevector-add-crc32! bv (xz-index-crc32 idx))
    bv))

(define encode-xz-index
  (make-thing-encoder xz-index->bytevector))

(define (bytevector-rfind-footer-magic-bytes bv)
  "Find the last occurance of the XZ stream footer magic bytes in BV."
  (let loop ((k (- (bytevector-length bv) 2)))
    (and (not (negative? k))
         (or (and (magic-bytes? magic-footer-bytes bv k) k)
             (loop (1- k))))))

(define (seek-back-to-xz-stream-footer port)
  "Search PORT backwards for the beginning an XZ stream footer"
  (define bv (make-bytevector 12))
  (let loop ((k (- (ftell port) 12)))
    (cond
     ((< k 0) #f)
     (else
      (seek port k SEEK_SET)
      (get-bytevector-n! port bv 0 12)
      (let ((j (bytevector-rfind-footer-magic-bytes bv)))
        (unless (bytevector-zero? bv (if j (+ j 2) 0))
          (error "Invalid XZ stream padding."))
        (if j
            (seek port (- (+ k j) 10) SEEK_SET)
            (loop (- k 11))))))))

(define-immutable-record-type <xz-stream>
  (make-xz-stream header blocks index footer)
  xz-stream?
  (header xz-stream-header)
  (blocks xz-stream-blocks)
  (index xz-stream-index)
  (footer xz-stream-footer))

(define (xz-stream-size strm)
  (let* ((index (xz-stream-index strm))
         (records (xz-index-records index))
         (index-size (xz-index-size index))
         (blocks-size (xz-index-records->blocks-size records))
         (size (+ 24 blocks-size index-size)))
    (+ size (padding-delta size 4))))

(define (xz-stream-uncompressed-size strm)
  (reduce + 0 (map xz-index-record-uncompressed-size
                   (xz-index-records (xz-stream-index strm)))))

(define (xz-stream-block-bounds strm k)
  "Return the offset and size (as two values) of the Kth block in XZ
stream STRM."
  (let loop ((records (xz-index-records (xz-stream-index strm)))
             (j 0)
             (offset 0))
    (match records
      (() (scm-error 'out-of-range 'xz-stream-block-bounds
                     "Bad XZ stream block index ~A"
                     (list k) (list k)))
      ((record . rest)
       (let ((size (xz-index-record-block-size record)))
         (if (= j k)
             (values offset size)
             (loop rest (1+ j) (+ offset size))))))))

(define (read-xz-block-headers port records)
  (let loop ((records records) (acc '()))
    (match records
      (() (reverse acc))
      ((record . rest)
       (let* ((raw-header-size (get-u8 port))
              (header-size (* (1+ raw-header-size) 4))
              (bv (make-bytevector header-size)))
         (bytevector-u8-set! bv 0 raw-header-size)
         (get-bytevector-n! port bv 1 (1- header-size))
         (let ((b-header (decode-xz-block-header bv))
               (size (xz-index-record-block-size record)))
           (seek port (- size header-size) SEEK_CUR)
           (loop rest (cons b-header acc))))))))

(define (read-xz-stream-from-footer port)
  (let* ((footer-bv (get-bytevector-n port 12))
         (footer (decode-xz-stream-footer footer-bv))
         (index-size (xz-stream-footer-backward-size footer)))
    (seek port (- 0 12 index-size) SEEK_CUR)
    (let* ((index-bv (get-bytevector-n port index-size))
           (index (decode-xz-index index-bv))
           (records (xz-index-records index))
           (blocks-size (xz-index-records->blocks-size records)))
      (seek port (- 0 index-size blocks-size 12) SEEK_CUR)
      (let* ((position (ftell port))
             (header-bv (get-bytevector-n port 12))
             (header (decode-xz-stream-header header-bv))
             (b-headers (read-xz-block-headers port records)))
        (seek port position SEEK_SET)
        (make-xz-stream header b-headers index footer)))))

(define (xz-stream-fold-right kons knil port)
  (seek port 0 SEEK_END)
  (let loop ((acc knil))
    (seek-back-to-xz-stream-footer port)
    (let ((result (kons (read-xz-stream-from-footer port) acc)))
      (if (zero? (ftell port))
          result
          (loop result)))))

(define (read-xz-streams port)
  (xz-stream-fold-right cons '() port))
