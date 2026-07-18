;;; Guile-LZMA
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Guile-LZMA.
;;;
;;; Guile-LZMA is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-LZMA is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-LZMA.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lzma)
  #:use-module (lzma config)
  #:use-module (lzma stream)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (%default-compression-level
            make-xz-output-port
            make-xz-input-port
            make-xz-input-port/compressed
            call-with-xz-output-port
            call-with-xz-input-port
            call-with-xz-input-port/compressed
            lzma-error?
            lzma-error-symbol
            lzma-error-code
            LZMA_CHECK_NONE
            LZMA_CHECK_CRC32
            LZMA_CHECK_CRC64
            LZMA_CHECK_SHA256))


;; Low-level FFI stuff.

(define %lzma
  (delay (dynamic-link %lzma-library-path)))

;; Nicer syntax for the 'dynamic-func' and 'pointer->procedure' dance.
(define-syntax bind-lzma-procedure
  (syntax-rules (->)
    ((_ (scheme-name c-name) args -> result body ...)
     (let* ((proc-ptr (dynamic-func c-name (force %lzma)))
            (scheme-name (pointer->procedure result proc-ptr args)))
       body ...))
    ((_ name args -> result body ...)
     (bind-lzma-procedure (name (string-map (lambda (c)
                                              (if (char=? c #\-) #\_ c))
                                            (symbol->string 'name)))
         args -> result
       body ...))))

;; Defined constants.
(define LZMA_PRESET_EXTREME (ash 1 31))
(define LZMA_CONCATENATED 8)

;; The lzma_check enum.
(define LZMA_CHECK_NONE 0)
(define LZMA_CHECK_CRC32 1)
(define LZMA_CHECK_CRC64 4)
(define LZMA_CHECK_SHA256 10)

;; The lzma_action enum.
(define LZMA_RUN 0)
(define LZMA_FINISH 3)

;; The lzma_ret enum.
(define LZMA_OK 0)
(define LZMA_STREAM_END 1)

;; The rest of the lzma_ret enum is for error conditions.  It is
;; defined here with associated messages.
(define %lzma-errors
  '((2 LZMA_NO_CHECK "Input stream has no integrity check")
    (3 LZMA_UNSUPPORTED_CHECK "Cannot calculate the integrity check")
    (4 LZMA_GET_CHECK "Integrity check type is now available")
    (5 LZMA_MEM_ERROR "Cannot allocate memory")
    (6 LZMA_MEMLIMIT_ERROR "Memory usage limit was reached")
    (7 LZMA_FORMAT_ERROR "File format not recognized")
    (8 LZMA_OPTIONS_ERROR "Invalid or unsupported options")
    (9 LZMA_DATA_ERROR "Data is corrupt")
    (10 LZMA_BUF_ERROR "No progress is possible")
    (11 LZMA_PROG_ERROR "Programming error")))

(define %lzma-end
  (delay (dynamic-func "lzma_end" (force %lzma))))


;; Mid-level helpers.

;; The liblzma API requires that we clean up streams using the
;; 'lzma_end' C function.  We do this using pointer finalizers as
;; usual.  However, we also need to modify the streams, so we keep a
;; reference to both the pointer and the bytevector it points to.  The
;; reference to the pointer prevents the finalizer from running until
;; we are done, and the bytevector lets us access and modify the
;; fields of the stream.
(define-record-type <lzma-stream>
  (%make-lzma-stream pointer bv)
  lzma-stream?
  (pointer lzma-stream-pointer)
  (bv lzma-stream-bv))

(define (make-lzma-stream)
  "Return a new LZMA stream initialized with default values."
  (define bv
    (make-lzma-stream-bv %null-pointer 0 0
                         %null-pointer 0 0
                         %null-pointer
                         %null-pointer
                         %null-pointer %null-pointer
                         %null-pointer %null-pointer
                         0 0 0 0
                         0 0))
  (define ptr (bytevector->pointer bv))
  (set-pointer-finalizer! ptr (force %lzma-end))
  (%make-lzma-stream ptr bv))

(define* (set-lzma-stream-next-in! strm bv #:optional (offset 0))
  (set-lzma-stream-bv-next-in!
   (lzma-stream-bv strm)
   (pointer-address (bytevector->pointer bv offset))))

(define* (set-lzma-stream-next-out! strm bv #:optional (offset 0))
  (set-lzma-stream-bv-next-out!
   (lzma-stream-bv strm)
   (pointer-address (bytevector->pointer bv offset))))

(define (lzma-stream-avail-in strm)
  (lzma-stream-bv-avail-in (lzma-stream-bv strm)))

(define (set-lzma-stream-avail-in! strm n)
  (set-lzma-stream-bv-avail-in! (lzma-stream-bv strm) n))

(define (lzma-stream-avail-out strm)
  (lzma-stream-bv-avail-out (lzma-stream-bv strm)))

(define (set-lzma-stream-avail-out! strm n)
  (set-lzma-stream-bv-avail-out! (lzma-stream-bv strm) n))

(define-exception-type &lzma-error &error
  make-lzma-error
  lzma-error?
  (symbol lzma-error-symbol)
  (code lzma-error-code))

(define-syntax-rule (lzma-error code)
  (match (assoc code %lzma-errors)
    ((code symbol msg)
     (raise-exception (make-exception (make-lzma-error symbol code)
                                      (make-exception-with-message msg))))
    (_
     (raise-exception (make-exception (make-lzma-error #f code)
                                      (make-exception-with-message
                                       "Unknown LZMA error"))))))

(define (lzma-ok? code)
  (= code LZMA_OK))

(define (lzma-stream-end? code)
  (= code LZMA_STREAM_END))

(define lzma-code
  (bind-lzma-procedure lzma-code
      `(* ,int) -> int
    (lambda (strm action)
      (lzma-code (lzma-stream-pointer strm) action))))

(define make-lzma-encoder
  (bind-lzma-procedure lzma-easy-encoder
      `(* ,uint32 ,int) -> int
    (lambda (preset check)
      (let ((strm (make-lzma-stream)))
        (match (lzma-easy-encoder (lzma-stream-pointer strm) preset check)
          ((? lzma-ok?) strm)
          (code (lzma-error code)))))))

(define make-lzma-decoder
  (bind-lzma-procedure lzma-stream-decoder
      `(* ,uint64 ,uint32) -> int
    (lambda ()
      (let ((strm (make-lzma-stream))
            (UINT64_MAX (- (expt 2 64) 1)))
        ;; The second argument is 'memlimit', which we set to
        ;; UINT64_MAX.  According to the docs, this "effectively
        ;; disable[s] the limiter."
        (match (lzma-stream-decoder (lzma-stream-pointer strm)
                                    UINT64_MAX LZMA_CONCATENATED)
          ((? lzma-ok?) strm)
          (code (lzma-error code)))))))


;; High-level interface.

(define %default-compression-level 6)

(define* (make-xz-output-port port
                              #:key
                              (level %default-compression-level)
                              extreme?
                              (check LZMA_CHECK_CRC64)
                              (close? #t))
  "Return a new port that wraps PORT, compressing everything written
to it using the LZMA2 compression algorithm and the XZ container
format.  Different compression levels (from 0 to 9) can be set via
LEVEL.  If EXTREME? is set, take more time (but only marginally more
memory) to get a slightly better compression ratio.  The value of
CHECK controls which checksum (if any) is used for the compressed
data.  If CLOSE?  is set (the default), close PORT when the wrapper
port is closed."
  (define stream
    (let ((preset (logior level (if extreme? LZMA_PRESET_EXTREME 0))))
      (make-lzma-encoder preset check)))

  (define buffer (make-bytevector 4096))

  (define (reset-buffer!)
    (set-lzma-stream-next-out! stream buffer)
    (set-lzma-stream-avail-out! stream (bytevector-length buffer)))

  (define (flush-output!)
    (put-bytevector port buffer 0
                    (- (bytevector-length buffer)
                       (lzma-stream-avail-out stream)))
    (reset-buffer!))

  (define (finish!)
    (let loop ()
      (define state (lzma-code stream LZMA_FINISH))
      (flush-output!)
      (cond
       ((lzma-stream-end? state) 0)
       ((lzma-ok? state) (loop))
       (else (lzma-error state)))))

  (define (write! bv start count)
    (set-lzma-stream-next-in! stream bv start)
    (set-lzma-stream-avail-in! stream count)
    (match (lzma-code stream LZMA_RUN)
      ((? lzma-ok?)
       (flush-output!)
       (- count (lzma-stream-avail-in stream)))
      (code (lzma-error code))))

  (define (close)
    (finish!)
    (when close?
      (close-port port)))

  (reset-buffer!)
  (make-custom-binary-output-port "xz-output" write! #f #f close))

(define (%make-xz-input-port port stream name close?)
  "Return a new port that wraps PORT, processing it with the LZMA codec
decribed by the <lzma-stream> STREAM.  The port will be named NAME.  If
CLOSE? is set, close PORT when the wrapper port is closed."
  (define buffer (make-bytevector 4096))

  (define (read! bv start count)
    (set-lzma-stream-next-out! stream bv start)
    (set-lzma-stream-avail-out! stream count)
    (cond
     ((positive? (lzma-stream-avail-in stream))
      (match (lzma-code stream LZMA_RUN)
        ((? lzma-ok?)
         (match (- count (lzma-stream-avail-out stream))
           (0 (read! bv start count))
           (n n)))
        (code (lzma-error code))))
     (else
      (match (get-bytevector-n! port buffer 0 (bytevector-length buffer))
        ((? eof-object?)
         (match (lzma-code stream LZMA_FINISH)
           ((? lzma-stream-end?) (- count (lzma-stream-avail-out stream)))
           ((? lzma-ok?) (match (- count (lzma-stream-avail-out stream))
                           (0 (read! bv start count))
                           (n n)))
           (code (lzma-error code))))
        (buffer-count
         (set-lzma-stream-next-in! stream buffer)
         (set-lzma-stream-avail-in! stream buffer-count)
         (read! bv start count))))))

  (define (close)
    (when close?
      (close-port port)))

  (make-custom-binary-input-port name read! #f #f close))

(define* (make-xz-input-port port
                             #:key
                             (close? #t))
  "Return a new port that wraps PORT, decompressing everything read
from it using the LZMA2 algorithm and the XZ container.  If CLOSE? is
set (the default), close PORT when the wrapper port is closed."
  (let ((stream (make-lzma-decoder)))
    (%make-xz-input-port port stream "xz-input" close?)))

(define* (make-xz-input-port/compressed port
                                        #:key
                                        (level %default-compression-level)
                                        extreme?
                                        (check LZMA_CHECK_CRC64)
                                        (close? #t))
  "Return a new port that wraps PORT, compressing everything read from
it using the LZMA2 algorithm and the XZ container.  Different
compression levels (from 0 to 9) can be set via LEVEL.  If EXTREME? is
set, take more time (but only marginally more memory) to get a
slightly better compression ratio.  The value of CHECK controls which
checksum (if any) is used for the compressed data.  If CLOSE? is
set (the default), close PORT when the wrapper port is closed."
  (let* ((preset (logior level (if extreme? LZMA_PRESET_EXTREME 0)))
         (stream (make-lzma-encoder preset check)))
    (%make-xz-input-port port stream "xz-input/compressed" close?)))

(define (call-with-port* port proc)
  (dynamic-wind
    (const #t)
    (lambda () (proc port))
    (lambda () (close port))))

(define* (call-with-xz-output-port port proc
                                   #:key
                                   (level %default-compression-level)
                                   extreme?
                                   (check LZMA_CHECK_CRC64))
  "Call PROC with a new port that wraps PORT, compressing everything
written to it using the LZMA2 compression algorithm and the XZ
container format.  Upon exit of PROC, PORT will be closed.  Different
compression levels (from 0 to 9) can be set via LEVEL.  If EXTREME? is
set, take more time (but only marginally more memory) to get a
slightly better compression ratio.  The value of CHECK controls which
checksum (if any) is used for the compressed data."
  (let ((xz (make-xz-output-port port
                                 #:level level
                                 #:extreme? extreme?
                                 #:check check)))
    (call-with-port* xz proc)))

(define (call-with-xz-input-port port proc)
  "Call PROC with a new port that wraps PORT, decompressing everything
read from it using the LZMA2 algorithm and the XZ container.  Upon
exit of PROC, PORT will be closed."
  (let ((xz (make-xz-input-port port)))
    (call-with-port* xz proc)))

(define* (call-with-xz-input-port/compressed port proc
                                             #:key
                                             (level %default-compression-level)
                                             extreme?
                                             (check LZMA_CHECK_CRC64))
  "Call PROC with a new port that wraps PORT, compressing everything
read from it using the LZMA2 compression algorithm and the XZ container
format.  Upon exit of PROC, PORT will be closed.  Different compression
levels (from 0 to 9) can be set via LEVEL.  If EXTREME? is set, take
more time (but only marginally more memory) to get a slightly better
compression ratio.  The value of CHECK controls which checksum (if any)
is used for the compressed data."
  (let ((xz (make-xz-input-port/compressed port
                                           #:level level
                                           #:extreme? extreme?
                                           #:check check)))
    (call-with-port* xz proc)))

;;; Local Variables:
;;; eval: (put 'bind-lzma-procedure 'scheme-indent-function 4)
;;; End:
