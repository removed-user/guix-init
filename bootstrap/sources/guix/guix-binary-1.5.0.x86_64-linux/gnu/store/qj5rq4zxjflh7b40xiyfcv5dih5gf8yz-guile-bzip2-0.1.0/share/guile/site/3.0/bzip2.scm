;;; Guile-bzip2
;;; Copyright © 2022 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Guile-bzip2.
;;;
;;; Guile-bzip2 is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-bzip2 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-bzip2.  If not, see <http://www.gnu.org/licenses/>.

(define-module (bzip2)
  #:use-module (bzip2 config)
  #:use-module (bzip2 stream)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (%default-compression-level
            make-bzip2-output-port
            make-bzip2-input-port
            make-bzip2-input-port/compressed
            call-with-bzip2-output-port
            call-with-bzip2-input-port
            call-with-bzip2-input-port/compressed
            bzip2-error?
            bzip2-error-symbol
            bzip2-error-code))


;; Low-level FFI stuff.

(define %bzip2
  (delay (dynamic-link %bzip2-library-path)))

(define (symbol->bzip2-name sym)
  (match (string-split (symbol->string sym) #\-)
    (("bz" . rest)
     (apply string-append "BZ2_bz"
            (map string-capitalize rest)))
    (_ (symbol->string sym))))

;; Nicer syntax for the 'dynamic-func' and 'pointer->procedure' dance.
(define-syntax bind-bzip2-procedure
  (syntax-rules (->)
    ((_ (scheme-name c-name) args -> result body ...)
     (let* ((proc-ptr (dynamic-func c-name (force %bzip2)))
            (scheme-name (pointer->procedure result proc-ptr args)))
       body ...))
    ((_ name args -> result body ...)
     (bind-bzip2-procedure (name (symbol->bzip2-name 'name))
         args -> result
       body ...))))

;; Actions.
(define BZ_RUN 0)
(define BZ_FLUSH 1)
(define BZ_FINISH 2)

;; Successful return values.
(define BZ_OK 0)
(define BZ_RUN_OK 1)
(define BZ_FLUSH_OK 2)
(define BZ_FINISH_OK 3)
(define BZ_STREAM_END 4)

;; Error return values.
(define %bz-errors
  '((-1 BZ_SEQUENCE_ERROR "Programming error")
    (-2 BZ_PARAM_ERROR "Invalid argument")
    (-3 BZ_MEM_ERROR "Cannot allocate memory")
    (-4 BZ_DATA_ERROR "Data is corrupt")
    (-5 BZ_DATA_ERROR_MAGIC "File format not recognized")
    (-6 BZ_IO_ERROR "Error reading or writing compressed file")
    (-7 BZ_UNEXPECTED_EOF "Compressed file ends before end of stream")
    (-8 BZ_OUTBUFF_FULL "Output data too large for buffer")
    (-9 BZ_CONFIG_ERROR "The current platform is not supported")))

(define %bz-compress-end
  (delay (dynamic-func "BZ2_bzCompressEnd" (force %bzip2))))

(define %bz-decompress-end
  (delay (dynamic-func "BZ2_bzDecompressEnd" (force %bzip2))))


;; Mid-level helpers.

;; The libbzip2 API requires that we clean up streams using either the
;; 'BZ2_bzCompressEnd' or 'BZ2_bzDecompressEnd' C functions.  We do
;; this using pointer finalizers as usual.  However, we also need to
;; modify the streams, so we keep a reference to both the pointer and
;; the bytevector it points to.  The reference to the pointer prevents
;; the finalizer from running until we are done, and the bytevector
;; lets us access and modify the fields of the stream.
(define-record-type <bz-stream>
  (%make-bz-stream pointer bv)
  bz-stream?
  (pointer bz-stream-pointer)
  (bv bz-stream-bv))

(define (make-bz-stream finalizer)
  "Return a new bzip2 stream initialized with default values."
  (define bv
    (make-bz-stream-bv %null-pointer 0 0 0
                       %null-pointer 0 0 0
                       %null-pointer
                       %null-pointer %null-pointer %null-pointer))
  (define ptr (bytevector->pointer bv))
  (set-pointer-finalizer! ptr finalizer)
  (%make-bz-stream ptr bv))

(define* (set-bz-stream-next-in! strm bv #:optional (offset 0))
  (set-bz-stream-bv-next-in!
   (bz-stream-bv strm)
   (pointer-address (bytevector->pointer bv offset))))

(define* (set-bz-stream-next-out! strm bv #:optional (offset 0))
  (set-bz-stream-bv-next-out!
   (bz-stream-bv strm)
   (pointer-address (bytevector->pointer bv offset))))

(define (bz-stream-avail-in strm)
  (bz-stream-bv-avail-in (bz-stream-bv strm)))

(define (set-bz-stream-avail-in! strm n)
  (set-bz-stream-bv-avail-in! (bz-stream-bv strm) n))

(define (bz-stream-avail-out strm)
  (bz-stream-bv-avail-out (bz-stream-bv strm)))

(define (set-bz-stream-avail-out! strm n)
  (set-bz-stream-bv-avail-out! (bz-stream-bv strm) n))

(define-exception-type &bzip2-error &error
  make-bzip2-error
  bzip2-error?
  (symbol bzip2-error-symbol)
  (code bzip2-error-code))

(define-syntax-rule (bzip2-error code)
  (match (assoc code %bz-errors)
    ((code symbol msg)
     (raise-exception (make-exception (make-bzip2-error symbol code)
                                      (make-exception-with-message msg))))
    (_
     (raise-exception (make-exception (make-bzip2-error #f code)
                                      (make-exception-with-message
                                       "Unknown bzip2 error"))))))

(define (bz-ok? code)
  (or (= code BZ_OK)
      (= code BZ_RUN_OK)
      (= code BZ_FLUSH_OK)
      (= code BZ_FINISH_OK)))

(define (bz-stream-end? code)
  (= code BZ_STREAM_END))

(define make-bz-compressor
  (bind-bzip2-procedure bz-compress-init
      `(* ,int ,int ,int) -> int
    (lambda (block-size verbosity work-factor)
      (let ((strm (make-bz-stream (force %bz-compress-end))))
        (match (bz-compress-init (bz-stream-pointer strm)
                                 block-size verbosity work-factor)
          ((? bz-ok?) strm)
          (code (bzip2-error code)))))))

(define bz-compress
  (bind-bzip2-procedure bz-compress
      `(* ,int) -> int
    (lambda (strm action)
      (bz-compress (bz-stream-pointer strm) action))))

(define make-bz-decompressor
  (bind-bzip2-procedure bz-decompress-init
      `(* ,int ,int) -> int
    (lambda (verbosity small?)
      (let ((strm (make-bz-stream (force %bz-decompress-end))))
        (match (bz-decompress-init (bz-stream-pointer strm)
                                   verbosity (if small? 1 0))
          ((? bz-ok?) strm)
          (code (bzip2-error code)))))))

(define bz-decompress
  (bind-bzip2-procedure bz-decompress
      `(*) -> int
    (lambda (strm)
      (bz-decompress (bz-stream-pointer strm)))))


;; High-level interface.

(define %default-compression-level 9)

(define* (make-bzip2-output-port port
                                 #:key
                                 (level %default-compression-level)
                                 (close? #t))
  "Return a new port that wraps PORT, compressing everything written
to it using bzip2.  Different compression levels (from 1 to 9) can be
set via LEVEL.  If CLOSE? is set (the default), close PORT when the
wrapper port is closed."
  (define stream (make-bz-compressor level 0 0))

  (define buffer (make-bytevector 4096))

  (define (reset-buffer!)
    (set-bz-stream-next-out! stream buffer)
    (set-bz-stream-avail-out! stream (bytevector-length buffer)))

  (define (flush-output!)
    (put-bytevector port buffer 0
                    (- (bytevector-length buffer)
                       (bz-stream-avail-out stream)))
    (reset-buffer!))

  (define (finish!)
    (let loop ()
      (define state (bz-compress stream BZ_FINISH))
      (flush-output!)
      (cond
       ((bz-stream-end? state) 0)
       ((bz-ok? state) (loop))
       (else (bzip2-error state)))))

  (define (write! bv start count)
    (set-bz-stream-next-in! stream bv start)
    (set-bz-stream-avail-in! stream count)
    (match (bz-compress stream BZ_RUN)
      ((? bz-ok?)
       (flush-output!)
       (- count (bz-stream-avail-in stream)))
      (code (bzip2-error code))))

  (define (close)
    (finish!)
    (when close?
      (close-port port)))

  (reset-buffer!)
  (make-custom-binary-output-port "bzip2-output" write! #f #f close))

(define (%make-bzip2-input-port port stream run finish name close?)
  "Return a new port that wraps PORT, processing it with the bzip2
codec described by the <bz-stream> STREAM.  The RUN and FINISH
arguments must be one argument procedures implementing running and
finishing the stream respectively.  The port will be named NAME.  If
CLOSE? is set, close PORT when the wrapper port is closed."
  (define buffer (make-bytevector 4096))
  (define stream-end? #f)

  (define (read! bv start count)
    (set-bz-stream-next-out! stream bv start)
    (set-bz-stream-avail-out! stream count)
    (cond
     ((positive? (bz-stream-avail-in stream))
      (when stream-end?
        ;; The compressed stream ended early; raise BZ_UNEXPECTED_EOF.
        (bzip2-error -7))
      (match (run stream)
        ((and (or (? bz-ok?) (? bz-stream-end?)) code)
         (when (bz-stream-end? code)
           (set! stream-end? #t))
         (match (- count (bz-stream-avail-out stream))
           (0 (read! bv start count))
           (n n)))
        (code (bzip2-error code))))
     (else
      (match (get-bytevector-n! port buffer 0 (bytevector-length buffer))
        ((? eof-object?)
         (match (if stream-end? BZ_STREAM_END (finish stream))
           ((? bz-stream-end?) (begin
                                 (set! stream-end? #t)
                                 (- count (bz-stream-avail-out stream))))
           ((? bz-ok?) (match (- count (bz-stream-avail-out stream))
                         (0 (read! bv start count))
                         (n n)))
           (code (bzip2-error code))))
        (buffer-count
         (set-bz-stream-next-in! stream buffer)
         (set-bz-stream-avail-in! stream buffer-count)
         (read! bv start count))))))

  (define (close)
    (when close?
      (close-port port)))

  (make-custom-binary-input-port name read! #f #f close))

(define* (make-bzip2-input-port port
                                #:key
                                (close? #t))
  "Return a new port that wraps PORT, decompressing everything read
from it using bzip2.  If CLOSE? is set (the default), close PORT when
the wrapper port is closed."
  (define (run stream)
    (bz-decompress stream))
  (define (finish stream)
    (let* ((avail-in (bz-stream-avail-in stream))
           (avail-out (bz-stream-avail-out stream))
           (result (bz-decompress stream)))
      (when (and (bz-ok? result)
                 (= avail-in (bz-stream-avail-in stream))
                 (= avail-out (bz-stream-avail-out stream)))
        ;; Raise BZ_DATA_ERROR as the compressed stream is truncated.
        (bzip2-error -4))
      result))
  (let ((stream (make-bz-decompressor 0 #f)))
    (%make-bzip2-input-port port stream run finish
                            "bzip2-input" close?)))

(define* (make-bzip2-input-port/compressed port
                                           #:key
                                           (level %default-compression-level)
                                           (close? #t))
  "Return a new port that wraps PORT, compressing everything read from
it using bzip2.  Different compression levels (from 1 to 9) can be set
via LEVEL.  If CLOSE? is set (the default), close PORT when the
wrapper port is closed."
  (define (run stream)
    (bz-compress stream BZ_RUN))
  (define (finish stream)
    (bz-compress stream BZ_FINISH))
  (let ((stream (make-bz-compressor level 0 0)))
    (%make-bzip2-input-port port stream run finish
                            "bzip2-input/compressed" close?)))

(define (call-with-port* port proc)
  (dynamic-wind
    (const #t)
    (lambda () (proc port))
    (lambda () (close port))))

(define* (call-with-bzip2-output-port port proc
                                      #:key
                                      (level %default-compression-level))
  "Call PROC with a new port that wraps PORT, compressing everything
written to it using bzip2.  Upon exit of PROC, PORT will be closed.
Different compression levels (from 1 to 9) can be set via LEVEL."
  (let ((bzip2 (make-bzip2-output-port port #:level level)))
    (call-with-port* bzip2 proc)))

(define (call-with-bzip2-input-port port proc)
  "Call PROC with a new port that wraps PORT, decompressing everything
read from it using bzip2.  Upon exit of PROC, PORT will be closed."
  (let ((bzip2 (make-bzip2-input-port port)))
    (call-with-port* bzip2 proc)))

(define* (call-with-bzip2-input-port/compressed port proc
                                                #:key
                                                (level
                                                 %default-compression-level))
  "Call PROC with a new port that wraps PORT, compressing everything
read from it using bzip2.  Upon exit of PROC, PORT will be closed.
Different compression levels (from 1 to 9) can be set via LEVEL."
  (let ((bzip2 (make-bzip2-input-port/compressed port #:level level)))
    (call-with-port* bzip2 proc)))

;;; Local Variables:
;;; eval: (put 'bind-bzip2-procedure 'scheme-indent-function 4)
;;; End:
