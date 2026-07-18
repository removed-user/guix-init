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

(define-module (lzma stream)
  #:use-module (bytestructures guile))

;;; Commentary:
;;;
;;; This module is here to abstract out the use of the
;;; scheme-bytescructures library.  We need to be able to construct
;;; and modify 'lzma_stream' structs, and while we could compute all
;;; the offsets manually, it's a bit tedious.  Putting the
;;; bytestructures stuff in its own module also avoids naming
;;; conflicts with Guile's normal FFI stuff.
;;;
;;; Code:

(define lzma-stream-descriptor
  (bs:struct
   `(;; Input state.
     (next-in ,(bs:pointer uint8))
     (avail-in ,size_t)
     (total-in ,uint64)
     ;; Output state.
     (next-out ,(bs:pointer uint8))
     (avail-out ,size_t)
     (total-out ,uint64)
     ;; Custom memory allocation (unimplemented).
     (lzma-allocator ,(bs:pointer 'void))
     ;; Internal state.
     (lzma-internal ,(bs:pointer 'void))
     ;; Reserved space for ABI-preserving extensions.
     (reserved-ptr1 ,(bs:pointer 'void))
     (reserved-ptr2 ,(bs:pointer 'void))
     (reserved-ptr3 ,(bs:pointer 'void))
     (reserved-ptr4 ,(bs:pointer 'void))
     (reserved-int1 ,uint64)
     (reserved-int2 ,uint64)
     (reserved-int3 ,size_t)
     (reserved-int4 ,size_t)
     (reserved-enum1 ,int)
     (reserved-enum2 ,int))))

(define-public (make-lzma-stream-bv . values)
  "Construct a bytevector formatted like an 'lzma_stream' struct
initialized with VALUES."
  (bytestructure-bytevector
   (bytestructure lzma-stream-descriptor (list->vector values))))

;; Only define and expose the fields that we need.

(define-syntax define-public-lzma-stream-bv-fields
  (syntax-rules ()
    ((_ ()) *unspecified*)
    ((_ ((field accessor modifier) . rest))
     (begin
       (define-public (accessor bv)
         (bytestructure-ref* bv 0 lzma-stream-descriptor 'field))
       (define-public (modifier bv val)
         (bytestructure-set!* bv 0 lzma-stream-descriptor 'field val))
       (define-public-lzma-stream-bv-fields rest)))))

(define-public-lzma-stream-bv-fields
  ((next-in lzma-stream-bv-next-in set-lzma-stream-bv-next-in!)
   (avail-in lzma-stream-bv-avail-in set-lzma-stream-bv-avail-in!)
   (next-out lzma-stream-bv-next-out set-lzma-stream-bv-next-out!)
   (avail-out lzma-stream-bv-avail-out set-lzma-stream-bv-avail-out!)))
