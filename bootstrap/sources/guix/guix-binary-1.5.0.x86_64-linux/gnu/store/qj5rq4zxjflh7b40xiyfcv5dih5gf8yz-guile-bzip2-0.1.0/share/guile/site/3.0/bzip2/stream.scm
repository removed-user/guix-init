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

(define-module (bzip2 stream)
  #:use-module (bytestructures guile))

;;; Commentary:
;;;
;;; This module is here to abstract out the use of the
;;; scheme-bytescructures library.  We need to be able to construct
;;; and modify 'bz_stream' structs, and while we could compute all
;;; the offsets manually, it's a bit tedious.  Putting the
;;; bytestructures stuff in its own module also avoids naming
;;; conflicts with Guile's normal FFI stuff.
;;;
;;; Code:

(define bz-stream-descriptor
  (bs:struct
   `(;; Input state.
     (next-in ,(bs:pointer uint8))
     (avail-in ,unsigned-int)
     (total-in-lo32 ,unsigned-int)
     (total-in-hi32 ,unsigned-int)
     ;; Output state.
     (next-out ,(bs:pointer uint8))
     (avail-out ,unsigned-int)
     (total-out-lo32 ,unsigned-int)
     (total-out-hi32 ,unsigned-int)
     ;; Internal state.
     (state ,(bs:pointer 'void))
     ;; Custom memory allocation (unimplemented).
     (bzalloc ,(bs:pointer 'void))
     (bzfree ,(bs:pointer 'void))
     (opaque ,(bs:pointer 'void)))))

(define-public (make-bz-stream-bv . values)
  "Construct a bytevector formatted like a 'bz_stream' struct
initialized with VALUES."
  (bytestructure-bytevector
   (bytestructure bz-stream-descriptor (list->vector values))))

;; Only define and expose the fields that we need.

(define-syntax define-public-bz-stream-bv-fields
  (syntax-rules ()
    ((_ ()) *unspecified*)
    ((_ ((field accessor modifier) . rest))
     (begin
       (define-public (accessor bv)
         (bytestructure-ref* bv 0 bz-stream-descriptor 'field))
       (define-public (modifier bv val)
         (bytestructure-set!* bv 0 bz-stream-descriptor 'field val))
       (define-public-bz-stream-bv-fields rest)))))

(define-public-bz-stream-bv-fields
  ((next-in bz-stream-bv-next-in set-bz-stream-bv-next-in!)
   (avail-in bz-stream-bv-avail-in set-bz-stream-bv-avail-in!)
   (next-out bz-stream-bv-next-out set-bz-stream-bv-next-out!)
   (avail-out bz-stream-bv-avail-out set-bz-stream-bv-avail-out!)))
