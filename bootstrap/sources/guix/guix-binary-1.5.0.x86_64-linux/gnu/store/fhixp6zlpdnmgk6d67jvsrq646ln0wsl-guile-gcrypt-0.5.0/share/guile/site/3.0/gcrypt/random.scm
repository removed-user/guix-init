;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;;
;;; This file is part of guile-gcrypt.
;;;
;;; guile-gcrypt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; guile-gcrypt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with guile-gcrypt.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gcrypt random)
  #:use-module (gcrypt internal)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:export (%gcry-weak-random
            %gcry-strong-random
            %gcry-very-strong-random
            gen-random-bv
            random-token))

(define %gcry-weak-random 0)  ; not used
(define %gcry-strong-random 1)
(define %gcry-very-strong-random 2)

(define %gcry-randomize
  (libgcrypt->procedure void
                      "gcry_randomize"
                      `(* ,size_t ,int)))  ; buffer, length, level

(define* (gen-random-bv #:optional (bv-length 50)
                        (level %gcry-strong-random))
  (let* ((bv (make-bytevector bv-length))
         (bv-ptr (bytevector->pointer bv)))
    (%gcry-randomize bv-ptr bv-length %gcry-strong-random)
    bv))

(define %gcry-create-nonce
  (libgcrypt->procedure void "gcry_create_nonce"
                        `(* ,size_t)))  ; buffer, length


(define* (gen-random-nonce #:optional (bv-length 50))
  (let* ((bv (make-bytevector bv-length))
         (bv-ptr (bytevector->pointer bv)))
    (%gcry-create-nonce bv-ptr bv-length)
    bv))

(define* (random-token #:optional (bv-length 30)
                       (type 'strong))
  "Generate a random token.

Generates a token of bytevector BV-LENGTH, default 30.

The default TYPE is 'strong.  Possible values are:
 - strong: Uses libgcrypt's gcry_randomize procedure with level
   GCRY_STRONG_RANDOM (\"use this level for session keys and similar
   purposes\").
 - very-strong: Also uses libgcrypt's gcry_randomize procedure with level
   GCRY_VERY_STRONG_RANDOM (\"Use this level for long term key material\")
 - nonce: Uses libgcrypt's gcry_xcreate_nonce, whose documentation I'll
   just quote inline:

     Fill BUFFER with LENGTH unpredictable bytes.  This is commonly
     called a nonce and may also be used for initialization vectors and
     padding.  This is an extra function nearly independent of the other
     random function for 3 reasons: It better protects the regular
     random generator's internal state, provides better performance and
     does not drain the precious entropy pool."
  (let ((bv (match type
              ('strong
               (gen-random-bv bv-length %gcry-strong-random))
              ('very-strong
               (gen-random-bv bv-length %gcry-very-strong-random))
              ('nonce
               (gen-random-nonce bv-length)))))
    (base64-encode bv 0 bv-length #f #t base64url-alphabet)))
