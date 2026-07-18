;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of guile-gcrypt.
;;;
;;; guile-gcrypt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; guile-gcrypt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-gcrypt.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gcrypt package-config)
  #:export (%libgcrypt
            %guile-gcrypt-package-name %guile-gcrypt-version))

(define %libgcrypt
  "/gnu/store/s4j90ngj19j3h5ckwqvis1kdg95yq904-libgcrypt-1.11.0/lib/libgcrypt")

(define %guile-gcrypt-package-name
  "Guile-Gcrypt")

(define %guile-gcrypt-version
  "0.5.0")
