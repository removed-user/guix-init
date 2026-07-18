;;; Guile-zlib --- GNU Guile bindings of zlib
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
;;;
;;; This file is part of Guile-zlib.
;;;
;;; Guile-zlib is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-zlib is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-zlib.  If not, see <http://www.gnu.org/licenses/>.

(define-module (zlib config)
  #:export (%libz))

(define %libz
  "/gnu/store/a2jnc1avp7jdyp01r9kpp8q1i72kk8g0-zlib-1.3.1/lib/libz")
