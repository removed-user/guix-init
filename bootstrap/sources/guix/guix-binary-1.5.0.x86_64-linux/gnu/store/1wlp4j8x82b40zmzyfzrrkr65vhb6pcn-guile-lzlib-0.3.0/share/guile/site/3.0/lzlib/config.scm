;;; Guile-lzlib --- GNU Guile bindings of lzlib
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
;;;
;;; This file is part of Guile-lzlib.
;;;
;;; Guile-lzlib is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-lzlib is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-lzlib.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lzlib config)
  #:export (%liblz))

(define %liblz
  "/gnu/store/f6x9cqngmg0hjm8jd0g8lvdl8gg5s0yw-lzlib-1.13/lib/../lib/liblz")
