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

(define-module (bzip2 config)
  #:export (%bzip2-library-path))

(define %bzip2-library-path "/gnu/store/3m35da59fr3wwdy0imzi2jg29jd6lhlz-bzip2-1.0.8/lib/../lib/libbz2")
