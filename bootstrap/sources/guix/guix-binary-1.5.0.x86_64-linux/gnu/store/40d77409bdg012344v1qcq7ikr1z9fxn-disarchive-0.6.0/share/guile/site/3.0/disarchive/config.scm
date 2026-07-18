;;; Disarchive
;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive config)
  #:export (%package-name
            %version
            version-message
            %tar
            %gzip
            %xz
            %bzip2
            %zgz
            %disarchive-directory-cache))

;;; Commentary:
;;;
;;; This module provides system-specific values.
;;;
;;; Code:

(define %package-name "Disarchive")
(define %version "0.6.0")

(define version-message (format #f "~a ~a~%" %package-name %version))

(define DISARCHIVE_O_NOFOLLOW 131072)

;; Older versions of Guile do not have O_NOFOLLOW, but newer ones do.
;; Hence, we check for O_NOFOLLOW and use the Guile version if we can.
(unless (and=> (module-variable (resolve-interface '(guile)) 'O_NOFOLLOW)
               variable-bound?)
  (export (DISARCHIVE_O_NOFOLLOW . O_NOFOLLOW)))

(define %tar "/gnu/store/x5nk7jclncdrrsjhpmjjfl2z9p36bxyw-tar-1.35/bin/tar")

(define %gzip "/gnu/store/vcmbby8alqahhhd5qnidpf5pbvr102jl-gzip-1.14/bin/gzip")

(define %xz "/gnu/store/gd41pn8r5723cyd6d79q9riqjbi070zz-xz-5.4.5/bin/xz")

(define %bzip2 "/gnu/store/j2bib9ysq7f3369gjg6hv1kp24g07472-bzip2-1.0.8/bin/bzip2")

(define (%zgz)
  (or (getenv "DISARCHIVE_ZGZ")
      "/gnu/store/40d77409bdg012344v1qcq7ikr1z9fxn-disarchive-0.6.0/libexec/disarchive-zgz"))

(define %disarchive-directory-cache (make-parameter #f))
