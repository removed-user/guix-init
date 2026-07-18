;;; Disarchive
;;; Copyright © 2020, 2021 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive logging)
  #:export (%disarchive-log-port
            message
            start-message
            without-logging))

;;; Commentary:
;;;
;;; This module provides a simple logging interface.
;;;
;;; Code:

(define %disarchive-log-port (make-parameter #f))

(define-syntax-rule (message format-string value ...)
  (let ((port (%disarchive-log-port)))
    (when port
      (format port format-string value ...)
      (newline port))))

(define-syntax-rule (start-message format-string value ...)
  (let ((port (%disarchive-log-port)))
    (when port
      (format port format-string value ...)
      (force-output port))))

(define-syntax-rule (without-logging body ...)
  (parameterize ((%disarchive-log-port (%make-void-port "w")))
    body ...))
