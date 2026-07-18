;;; Guile-SemVer
;;; Copyright 2019 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Guile-SemVer.
;;;
;;; Guile-SemVer is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-SemVer is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-SemVer.  If not, see <https://www.gnu.org/licenses/>.

(define-module (semver partial)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (string->partial-semver))

(define number-string?
  (let ((rx (make-regexp "^(0|[1-9][0-9]*)$")))
    (lambda (str)
      (regexp-exec rx str))))

(define *partial-pattern*
  (let* ((nr        "(x|X|\\*|0|[1-9][0-9]*)")
         (part      (string-append "(" nr "|([0-9A-Za-z-]+))"))
         (parts     (string-append part "(\\." part ")*"))
         (qualifier (string-append "(\\-(" parts "))?(\\+(" parts "))?")))
    (string-append "^v?" nr
                   "(\\." nr ")?"
                   "(\\." nr ")?"
                   "(\\." nr ")?"
                   qualifier "$")))

(define *partial-rx*
  (make-regexp *partial-pattern*))

(define (string->partial-semver str)
  "Return the partial SemVer represented by the string STR.  A partial
SemVer is like a regular SemVar, but parts can be replaced by
wildcards (`x', `X', or `*') and trailing wildcards can be omitted.
If STR is not a syntactically valid notation for a partial SemVer,
then `string->partial-semver' returns `#f'.

This procedure returns a five-element list with the form `(MAJOR MINOR
PATCH (PRE-PARTS ...) (BUILD-PARTS ...))'.  If any of MAJOR, MINOR, or
PATCH is a wildcard, it will represented by the symbol `*'."
  (define (string->parts str)
    (match str
      (#f '())
      (_ (map (lambda (part)
                (if (number-string? part)
                    (string->number part)
                    part))
              (string-split str #\.)))))
  (define (number-or-* x)
    (or (string->number x) '*))
  (match (regexp-exec *partial-rx* str)
    (#f #f)
    (m (list (number-or-* (match:substring m 1))
             (number-or-* (or (match:substring m 3) "*"))
             (number-or-* (or (match:substring m 5) "*"))
             (number-or-* (or (match:substring m 7) "0"))
             (string->parts (match:substring m 9)) ; pre without "-"
             (string->parts (match:substring m 18)))))) ; build without "+"
