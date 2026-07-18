;;; Disarchive
;;; Copyright © 2020-2022 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive scripts assemble)
  #:use-module (disarchive)
  #:use-module (disarchive config)
  #:use-module (disarchive utils)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:export (assemble-main))

(define options-grammar
  '((output (single-char #\o) (value #t))
    (help (single-char #\h))
    (version)))

(define help-message "\
Usage: disarchive assemble [OPTION]... DIRECTORY SPECIFICATION
Assemble DIRECTORY into an archive following SPECIFICATION.

If SPECIFICATION is set to '-', it will be read from standard input.

Options:
  -o, --output=FILE  write archive to FILE instead of standard output
  -h, --help         display this help and exit
  --version          display version information and exit
")

(define short-help-message "\
Run 'disarchive assmeble --help' for usage instructions.
")

(define (getopt-long* . args)
  (catch 'quit
    (lambda ()
      (apply getopt-long args))
    (lambda (_ status)
      (display short-help-message (current-error-port))
      (exit status))))

(define (make-resolver input)
  (lambda (addresses output)
    (copy-recursively input output #:log (%make-void-port "w"))))

(define (assemble-main . args)
  (%disarchive-log-port (current-error-port))
  (let ((options (getopt-long* args options-grammar)))
    (when (option-ref options 'help #f)
      (display help-message)
      (exit EXIT_SUCCESS))
    (when (option-ref options 'version #f)
      (display version-message)
      (exit EXIT_SUCCESS))
    (match (option-ref options '() '())
      ((directory specification)
       (let ((input (if (string=? specification "-")
                        (current-input-port)
                        specification))
             (output (option-ref options 'output (current-output-port))))
         (disarchive-assemble input output
                              #:resolver (make-resolver directory)))
       (exit EXIT_SUCCESS))
      (_ (with-output-to-port (current-error-port)
           (lambda ()
             (display "\
disarchive assemble: Both a directory and specification are required.
")
             (display short-help-message)
             (exit EXIT_FAILURE)))))))
