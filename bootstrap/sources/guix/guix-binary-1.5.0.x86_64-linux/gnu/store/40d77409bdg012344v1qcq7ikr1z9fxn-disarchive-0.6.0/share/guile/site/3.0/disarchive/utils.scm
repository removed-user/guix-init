;;; Disarchive
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020, 2021 Timothy Sample <samplet@ngyro.com>
;;;
;;; The procedures 'call-with-temporary-output-file' and
;;; 'call-with-temporary-directory' are taken from the
;;; 'guix/utils.scm' file of Guix.  That file has the following
;;; copyright notices:
;;;
;;; Copyright © 2012, 2013, 2014, 2015, 2016,
;;;     2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020 Marius Bakke <marius@gnu.org>
;;;
;;; The procedures 'copy-recursively', 'delete-file-recursively',
;;; 'directory-exists?', 'invoke', and 'mkdir-p' are taken from the
;;; 'guix/build/utils.scm' file of Guix.  That file has the following
;;; copyright notices:
;;;
;;; Copyright © 2012, 2013, 2014, 2015, 2016,
;;;     2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (disarchive utils)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:export (dump-port-n
            dump-port-all
            call-with-temporary-output-file
            call-with-temporary-directory
            copy-recursively
            delete-file-recursively
            directory-exists?
            invoke
            mkdir-p
            make-thing-encoder
            bytevector-zero?
            bytevector-index
            bytevector-append
            sub-bytevector
            bytevector-fill!*))

(define libc (dynamic-link))

(define mkdtemp!
  (let* ((fptr (dynamic-func "mkdtemp" libc))
         (f (pointer->procedure '* fptr '(*) #:return-errno? #t)))
    (lambda (template)
      (call-with-values (lambda () (f (string->pointer template)))
        (lambda (result errno)
          (when (null-pointer? result)
            (scm-error 'system-error 'mkdtemp! "~A"
                       (list (strerror errno))
                       (list errno)))
          (pointer->string result))))))

(define (dump-port-n in out size)
  "Copy SIZE bytes from IN to OUT."
  (define buf-size 65536)
  (define buf (make-bytevector buf-size))

  (let loop ((left size))
    (if (<= left 0)
        0
        (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
          (if (eof-object? read)
              left
              (begin
                (put-bytevector out buf 0 read)
                (loop (- left read))))))))

(define (dump-port-all in out)
  "Drain the port IN and write everything to the port OUT."
  (define buf-len (* 64 1024))
  (define buf (make-bytevector buf-len))
  (let loop ((n (get-bytevector-n! in buf 0 buf-len)))
    (unless (eof-object? n)
      (put-bytevector out buf 0 n)
      (loop (get-bytevector-n! in buf 0 buf-len)))))

(define (call-with-temporary-output-file proc)
  "Call PROC with a name of a temporary file and open output port to that
file; close the file and delete it when leaving the dynamic extent of this
call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/disarchive-file.XXXXXX"))
         (out       (mkstemp! template)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc template out))
      (lambda ()
        (false-if-exception (close out))
        (false-if-exception (delete-file template))))))

(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/disarchive-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define* (copy-recursively source destination
                           #:key
                           (log (current-output-port))
                           (follow-symlinks? #f))
  "Copy SOURCE directory to DESTINATION.  Follow symlinks if
FOLLOW-SYMLINKS?  is true; otherwise, just preserve them.  Write verbose
output to the LOG port."
  (define strip-source
    (let ((len (string-length source)))
      (lambda (file)
        (substring file len))))

  (file-system-fold (const #t)                    ; enter?
                    (lambda (file stat result)    ; leaf
                      (let ((dest (string-append destination
                                                 (strip-source file))))
                        (format log "`~a' -> `~a'~%" file dest)
                        (case (stat:type stat)
                          ((symlink)
                           (let ((target (readlink file)))
                             (symlink target dest)))
                          (else
                           (copy-file file dest)))))
                    (lambda (dir stat result)     ; down
                      (let ((target (string-append destination
                                                   (strip-source dir))))
                        (mkdir-p target)))
                    (lambda (dir stat result)     ; up
                      result)
                    (const #t)                    ; skip
                    (lambda (file stat errno result)
                      (format (current-error-port) "i/o error: ~a: ~a~%"
                              file (strerror errno))
                      #f)
                    #t
                    source

                    (if follow-symlinks?
                        stat
                        lstat)))

(define* (delete-file-recursively dir
                                  #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.
Don't follow mount points either, unless FOLLOW-MOUNTS? is true.  Report
but ignore errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)    ; enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result)   ; leaf
                        (delete-file file))
                      (const #t)                   ; down
                      (lambda (dir stat result)    ; up
                        (rmdir dir))
                      (const #t)                   ; skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))

(define (directory-exists? dir)
  "Return #t if DIR exists and is a directory."
  (let ((s (stat dir #f)))
    (and s
         (eq? 'directory (stat:type s)))))

(define (invoke program . args)
  "Invoke PROGRAM with the given ARGS.  Raise an exception
if the exit code is non-zero; otherwise return #t."
  (let ((code (apply system* program args)))
    (unless (zero? code)
      (scm-error 'misc-error 'invoke
                 "command ~A with arguments ~S failed with code ~A"
                 (list program args (status:exit-val code))
                 (list code)))
    #t))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (make-thing-encoder thing->bytevector)
  "Create a procedure with the full encoder interface based on
THING->BYTEVECTOR."
  (define (encode-thing thing bv start end)
    (let* ((tbv (thing->bytevector thing))
           (tbv-len (bytevector-length tbv))
           (space (- end start))
           (leftover-space (- space tbv-len)))
      (bytevector-copy! tbv 0 bv start (min tbv-len (- end start)))
      (when (positive? leftover-space)
        (bytevector-fill!* bv 0 end leftover-space))))
  (case-lambda
    ((thing) (thing->bytevector thing))
    ((thing bv) (encode-thing thing bv 0 (bytevector-length bv)))
    ((thing bv start) (encode-thing thing bv start (bytevector-length bv)))
    ((thing bv start end) (encode-thing thing bv start end))))

(define* (bytevector-zero? bv #:optional
                           (start 0) (end (bytevector-length bv)))
  (let loop ((k start))
    (or (>= k end)
        (and (zero? (bytevector-u8-ref bv k))
             (loop (1+ k))))))

(define* (bytevector-index bv byte #:optional
                           (start 0) (end (bytevector-length bv)))
  "Find the index of the first occurance of BYTE in BV (starting at
index START and stopping at END).  If BYTE does not occur in BV, this
procedure returns \"#f\".  If omitted, START defaults to 0 and END to
the length of BV."
  (let loop ((k start))
    (and (< k end)
         (if (= (bytevector-u8-ref bv k) byte)
             k
             (loop (1+ k))))))

(define (bytevector-append . bvs)
  "Return a bytevector whose bytes form the concatenation of the given
bytevectors BVS."
  (let* ((len (reduce + 0 (map bytevector-length bvs)))
         (result (make-bytevector len)))
    (let loop ((bvs bvs) (k 0))
      (match bvs
        (() result)
        ((bv . rest)
         (let ((bv-len (bytevector-length bv)))
           (bytevector-copy! bv 0 result k bv-len)
           (loop rest (+ k bv-len))))))))

(define* (sub-bytevector bv #:optional
                         (start 0) (end (bytevector-length bv)))
  "Create a new bytevector containing the bytes of BV starting at START
and ending at END.  If omitted, START defaults to 0 and END to the
length of BV."
  (let* ((size (- end start))
         (sub (make-bytevector size)))
    (bytevector-copy! bv start sub 0 size)
    sub))

(define* (bytevector-fill!* bv fill start end)
  "Fill BV with the byte FILL starting at START and ending at END."
  (let loop ((k start))
    (when (< k end)
      (bytevector-u8-set! bv k fill)
      (loop (1+ k)))))
