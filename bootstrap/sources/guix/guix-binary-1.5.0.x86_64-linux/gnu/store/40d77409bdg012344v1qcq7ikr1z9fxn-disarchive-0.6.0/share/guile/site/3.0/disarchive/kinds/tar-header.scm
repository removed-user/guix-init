;;; Disarchive
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (disarchive kinds tar-header)
  #:use-module (disarchive kinds binary-string)
  #:use-module (disarchive kinds octal)
  #:use-module (disarchive kinds tar-extension) ; recursive
  #:use-module (disarchive kinds zero-string)
  #:use-module (disarchive serialization)
  #:use-module (disarchive utils)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-71)
  #:export (<tar-header>
            make-tar-header
            tar-header?
            tar-header-name
            tar-header-mode
            tar-header-uid
            tar-header-gid
            tar-header-size
            tar-header-mtime
            tar-header-chksum
            tar-header-typeflag
            tar-header-linkname
            tar-header-magic
            tar-header-version
            tar-header-uname
            tar-header-gname
            tar-header-devmajor
            tar-header-devminor
            tar-header-prefix
            tar-header-padding
            tar-header-data-padding
            set-tar-header-data-padding
            tar-header-extension
            set-tar-header-extension

            tar-header-path

            bytevector->tar-header
            tar-header->bytevector
            read-tar-header
            write-tar-header
            end-of-tarball-object?
            -tar-header-
            %default-default-tar-header
            default-tar-header))

;;; Commentary:
;;;
;;; A tar header is a record of fields that describe a file included
;;; in a tarball.
;;;
;;; Code:

(define-immutable-record-type <tar-header>
  (make-tar-header name mode uid gid size mtime chksum typeflag
                   linkname magic version uname gname devmajor
                   devminor prefix padding data-padding extension)
  tar-header?
  ;; (? zero-string?)
  (name %tar-header-name)
  ;; (? octal?)
  (mode %tar-header-mode)
  ;; (? octal?)
  (uid %tar-header-uid)
  ;; (? octal?)
  (gid %tar-header-gid)
  ;; (? octal?)
  (size %tar-header-size)
  ;; (? octal?)
  (mtime %tar-header-mtime)
  ;; (? octal?)
  (chksum %tar-header-chksum)
  ;; (? byte?)
  (typeflag %tar-header-typeflag)
  ;; (? zero-string?)
  (linkname %tar-header-linkname)
  ;; (? binary-string?)
  (magic tar-header-magic)
  ;; (? binary-string?)
  (version tar-header-version)
  ;; (? zero-string?)
  (uname %tar-header-uname)
  ;; (? zero-string?)
  (gname %tar-header-gname)
  ;; (? octal?)
  (devmajor %tar-header-devmajor)
  ;; (? octal?)
  (devminor %tar-header-devminor)
  ;; (? zero-string?)
  (prefix %tar-header-prefix)
  ;; (? binary-string?)
  (padding tar-header-padding)
  ;; (? binary-string?)
  (data-padding tar-header-data-padding set-tar-header-data-padding)
  ;; (or (? tar-extension?) #f)
  (extension tar-header-extension set-tar-header-extension))

(define tar-header-name (compose zero-string-value %tar-header-name))
(define tar-header-mode (compose octal-value %tar-header-mode))
(define tar-header-uid (compose octal-value %tar-header-uid))
(define tar-header-gid (compose octal-value %tar-header-gid))
(define tar-header-size (compose octal-value %tar-header-size))
(define tar-header-mtime (compose octal-value %tar-header-mtime))
(define tar-header-chksum (compose octal-value %tar-header-chksum))
(define tar-header-linkname (compose zero-string-value %tar-header-linkname))
(define tar-header-uname (compose zero-string-value %tar-header-uname))
(define tar-header-gname (compose zero-string-value %tar-header-gname))
(define tar-header-devmajor (compose octal-value %tar-header-devmajor))
(define tar-header-devminor (compose octal-value %tar-header-devminor))
(define tar-header-prefix (compose zero-string-value %tar-header-prefix))

;; XXX: This needs to be a procedure rather than a macro due to the
;; module dependency loop between tar-header and tar-extension.
(define (tar-header-typeflag header)
  (%tar-header-typeflag header))

(define (tar-header-path header)
  (or (and=> (tar-header-extension header)
             (lambda (extension)
               (any (match-lambda
                      (("path" . (? zero-string? value))
                       (zero-string-value value))
                      (("path" . (? string? value))
                       value)
                      (_ #f))
                    (tar-extension-content extension))))
      (let ((name (tar-header-name header))
            (prefix (tar-header-prefix header)))
        (if (string-null? prefix)
            name
            (string-append prefix "/" name)))))

(define (bytevector->tar-header bv)
  (let ((name (decode-zero-string bv 0 100))
        (mode (decode-octal bv 100 108))
        (uid (decode-octal bv 108 116))
        (gid (decode-octal bv 116 124))
        (size (decode-octal bv 124 136))
        (mtime (decode-octal bv 136 148))
        (chksum (decode-octal bv 148 156))
        (typeflag (bytevector-u8-ref bv 156))
        (linkname (decode-zero-string bv 157 257))
        (magic (decode-binary-string bv 257 263))
        (version (decode-binary-string bv 263 265))
        (uname (decode-zero-string bv 265 297))
        (gname (decode-zero-string bv 297 329))
        (devmajor (decode-octal bv 329 337))
        (devminor (decode-octal bv 337 345))
        (prefix (decode-zero-string bv 345 500))
        (padding (if (bytevector-zero? bv 500 512)
                     ""
                     (decode-binary-string bv 500 512)))
        (data-padding "")
        (extension #f))
    (make-tar-header name mode uid gid size mtime chksum typeflag
                     linkname magic version uname gname devmajor
                     devminor prefix padding data-padding extension)))

(define* (tar-header->bytevector header #:optional
                                 (bv (make-bytevector 512)))
  (match-let ((($ <tar-header> name mode uid gid size mtime chksum
                  typeflag linkname magic version uname gname devmajor
                  devminor prefix padding data-padding extension)
               header))
    (encode-zero-string name bv 0 100)
    (encode-octal mode bv 100 108)
    (encode-octal uid bv 108 116)
    (encode-octal gid bv 116 124)
    (encode-octal size bv 124 136)
    (encode-octal mtime bv 136 148)
    (encode-octal chksum bv 148 156)
    (bytevector-u8-set! bv 156 typeflag)
    (encode-zero-string linkname bv 157 257)
    (encode-binary-string magic bv 257 263)
    (encode-binary-string version bv 263 265)
    (encode-zero-string uname bv 265 297)
    (encode-zero-string gname bv 297 329)
    (encode-octal devmajor bv 329 337)
    (encode-octal devminor bv 337 345)
    (encode-zero-string prefix bv 345 500)
    (encode-binary-string padding bv 500 512)
    bv))

(define self-extension-header?
  (let ((pax-global-extended-header (char->integer #\g)))
    (lambda (header)
      "Check if the tar header HEADER is an extension header that does
not extend another tar header but rather extends itself."
      (= (tar-header-typeflag header) pax-global-extended-header))))

(define extension-header?
  (let* ((pax-extended-header (char->integer #\x))
         (gnu-long-name (char->integer #\L))
         (gnu-long-link (char->integer #\K))
         (extension-headers (list pax-extended-header
                                  gnu-long-name
                                  gnu-long-link)))
    (lambda (header)
      "Check if the tar header HEADER is an extension header."
      (memv (tar-header-typeflag header) extension-headers))))

(define (tar-header-extension-typeflag header)
  (let* ((extension (tar-header-extension header)))
    (and extension
         (tar-header-typeflag (or (tar-extension-header extension)
                                  header)))))

(define (read-header-extension port header)
  (let* ((size (tar-header-size header))
         (typeflag (tar-header-typeflag header))
         (decode-content (typeflag-decoder typeflag))
         (content (decode-content (get-bytevector-n port size)))
         (remainder (modulo size 512))
         (padding (match (and (not (zero? remainder))
                              (get-bytevector-n port (- 512 remainder)))
                    (#f "")
                    ((? bytevector-zero?) "")
                    (bv (decode-binary-string bv)))))
    (values (make-tar-extension
             (and (not (self-extension-header? header))
                  (set-tar-header-data-padding header padding))
             content)
            padding)))

(define (write-extension-content port header content)
  (let* ((size (tar-header-size header))
         (typeflag (tar-header-typeflag header))
         (encode-content (typeflag-encoder typeflag))
         (content-bv (make-bytevector size))
         (remainder (modulo size 512))
         (padding-size (if (zero? remainder) 0 (- 512 remainder)))
         (padding-bv (make-bytevector padding-size 0))
         (data-padding (tar-header-data-padding header)))
    (encode-content content content-bv)
    (put-bytevector port content-bv)
    (encode-binary-string data-padding padding-bv)
    (put-bytevector port padding-bv)))

(define end-of-tarball-object (list))

(define (end-of-tarball-object? obj)
  (eq? obj end-of-tarball-object))

(define %zeros (make-bytevector 512 0))

(define (read-tar-header port)
  (let* ((bv (get-bytevector-n port 512))
         (zeros? (equal? %zeros bv))
         (next-bv (and zeros? (get-bytevector-n port 512))))
    (cond
     ((equal? next-bv %zeros) end-of-tarball-object)
     (else
      (when next-bv
        (unget-bytevector port next-bv))
      (let ((header (bytevector->tar-header bv)))
        (cond
         ((extension-header? header)
          (let* ((extension padding (read-header-extension port header))
                 (next-header (bytevector->tar-header
                               (get-bytevector-n port 512))))
            (set-tar-header-extension next-header extension)))
         ((self-extension-header? header)
          (let ((extension padding (read-header-extension port header)))
            (set-fields header
              ((tar-header-extension) extension)
              ((tar-header-data-padding) padding))
            (set-tar-header-extension header extension)))
         (else header)))))))

(define (write-tar-header port header)
  (match (tar-header-extension header)
    (#f (put-bytevector port (tar-header->bytevector header)))
    (($ <tar-extension> e-header content)
     (when e-header
       (put-bytevector port (tar-header->bytevector e-header))
       (write-extension-content port e-header content))
     (put-bytevector port (tar-header->bytevector header))
     (unless e-header
       (write-extension-content port header content)))))

(define -tar-header-
  (make-record-serializer
   make-tar-header
   `((name ,%tar-header-name ,-zero-string-)
     (mode ,%tar-header-mode ,-octal-)
     (uid ,%tar-header-uid ,-octal-)
     (gid ,%tar-header-gid ,-octal-)
     (size ,%tar-header-size ,-octal-)
     (mtime ,%tar-header-mtime ,-octal-)
     (chksum ,%tar-header-chksum ,-octal-)
     (typeflag ,tar-header-typeflag #f)
     (linkname ,%tar-header-linkname ,-zero-string-)
     (magic ,tar-header-magic ,-binary-string-)
     (version ,tar-header-version ,-binary-string-)
     (uname ,%tar-header-uname ,-zero-string-)
     (gname ,%tar-header-gname ,-zero-string-)
     (devmajor ,%tar-header-devmajor ,-octal-)
     (devminor ,%tar-header-devminor ,-octal-)
     (prefix ,%tar-header-prefix ,-zero-string-)
     (padding ,tar-header-padding ,-binary-string-)
     (data-padding ,tar-header-data-padding ,-binary-string-)
     (extension ,tar-header-extension ,(delay -tar-extension-)))
   #:elide-first-field? #t))

(define %default-default-tar-header
  (make-tar-header
   (make-zero-string #f "")
   (make-padded-octal #o644 7 #\0 "")
   (make-padded-octal 0 7 #\0 "")
   (make-padded-octal 0 7 #\0 "")
   (make-padded-octal 0 11 #\0 "")
   (make-padded-octal 0 11 #\0 "")
   (make-padded-octal #f 6 #\0 "\x00 ")
   (char->integer #\0)
   (make-zero-string "" "")
   "ustar\x00"
   "00"
   (make-zero-string "" "")
   (make-zero-string "" "")
   (make-padded-octal 0 7 #\0 "")
   (make-padded-octal 0 7 #\0 "")
   (make-zero-string "" "")
   ""
   #f
   #f))

(define (default-tar-header headers)
  (define all-fields (record-type-fields <tar-header>))
  (define field-counts
    (make-hash-table (length all-fields)))
  (define (count-field header field)
    (let* ((accessor (record-accessor <tar-header> field))
           (counts (hashq-ref field-counts field))
           (key (accessor header))
           (count (hash-ref counts key 0)))
      (hash-set! counts key (1+ count))))
  (define (field-mode field)
    (let ((counts (hashq-ref field-counts field)))
      (cdr (hash-fold (lambda (value count acc)
                        (match-let (((best-count . best-value) acc))
                          (if (> count best-count)
                              (cons count value)
                              acc)))
                      '(0 . #f)
                      counts))))
  (define (undefault-fields header)
    (let ((mtime (%tar-header-mtime header))
          (size (%tar-header-size header))
          (chksum (%tar-header-chksum header)))
      (set-fields header
        ((%tar-header-name zero-string-value) #f)
        ((%tar-header-mtime) (set-octal-value mtime 0))
        ((%tar-header-size) (set-octal-value size 0))
        ((%tar-header-chksum) (set-octal-value chksum #f)))))
  (for-each (lambda (field)
              (hashq-set! field-counts field
                          (make-hash-table (length headers))))
            all-fields)
  (for-each (lambda (header)
              (for-each (lambda (field)
                          (count-field header field))
                        all-fields))
            headers)
  (undefault-fields
   (apply make-tar-header
          (map field-mode (record-type-fields <tar-header>)))))
