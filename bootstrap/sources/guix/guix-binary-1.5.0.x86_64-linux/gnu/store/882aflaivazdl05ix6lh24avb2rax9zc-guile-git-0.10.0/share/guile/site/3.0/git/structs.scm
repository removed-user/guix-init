;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017, 2019-2021, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of Guile-Git.
;;;
;;; Guile-Git is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-Git is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

(define-module (git structs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (git configuration)
  #:use-module ((system foreign) #:select (null-pointer?
                                           %null-pointer
                                           bytevector->pointer
                                           make-pointer
                                           procedure->pointer
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           dereference-pointer
                                           pointer-address
                                           void
                                           (int . ffi:int)))
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (git-error? git-error-code git-error-message git-error-class pointer->git-error
            time->pointer pointer->time time-time time-offset
            signature->pointer pointer->signature signature-name signature-email signature-when
            oid? oid->pointer pointer->oid make-oid-pointer oid=?

            string-list->strarray

            diff-file? diff-file-oid diff-file-path diff-file-size diff-file-flags diff-file-mode diff-file-id-abbrev
            diff-binary-file? diff-binary-file-type diff-binary-file-data diff-binary-file-datalen diff-binary-file-inflatedlen

            diff-delta? diff-delta-status diff-delta-flags diff-delta-status diff-delta-nfiles diff-delta-old-file diff-delta-new-file
            pointer->diff-delta
            diff-binary? diff-binary-contains-data diff-binary-old-file diff-binary-new-file
            pointer->diff-binary
            diff-line? diff-line-origin diff-line-old-lineno diff-line-new-lineno diff-line-num-lines diff-line-content-len
            diff-line-content-offset diff-line-content pointer->diff-line
            diff-hunk? diff-hunk-old-start diff-hunk-old-lines diff-hunk-new-start diff-hunk-new-lines diff-hunk-header-len diff-hunk-header
            pointer->diff-hunk

            config-entry? pointer->config-entry
            config-entry-name config-entry-value config-entry-include-depth
            config-entry-level config-entry-backend-type
            config-entry-origin-path

            status-entry? status-entry-status status-entry-head-to-index status-entry-index-to-workdir pointer->status-entry

            make-status-options-bytestructure status-options->pointer set-status-options-show! set-status-options-flags!

            make-strarray strarray->pointer set-strarray-strings! set-strarray-count!

            make-remote-callbacks remote-callbacks->pointer set-remote-callbacks-version! set-remote-callbacks-transfer-progress!
            set-remote-callbacks-certificate-check!
            make-fetch-options-bytestructure fetch-options-bytestructure fetch-options->pointer fetch-options-remote-callbacks
            fetch-options-download-tags set-fetch-options-download-tags!
            fetch-options-depth set-fetch-options-depth!
            set-remote-callbacks-credentials!
            fetch-options-proxy-options

            indexer-progress?
            indexer-progress-total-objects
            indexer-progress-indexed-objects
            indexer-progress-received-objects
            indexer-progress-local-objects
            indexer-progress-total-deltas
            indexer-progress-indexed-deltas
            indexer-progress-received-bytes

            proxy-options?
            proxy-options-bytestructure proxy-options->pointer proxy-options-callbacks
            proxy-options-url proxy-options-type
            set-proxy-options-url! set-proxy-options-type!


            make-checkout-options-bytestructure checkout-options-bytestructure checkout-options->pointer
            make-clone-options-bytestructure clone-options-bytestructure clone-options->pointer set-clone-options-fetch-opts!
            submodule-update-options-bytestructure
            make-submodule-update-options-bytestructure
            submodule-update-options->pointer
            submodule-update-options-bytestructure
            set-submodule-update-options-allow-fetch?!
            set-submodule-update-options-fetch-options!

            make-describe-options-bytestructure describe-options->pointer describe-options->bytestructure
            set-describe-options-max-candidates-tag! set-describe-options-strategy!
            set-describe-options-pattern! set-describe-options-only-follow-first-parent!
            set-describe-options-show-commit-oid-as-fallback!

            make-describe-format-options-bytestructure describe-format-options->pointer describe-format-options->bytestructure
            set-describe-format-options-abbreviated-size!
            set-describe-format-options-always-use-long-format!
            set-describe-format-options-dirty-suffix!

            make-diff-options-bytestructure diff-options->pointer diff-options->bytestsructure
            set-diff-options-version!
            set-diff-options-flags!
            set-diff-options-ignore-submodules!
            set-diff-options-pathspec!
            set-diff-options-notify-cb!
            set-diff-options-progress-cb!
            set-diff-options-payload!
            set-diff-options-context-lines!
            set-diff-options-interhunk-lines!
            set-diff-options-id-abbrev!
            set-diff-options-max-size!
            set-diff-options-old-prefix!
            set-diff-options-new-prefix!

            remote-head? remote-head-local remote-head-oid remote-head-loid remote-head-name pointer->remote-head pointer->remote-head-list))


;;; bytestructures helper

(define bytestructure->pointer
  (compose bytevector->pointer bytestructure-bytevector))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure (pointer->bytevector pointer (bytestructure-descriptor-size struct))
                      0
                      struct))

;;; git-time

(define %time (bs:struct `((time ,int64)         ;time in seconds since epoch
                           (offset ,int)         ;timezone offset, in minutes
                           (sign ,int8))))       ;sign for "-0000" offsets

(define-record-type <time>
  (%make-time bytestructure)
  time?
  (bytestructure time-bytestructure))

(define (pointer->time pointer)
  (%make-time (pointer->bytestructure pointer %time)))

(define (time->pointer time)
  (bytestructure->pointer (time-bytestructure time)))

(define (time-time time)
  (bytestructure-ref (time-bytestructure time) 'time))

(define (time-offset time)
  (bytestructure-ref (time-bytestructure time) 'offset))

;;; git-signature

(define %signature (bs:struct `((name ,(bs:pointer uint8)) ;; char *
                                (email ,(bs:pointer uint8)) ;; char *
                                (when ,%time))))

(define-record-type <signature>
  (%make-signature bytestructure)
  signature?
  (bytestructure signature-bytestructure))

(define (pointer->signature pointer)
  (%make-signature (pointer->bytestructure pointer %signature)))

(define (signature->pointer signature)
  (bytestructure->pointer (signature-bytestructure signature)))

(define (signature-name signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'name))))

(define (signature-email signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'email))))

(define (signature-when signature)
  (let ((when* (bytestructure-ref (signature-bytestructure signature) 'when)))
    (%make-time when*)))

;;; git oid

(define-syntax GIT-OID-RAWSZ
  (identifier-syntax 20))

(define-record-type <oid>
  (%make-oid bytevector)
  oid?
  (bytevector oid-bytevector))

(define* (pointer->oid pointer #:optional (offset 0))
  ;; C functions typically return 'const git_oid *' and the OID's memory
  ;; belongs to the object it is associated with.  Thus, always copy the OID
  ;; contents to make sure it's not modified or freed behind our back.
  (%make-oid (bytevector-copy
              (pointer->bytevector pointer GIT-OID-RAWSZ offset))))

(define (oid->pointer oid)
  (bytevector->pointer (oid-bytevector oid)))

(define (make-oid-pointer)
  (bytevector->pointer (make-bytevector GIT-OID-RAWSZ)))

(define (oid=? oid1 oid2)
  "Return true if OID1 and OID2 are equal."
  ;; This is more efficient than calling 'git_oid_equal' through the FFI.
  (bytevector=? (oid-bytevector oid1)
                (oid-bytevector oid2)))

;;; git status options

(define %error
  (bs:struct `((message ,(bs:pointer uint8))
               (class   ,int))))

(define %strarray
  (bs:struct `((strings ,(bs:pointer
                          (bs:pointer uint8)))
               (count ,size_t))))

(define %status-options
  (bs:struct `((version ,unsigned-int)
               (status-show ,int)
               (flags ,unsigned-int)
               (pathspec ,%strarray)
               (baseline ,(bs:pointer void))
               ,@(if %have-status-options-rename-threshold?
                     `((rename-threshold ,uint16))
                     '()))))

(define %diff-file
  (bs:struct `((oid ,(bs:vector 20 uint8))
               (path ,(bs:pointer uint8))
               (size ,int64)
               (flags ,uint32)
               (mode ,uint16)
               (id-abbrev ,uint16))))

(define %diff-binary-file
  (bs:struct `((type ,int)
               (data ,(bs:pointer uint8))
               (datalen ,size_t)
               (inflatedlen ,size_t))))

(define %diff-delta
  (bs:struct `((status ,int)
               (flags ,uint32)
               (similarity ,uint16)
               (nfiles ,uint16)
               (old-file ,%diff-file)
               (new-file ,%diff-file))))

(define %diff-binary
  (bs:struct `((contains-data ,int)
               (old-file ,%diff-binary-file)
               (new-file ,%diff-binary-file))))

(define %status-entry
  (bs:struct `((status ,int)
               (head-to-index ,(bs:pointer %diff-delta))
               (index-to-workdir ,(bs:pointer %diff-delta)))))

(define %diff-line
  (bs:struct `((origin ,int8)
               (old-lineno ,int)
               (new-lineno ,int)
               (num-lines ,int)
               (content-len ,size_t)
               (content-offset ,int64)
               (content ,(bs:pointer int8))))) ;char *

(define %diff-hunk
  (bs:struct `((old-start ,int)
               (old-lines ,int)
               (new-start ,int)
               (new-lines ,int)
               (header-len ,size_t)
               (header ,(bs:vector 128 int8))))) ; char[128]

(define (flags->symbols flags map-list)
  (fold (lambda (flag-map symbols)
          (match flag-map
            ((flag symbol)
             (if (> (logand flag flags) 0)
                 (cons symbol symbols)
                 symbols))))
        '()
        map-list))

(define (status-names status)
  (flags->symbols status
                  '((1     index-new)
                    (2     index-modified)
                    (4     index-deleted)
                    (8     index-renamed)
                    (16    index-typechange)
                    (128   wt-new)
                    (256   wt-modified)
                    (512   wt-deleted)
                    (1024  wt-typechange)
                    (2048  wt-renamed)
                    (4096  wt-unreadable)
                    (16384 ignored)
                    (32768 conflicted))))

(define-record-type <git-error>
  (%make-git-error code message class)
  git-error?
  (code    git-error-code)
  (message git-error-message)
  (class   git-error-class))

(define-record-type <diff-file>
  (%make-diff-file oid path size flags mode id-abbrev)
  diff-file?
  (oid diff-file-oid)
  (path diff-file-path)
  (size diff-file-size)
  (flags diff-file-flags)
  (mode diff-file-mode)
  (id-abbrev diff-file-id-abbrev))

(define-record-type <diff-binary-file>
  (%make-diff-binary-file type data datalen inflatedlen)
  diff-binary-file?
  (type diff-binary-file-type)
  (data diff-binary-file-data)
  (datalen diff-binary-file-datalen)
  (inflatedlen diff-binary-file-inflatedlen))

(define-record-type <diff-delta>
  (%make-diff-delta status flags similarity nfiles old-file new-file)
  diff-delta?
  (status diff-delta-status)
  (flags diff-delta-flags)
  (similarity diff-delta-similarity)
  (nfiles diff-delta-nfiles)
  (old-file diff-delta-old-file)
  (new-file diff-delta-new-file))

(define-record-type <diff-binary>
  (%make-diff-binary contains-data old-file new-file)
  diff-binary?
  (contains-data diff-binary-contains-data)
  (old-file diff-binary-old-file)
  (new-file diff-binary-new-file))

(define-record-type <status-entry>
  (%make-status-entry status head-to-index index-to-workdir)
  status-entry?
  (status status-entry-status)
  (head-to-index status-entry-head-to-index)
  (index-to-workdir status-entry-index-to-workdir))

(define-record-type <diff-line>
  (%make-diff-line origin old-lineno new-lineno num-lines content-len content-offset content)
  diff-line?
  (origin diff-line-origin)
  (old-lineno diff-line-old-lineno)
  (new-lineno diff-line-new-lineno)
  (num-lines diff-line-num-lines)
  (content-len diff-line-content-len)
  (content-offset diff-line-content-offset)
  (content diff-line-content))

(define-record-type <diff-hunk>
  (%make-diff-hunk old-start old-lines new-start new-lines header-len header)
  diff-hunk?
  (old-start diff-hunk-old-start)
  (old-lines diff-hunk-old-lines)
  (new-start diff-hunk-new-start)
  (new-lines diff-hunk-new-lines)
  (header-len diff-hunk-header-len)
  (header diff-hunk-header))

(define-record-type <status-options>
  (%make-status-options bytestructure)
  status-options?
  (bytestructure status-options-bytestructure))

(define* (pointer->git-error pointer code)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %error)))
        (%make-git-error code
                         (pointer->string
                          (make-pointer (bytestructure-ref bs 'message)))
                         (bytestructure-ref bs 'class)))))

(define (make-status-options-bytestructure)
  (%make-status-options (bytestructure %status-options)))

(define (status-options->pointer status-options)
  (bytestructure->pointer (status-options-bytestructure status-options)))

(define (set-status-options-show! status-options show)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'status-show show))

(define (set-status-options-flags! status-options flags)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'flags flags))

(define (bs-diff-file->diff-file bs)
  (%make-diff-file
   (%make-oid (bytestructure-bytevector
               (bytestructure-ref bs 'oid)))
   (pointer->string
    (make-pointer (bytestructure-ref bs 'path)))
   (bytestructure-ref bs 'size)
   (bytestructure-ref bs 'flags)
   (bytestructure-ref bs 'mode)
   (bytestructure-ref bs 'id-abbrev)))

(define (bs-diff-binary-file->diff-binary-file bs)
  (%make-diff-binary-file
   (bytestructure-ref bs 'type)
   (let ((data (make-pointer (bytestructure-ref bs 'data))))
     (if (null-pointer? data)
         #()
         (pointer->bytevector (make-pointer data)
                              (bytestructure-ref bs 'datalen))))
   (bytestructure-ref bs 'datalen)
   (bytestructure-ref bs 'inflatedlen)))

(define (pointer->diff-delta pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-delta)))
        (%make-diff-delta
         (bytestructure-ref bs 'status)
         (bytestructure-ref bs 'flags)
         (bytestructure-ref bs 'similarity)
         (bytestructure-ref bs 'nfiles)
         (bs-diff-file->diff-file (bytestructure-ref bs 'old-file))
         (bs-diff-file->diff-file (bytestructure-ref bs 'new-file))))))

(define (pointer->diff-binary pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-binary)))
        (%make-diff-binary
         (bytestructure-ref bs 'contains-data)
         (bs-diff-binary-file->diff-binary-file (bytestructure-ref bs 'old-file))
         (bs-diff-binary-file->diff-binary-file (bytestructure-ref bs 'new-file))))))

(define (pointer->status-entry pointer)
  (let ((bs (pointer->bytestructure pointer %status-entry)))
    (%make-status-entry
     (status-names (bytestructure-ref bs 'status))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'head-to-index)))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'index-to-workdir))))))

(define (pointer->diff-line pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-line)))
        (%make-diff-line
         (bytestructure-ref bs 'origin)
         (bytestructure-ref bs 'old-lineno)
         (bytestructure-ref bs 'new-lineno)
         (bytestructure-ref bs 'num-lines)
         (bytestructure-ref bs 'content-len)
         (bytestructure-ref bs 'content-offset)
         (pointer->string
           (make-pointer (bytestructure-ref bs 'content))
           (bytestructure-ref bs 'content-len))))))

(define (pointer->diff-hunk pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-hunk)))
        (%make-diff-hunk
         (bytestructure-ref bs 'old-start)
         (bytestructure-ref bs 'old-lines)
         (bytestructure-ref bs 'new-start)
         (bytestructure-ref bs 'new-lines)
         (bytestructure-ref bs 'header-len)
         (let ((bv (bytestructure-bytevector (bytestructure-ref bs 'header)))
               (output (make-bytevector (bytestructure-ref bs 'header-len))))
           (bytevector-copy! bv 0 output 0 (bytestructure-ref bs 'header-len))
           output)))))

;; config entry

(define %config-entry
  (bs:struct `((name ,(bs:pointer uint8))         ;char *
               (value ,(bs:pointer uint8))        ;char *
               ,@(if %have-config-entry-backend-type?
                     `((backend-type ,(bs:pointer uint8)) ;char *
                       (origin-path ,(bs:pointer uint8))) ;char *
                     '())
               (include-depth ,unsigned-int)
               (level ,int)                       ;git_config_level_t
               ,@(if %have-config-entry-free?
                     `((free ,(bs:pointer int)))
                     '())
               ,@(if %have-config-entry-backend-type?
                     '()
                     `((payload ,(bs:pointer int))))))) ;removed in 1.8

(define-record-type <config-entry>
  (%make-config-entry name value backend-type origin-path
                      depth level)
  config-entry?
  (name   config-entry-name)
  (value  config-entry-value)
  (backend-type config-entry-backend-type)        ;#f | symbol
  (origin-path  config-entry-origin-path)         ;#f | string
  (depth  config-entry-include-depth)
  (level  config-entry-level))

(define (pointer->config-entry pointer)
  "Return a <config-entry> record based on the 'git_config_entry' struct
pointed to by POINTER.  The data pointed to by POINTER is not freed."
  (define bs (pointer->bytestructure pointer %config-entry))

  ;; Duplicate the structure POINTER refers to so that users can capture it
  ;; and pass it around.
  (%make-config-entry
   (pointer->string (make-pointer (bytestructure-ref bs 'name)))
   (pointer->string (make-pointer (bytestructure-ref bs 'value)))
   (and %have-config-entry-backend-type?
        (string->symbol
         (pointer->string
          (make-pointer (bytestructure-ref bs 'backend-type)))))
   (and %have-config-entry-backend-type?
        (pointer->string
         (make-pointer (bytestructure-ref bs 'origin-path))))
   (bytestructure-ref bs 'include-depth)
   (bytestructure-ref bs 'level)))

;; proxy options: https://libgit2.org/libgit2/#HEAD/type/git_proxy_options

(define %proxy-options
  (bs:struct `((version ,unsigned-int)            ;GIT-PROXY-OPTIONS-VERSION
               (type ,int)                        ;git_proxy_t enum
               (url ,(bs:pointer void))           ;string | NULL
               (credendials ,(bs:pointer void))   ;git_cred_acquire_cb *
                                                  ;git_transport_certificate_check_cb
               (transport-certificate-check-cb ,(bs:pointer void))
               (payload ,(bs:pointer void)))))

(define GIT-PROXY-OPTIONS-VERSION 1)   ;supported version--see <git2/proxy.h>

(define-record-type <proxy-options>
  (%make-proxy-options bytestructure)
  proxy-options?
  (bytestructure proxy-options-bytestructure))

(define (proxy-options->pointer proxy-options)
  (bytestructure->pointer (proxy-options-bytestructure proxy-options)))

(define %proxy-options-strings
  ;; This weak-key 'eq?' hash table maps <proxy-options> records to pointer
  ;; objects that must outlive them.
  (make-weak-key-hash-table))

(define (symbol->proxy-type symbol)
  "Convert SYMBOL to an integer of the 'git_proxy_t' enum."
  (match symbol
    ('none      0)
    ('auto      1)
    ('specified 2)))

(define (proxy-type->symbol type)
  "Convert INTEGER, a value of the 'git_proxy_t' enum, to a symbol."
  (match type
    (0 'none)
    (1 'auto)
    (2 'specified)))

(define (proxy-options-type proxy-options)
  "Return the proxy type, a symbol, specified in PROXY-OPTIONS."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options)))
    (proxy-type->symbol
     (bytestructure-ref proxy-options-bs 'type))))

(define (proxy-options-url proxy-options)
  "Return the proxy URL specified in PROXY-OPTIONS, or #f if there is none."
  (let* ((proxy-options-bs (proxy-options-bytestructure proxy-options))
         (ptr              (make-pointer
                            (bytestructure-ref proxy-options-bs 'url))))
    (and (not (null-pointer? ptr))
         (pointer->string ptr -1 "UTF-8"))))

(define (set-proxy-options-type! proxy-options type)
  "Change the type of proxy in PROXY-OPTIONS to TYPE, one of 'none (no
proxy), 'auto (auto-detect proxy), or 'specified (use the specified proxy
URL)."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options)))
    (bytestructure-set! proxy-options-bs 'type
                        (symbol->proxy-type type))))

(define (set-proxy-options-url! proxy-options url)
  "Set the proxy URL in PROXY-OPTIONS to URL.  Make sure to change the proxy
type to 'specified for this to take effect."
  (let ((proxy-options-bs (proxy-options-bytestructure proxy-options))
        (str              (and url (string->pointer url "UTF-8"))))
    (if str
        (begin
          ;; Make sure STR is not reclaimed before PROXY-OPTIONS-BS.
          (hashq-set! %proxy-options-strings proxy-options-bs str)
          (bytestructure-set! proxy-options-bs 'url (pointer-address str)))
        (bytestructure-set! proxy-options-bs 'url 0))))


;; git fetch options

(define %indexer-progress
  (bs:struct `((total-objects ,unsigned-int)
               (indexed-objects ,unsigned-int)
               (received-objects ,unsigned-int)
               (local-objects ,unsigned-int)
               (total-deltas ,unsigned-int)
               (indexed-deltas ,unsigned-int)
               (received-bytes ,size_t))))

(define-record-type <indexer-progress>
  (%make-indexer-progress total-objects indexed-objects
                          received-objects local-objects
                          total-deltas indexed-deltas
                          received-bytes)
  indexer-progress?
  (total-objects    indexer-progress-total-objects)
  (indexed-objects  indexer-progress-indexed-objects)
  (received-objects indexer-progress-received-objects)
  (local-objects    indexer-progress-local-objects)
  (total-deltas     indexer-progress-total-deltas)
  (indexed-deltas   indexer-progress-indexed-deltas)
  (received-bytes   indexer-progress-received-bytes))

(define (bytestructure->indexer-progress bs)
  "Return a copy of BS, an %INDEXER-PROGRESS bytestructure, as an
<indexer-progress> record."
  (let-syntax ((make (syntax-rules ()
                       ((_ field ...)
                        (%make-indexer-progress
                         (bytestructure-ref bs 'field) ...)))))
    (make total-objects indexed-objects
          received-objects local-objects
          total-deltas indexed-deltas
          received-bytes)))

(define %remote-callbacks
  (bs:struct `((version ,unsigned-int)
               (sideband-progress ,(bs:pointer uint8))
               (completion ,(bs:pointer uint8))
               (credentials ,(bs:pointer uint8))
               (certificate-check ,(bs:pointer uint8))
               (transfer-progress ,(bs:pointer uint8))
               (update-tips ,(bs:pointer uint8))
               (pack-progress ,(bs:pointer uint8))
               (push-transfer-progress ,(bs:pointer uint8))
               (push-update-reference ,(bs:pointer uint8))
               (push-negotiation ,(bs:pointer uint8))
               (transport ,(bs:pointer uint8))
               (remote-ready ,(bs:pointer void))
               (payload ,(bs:pointer uint8))
               (resolve-url ,(bs:pointer uint8))
               ,@(if %have-remote-callbacks-update-refs?
                     `((update-refs ,(bs:pointer uint8)))
                     '()))))

(define-record-type <remote-callbacks>
  (%make-remote-callbacks bytestructure)
  remote-callbacks?
  (bytestructure remote-callbacks-bytestructure))

(define REMOTE-CALLBACKS-VERSION 1)               ;<git2/remote.h>

(define (make-remote-callbacks)
  (let ((bs (bytestructure %remote-callbacks)))
    (bytestructure-set! bs 'version REMOTE-CALLBACKS-VERSION)
    (%make-remote-callbacks bs)))

(define (remote-callbacks->pointer remote-callbacks)
  (bytestructure->pointer (remote-callbacks-bytestructure remote-callbacks)))

(define (set-remote-callbacks-version! remote-callbacks version)
  (bytestructure-set! (remote-callbacks-bytestructure remote-callbacks) 'version version))

(define %fetch-options
  (bs:struct `((version ,int)
               (callbacks ,%remote-callbacks)
               (prune ,int)
               (update-fetchhead ,int)
               (download-tags ,int)
               (proxy-opts ,%proxy-options)
               ,@(if %have-fetch-options-depth?
                     `((depth ,int))     ;one of the GIT-FETCH-DEPTH-* values
                     '())
               ,@(if %have-fetch-options-follow-redirects?
                     `((follow-redirects ,int))
                     '())
               (custom-headers ,%strarray))))

(define GIT-FETCH-DEPTH-FULL 0)
(define GIT-FETCH-DEPTH-UNSHALLOW 2147483647)

(define-record-type <fetch-options>
  (%make-fetch-options bytestructure)
  fetch-options?
  (bytestructure fetch-options-bytestructure))

(define (make-fetch-options-bytestructure)
  (%make-fetch-options (bytestructure %fetch-options)))

(define (fetch-options->pointer fetch-options)
  (bytestructure->pointer (fetch-options-bytestructure fetch-options)))

(define (remote-autotag-option->symbol integer)
  "Convert INTEGER, a value of the 'git_remote_autotag_option_t' enum, to a
symbol."
  (case integer
    ((0) 'unspecified)          ;follow user configuration
    ((1) 'auto)                 ;tags for objects we're downloading
    ((2) 'none)                 ;don't ask for any tags
    ((3) 'all)                  ;ask for all the tags
    (else 'unknown)))

(define (symbol->remote-autotag-option symbol)
  "Convert SYMBOL to an integer of the 'git_remote_autotag_option_t' enum."
  (case symbol
    ((unspecified) 0)
    ((auto)        1)
    ((none)        2)
    ((all)         3)
    (else          1)))

(define (fetch-options-download-tags fetch-options)
  "Return the tag download policy specified by FETCH-OPTIONS.  The policy is
denoted by a symbol: 'unspecified means to follow the user configuration,
'auto to download tags for objects we're already downloading, 'none to not
ask for any tags, and 'all to ask for all the tags."
  (remote-autotag-option->symbol
   (bytestructure-ref (fetch-options-bytestructure fetch-options)
                      'download-tags)))

(define* (set-fetch-options-download-tags! fetch-options policy)
  "Use POLICY, a symbol (see 'fetch-options-download-tags'), as the download
tag policy in FETCH-OPTIONS."
  (bytestructure-set! (fetch-options-bytestructure fetch-options)
                      'download-tags
                      (symbol->remote-autotag-option policy)))

(define (fetch-depth->symbol depth)
  (cond ((= depth GIT-FETCH-DEPTH-UNSHALLOW) 'unshallow)
        (else 'full)))

(define (symbol->fetch-depth symbol)
  (case symbol
    ((unshallow) GIT-FETCH-DEPTH-UNSHALLOW)
    ((full)      GIT-FETCH-DEPTH-FULL)
    (else        GIT-FETCH-DEPTH-FULL)))

(define (fetch-options-depth fetch-options)
  "Return the \"depth\" parameter of FETCH-OPTIONS as a symbol: 'full or
'unshallow.  On libgit2 < 1.7, this is always 'full."
  (if %have-fetch-options-depth?
      (fetch-depth->symbol
       (bytestructure-ref (fetch-options-bytestructure fetch-options)
                          'depth))
      'full))

(define (set-fetch-options-depth! fetch-options depth)
  "Change FETCH-OPTIONS depth parameter to DEPTH, one of 'full (obtain the
full repository history), and 'unshallow (fetch missing data from a shallow
repository)."
  (when %have-fetch-options-depth?
    (bytestructure-set! (fetch-options-bytestructure fetch-options)
                        'depth (symbol->fetch-depth depth))))

(define fetch-options-remote-callbacks
  (let ((cache (make-weak-key-hash-table 20)))
    (lambda (fetch-options)
      "Return the <remote-callbacks> associated with FETCH-OPTIONS."
      ;; This cache ensures that the <remote-callbacks> record remains live
      ;; as long as FETCH-OPTIONS is live, which in turn allows
      ;; 'set-remote-callbacks-transfer-progress!' to have a similar
      ;; lifecycle hash table.
      (or (hashq-ref cache fetch-options)
          (let ((callbacks
                 (%make-remote-callbacks
                  (bytestructure-ref (fetch-options-bytestructure fetch-options)
                                     'callbacks))))
            (hashq-set! cache fetch-options callbacks)
            callbacks)))))

(define (set-remote-callbacks-credentials! callbacks credentials)
  (bytestructure-set! (remote-callbacks-bytestructure callbacks)
                      'credentials credentials))

(define (procedure->indexer-progress-callback proc)
  "Wrap PROC and return a pointer that can be used as a
'git_indexer_progress_cb' value."
  ;; https://libgit2.org/libgit2/#HEAD/group/callback/git_indexer_progress_cb
  (procedure->pointer ffi:int
                      (lambda (ptr _)
                        ;; Return a value less than zero to cancel the
                        ;; indexing or download.
                        (if (proc (bytestructure->indexer-progress
                                   (pointer->bytestructure ptr
                                                           %indexer-progress)))
                            0
                            -1))
                      '(* *)))


(define %weak-references
  ;; Weak references: values in this table must be kept live as long as their
  ;; key is live.
  (make-weak-key-hash-table 100))

(define (set-remote-callbacks-transfer-progress! callbacks proc)
  "Set PROC as a transfer-progress callback in CALLBACKS.  PROC will be
called periodically as data if fetched from the remote, with one argument: an
indexer progress record.  PROC can cancel the on-going transfer by returning
#f."
  (let ((ptr (procedure->indexer-progress-callback proc)))
    ;; Make sure the pointer object remains live for as long as CALLBACKS;
    ;; otherwise the underlying libffi closure could be finalized too early.
    (hashq-set! %weak-references callbacks ptr)
    (bytestructure-set! (remote-callbacks-bytestructure callbacks)
                        'transfer-progress
                        (pointer-address ptr))))

(define (set-remote-callbacks-certificate-check! callbacks proc)
  "Use PROC as the certificate check procedure in CALLBACKS.  PROC will be
called with two arguments: the host name and a boolean indicating whether
libgit2 thinks the certificate is valid; it must return true to indicate that
the program should proceed with the connection, #f to abort the connection
attempt."
  ;; https://libgit2.org/libgit2/#HEAD/group/callback/git_transport_certificate_check_cb
  (let ((ptr (procedure->pointer ffi:int
                                 (lambda (certificate valid host payload)
                                   ;; Note: In theory the callback can also
                                   ;; return a strictly positive number "to
                                   ;; indicate that the callback refused to
                                   ;; act and that the existing validity
                                   ;; determination should be honored".  That
                                   ;; extra value doesn't bring much compared
                                   ;; to return VALID as is, so we ignore it.
                                   (if (proc (pointer->string host)
                                             (not (= 0 valid)))
                                       0
                                       -1))
                                 `(* ,ffi:int * *))))
    (hashq-set! %weak-references callbacks ptr)
    (bytestructure-set! (remote-callbacks-bytestructure callbacks)
                        'certificate-check (pointer-address ptr))))


(define (fetch-options-proxy-options fetch-options)
  "Return the <proxy-options> record associated with FETCH-OPTIONS."
  (let ((bs (fetch-options-bytestructure fetch-options)))
    (%make-proxy-options (bytestructure-ref bs 'proxy-opts))))


;; git checkout options

(define %checkout-options
  (bs:struct `((version ,unsigned-int)
               (checkout-strategy ,unsigned-int)
               (disable-filters ,int)
               (dir-mode ,unsigned-int)
               (file-mode ,unsigned-int)
               (file-open-flags ,int)
               (notify-flags ,unsigned-int)
               (notify-cb ,(bs:pointer uint8))
               (notify-payload ,(bs:pointer uint8))
               (progress-cb ,(bs:pointer uint8))
               (progress-payload ,(bs:pointer uint8))
               (paths ,%strarray)
               (baseline ,(bs:pointer uint8))
               (baseline-index ,(bs:pointer uint8))
               (target-directory ,(bs:pointer uint8))
               (ancestor-label ,(bs:pointer uint8))
               (our-label ,(bs:pointer uint8))
               (their-label ,(bs:pointer uint8))
               (perfdata-cb ,(bs:pointer uint8))
               (perfdata-payload ,(bs:pointer uint8)))))

(define-record-type <checkout-options>
  (%make-checkout-options bytestructure)
  checkout-options?
  (bytestructure checkout-options-bytestructure))

(define (make-checkout-options-bytestructure)
  (%make-checkout-options (bytestructure %checkout-options)))

(define (checkout-options->pointer checkout-options)
  (bytestructure->pointer (checkout-options-bytestructure checkout-options)))



;; git clone options

(define %clone-options
  (bs:struct `((version ,int)
               (checkout-opts ,%checkout-options)
               (fetch-opts ,%fetch-options)
               (bare ,int)
               (local ,int)
               (checkout-branch ,(bs:pointer uint8))
               (repository-cb ,(bs:pointer uint8))
               (repository-cb-payload ,(bs:pointer uint8))
               (remote-cb ,(bs:pointer uint8))
               (remote-cb-payload ,(bs:pointer uint8)))))

(define-record-type <clone-options>
  (%make-clone-options bytestructure)
  clone-options?
  (bytestructure clone-options-bytestructure))

(define (make-clone-options-bytestructure)
  (%make-clone-options (bytestructure %clone-options)))

(define (clone-options->pointer clone-options)
  (bytestructure->pointer (clone-options-bytestructure clone-options)))

(define (set-clone-options-fetch-opts! clone-options fetch-options)
  (let ((clone-options-bs (clone-options-bytestructure clone-options))
        (fetch-options-bs (fetch-options-bytestructure fetch-options)))
    (bytestructure-set! clone-options-bs 'fetch-opts
                        (bytestructure-bytevector fetch-options-bs))))


;; submodule update options

(define %submodule-update-options
  (bs:struct `((version ,unsigned-int)
               (checkout-options ,%checkout-options)
               (fetch-options ,%fetch-options)
               (allow-fetch? ,int))))

(define-record-type <submodule-update-options>
  (%make-submodule-update-options bytestructure)
  submodule-update-options?
  (bytestructure submodule-update-options-bytestructure))

(define (make-submodule-update-options-bytestructure)
  (%make-submodule-update-options (bytestructure %submodule-update-options)))

(define (submodule-update-options->pointer options)
  (bytestructure->pointer (submodule-update-options-bytestructure options)))

(define (set-submodule-update-options-allow-fetch?! submodule-update-options
                                                    allow-fetch?)
  (let ((bs (submodule-update-options-bytestructure submodule-update-options)))
    (bytestructure-set! bs 'allow-fetch? (if allow-fetch? 1 0))))

;; TODO: set-submodule-update-options-checkout-options!

(define (set-submodule-update-options-fetch-options! submodule-update-options
                                                     fetch-options)
  (let ((bs (submodule-update-options-bytestructure submodule-update-options))
        (fetch-options-bs (fetch-options-bytestructure fetch-options)))
    (bytestructure-set! bs 'fetch-options
                        (bytestructure-bytevector fetch-options-bs))

    ;; FETCH-OPTIONS might contain things like pointers coming from
    ;; 'procedure->pointer' (callbacks), which should be protected from GC as
    ;; long as SUBMODULE-UPDATE-OPTIONS is live.
    (hashq-set! %weak-references submodule-update-options fetch-options)))


;; git remote head

(define %remote-head
  (bs:struct `((local ,int)
               (oid ,(bs:vector GIT-OID-RAWSZ uint8))
               (loid ,(bs:vector GIT-OID-RAWSZ uint8))
               (name ,(bs:pointer uint8))
               (symref-target ,(bs:pointer uint8)))))

(define-record-type <remote-head>
  (%make-remote-head local oid loid name symref-target)
  remote-head?
  (local remote-head-local)
  (oid remote-head-oid)
  (loid remote-head-loid)
  (name remote-head-name)
  (symref-target remote-head-symref-target))

(define (pointer->remote-head pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %remote-head)))
        (%make-remote-head
         (bytestructure-ref bs 'local)
	 (pointer->oid pointer (bytestructure-offset
				(bytestructure-ref bs 'oid)))
	 (pointer->oid pointer (bytestructure-offset
				(bytestructure-ref bs 'loid)))
         (pointer->string (make-pointer (bytestructure-ref bs 'name)))
         (bytestructure-ref bs 'symref-target)))))

(define (pointer->pointer-list ptr length)
  (let ((size (sizeof '*)))
    (let ((main (pointer->bytevector ptr (* size length))))
        (let loop ((count 0)
                   (acc '()))
          (if (= count length)
              (reverse acc)
              (let* ((offset (* count size))
                     (next-ptr (bytevector->pointer main offset)))
                (loop (+ count 1)
                      (cons (dereference-pointer next-ptr) acc))))))))

(define (pointer->remote-head-list ptr length)
   (map pointer->remote-head
        (pointer->pointer-list ptr length)))

;;; git describe options

(define %describe-options
  (bs:struct `((version ,unsigned-int)
               (max-candidates-tag ,unsigned-int)
               (describe-strategy ,unsigned-int)
               (pattern ,(bs:pointer uint8)) ;char *
               (only-follow-first-parent ,int)
               (show-commit-oid-as-fallback ,int))))

(define-record-type <describe-options>
  (%make-describe-options bytestructure)
  describe-options?
  (bytestructure describe-options-bytestructure))

(define (make-describe-options-bytestructure)
  (%make-describe-options (bytestructure %describe-options)))

(define (describe-options->pointer options)
  (bytestructure->pointer (describe-options-bytestructure options)))

(define (set-describe-options-max-candidates-tag! options max-candidates)
  (bytestructure-set! (describe-options-bytestructure options)
                      'max-candidates-tag max-candidates))

(define (set-describe-options-strategy! options strategy)
  (bytestructure-set! (describe-options-bytestructure options)
                      'describe-strategy strategy))

(define (set-describe-options-pattern! options pattern)
  (bytestructure-set! (describe-options-bytestructure options)
                      'pattern (pointer-address pattern)))

(define (set-describe-options-only-follow-first-parent! options only-follow-first-parent?)
  (bytestructure-set! (describe-options-bytestructure options)
                      'only-follow-first-parent only-follow-first-parent?))

(define (set-describe-options-show-commit-oid-as-fallback! options fallback-to-oid?)
  (bytestructure-set! (describe-options-bytestructure options)
                      'show-commit-oid-as-fallback fallback-to-oid?))

;;; git describe format options

(define %describe-format-options
  (bs:struct `((version ,unsigned-int)
               (abbreviated-size ,unsigned-int)
               (always-use-long-format ,int)
               (dirty-suffix ,(bs:pointer uint8))))) ;char *

(define-record-type <describe-format-options>
  (%make-describe-format-options bytestructure)
  describe-format-options?
  (bytestructure describe-format-options-bytestructure))

(define (describe-format-options->pointer format-options)
  (bytestructure->pointer (describe-format-options-bytestructure
                           format-options)))

(define (make-describe-format-options-bytestructure)
  (%make-describe-format-options (bytestructure %describe-format-options)))

(define (set-describe-format-options-abbreviated-size! format-options abbreviated-size)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'abbreviated-size abbreviated-size))

(define (set-describe-format-options-always-use-long-format! format-options
                                                             always-use-long-format?)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'always-use-long-format always-use-long-format?))

(define (set-describe-format-options-dirty-suffix! format-options dirty-suffix)
  (bytestructure-set! (describe-format-options-bytestructure format-options)
                      'dirty-suffix (pointer-address dirty-suffix)))

;; git diff options

(define %diff-options
  (bs:struct `((version ,unsigned-int)
               (flags ,uint32)
               (ignore-submodules ,int) ;git_submodule_ignore_t
               (pathspec ,%strarray) ;git_strarray
               (notify-cb ,(bs:pointer void)) ;git_diff_notify_cb
               (progress-cb ,(bs:pointer void)) ;git_diff_progress_cb
               (payload ,(bs:pointer void))
               (context-lines ,uint32)
               (interhunk-lines ,uint32)
               ,@(if %have-diff-options-oid-type?
                     `((oid-type ,int))           ;git_oid_t
                     '())
               (id-abbrev ,uint16)
               (max-size ,int64) ;git_off_t
               (old-prefix ,(bs:pointer uint8)) ;char*
               (new-prefix ,(bs:pointer uint8))))) ;char*

(define-record-type <diff-options>
  (%make-diff-options bytestructure)
  diff-options?
  (bytestructure diff-options-bytestructure))

(define (diff-options->pointer options)
  (bytestructure->pointer (diff-options-bytestructure options)))

(define (make-diff-options-bytestructure)
  (%make-diff-options (bytestructure %diff-options)))

(define (set-diff-options-version! options version)
  (bytestructure-set! (diff-options-bytestructure options)
                      'version version))

(define (set-diff-options-flags! options flags)
  (bytestructure-set! (diff-options-bytestructure options)
                      'flags flags))

(define (set-diff-options-ignore-submodules! options ignore-submodules)
  (bytestructure-set! (diff-options-bytestructure options)
                      'ignore-submodules ignore-submodules))

(define (set-diff-options-pathspec! options pathspec)
  (bytestructure-set! (diff-options-bytestructure options)
                      'pathspec pathspec))

(define (set-diff-options-notify-cb! options notify-cb)
  (bytestructure-set! (diff-options-bytestructure options)
                      'notify-cb notify-cb))

(define (set-diff-options-progress-cb! options progress-cb)
  (bytestructure-set! (diff-options-bytestructure options)
                      'progress-cb progress-cb))

(define (set-diff-options-payload! options payload)
  (bytestructure-set! (diff-options-bytestructure options)
                      'payload payload))

(define (set-diff-options-context-lines! options context-lines)
  (bytestructure-set! (diff-options-bytestructure options)
                      'context-lines context-lines))

(define (set-diff-options-interhunk-lines! options interhunk-lines)
  (bytestructure-set! (diff-options-bytestructure options)
                      'interhunk-lines))

(define (set-diff-options-id-abbrev! options id-abbrev)
  (bytestructure-set! (diff-options-bytestructure options)
                      'id-abbrev id-abbrev))

(define (set-diff-options-max-size! options max-size)
  (bytestructure-set! (diff-options-bytestructure options)
                      'max-size max-size))

(define (set-diff-options-old-prefix! options old-prefix)
  (bytestructure-set! (diff-options-bytestructure options)
                      'old-prefix old-prefix))

(define (set-diff-options-new-prefix! options new-prefix)
  (bytestructure-set! (diff-options-bytestructure options)
                      'new-prefix new-prefix))

;; git_str_array used in various function calls

(define-record-type <strarray>
  (%make-strarray bytestructure
                  pointers)
  strarray?
  (bytestructure strarray-bytestructure)
  ;; The pointers field is used to synchronize the life-time of
  ;; underlying pointer objects with the life-time of the related
  ;; strarray object.
  (pointers      strarray-pointers
                 set-strarray-pointers!))

(define (make-strarray)
  (%make-strarray (bytestructure %strarray)
                  '()))

(define (strarray->pointer strarray)
  (bytestructure->pointer (strarray-bytestructure strarray)))

(define (set-strarray-strings! strarray string-list)
  (let* ((strarray-bs (strarray-bytestructure strarray))
         (pointers '())
         (strings-pointer (if (null? string-list)
                              %null-pointer
                              (bytevector->pointer
                               (uint-list->bytevector
                                (map (lambda (string)
                                       (let ((pointer (string->pointer string)))
                                         (set! pointers (cons pointer
                                                              pointers))
                                         (pointer-address pointer)))
                                     string-list)
                                (native-endianness)
                                (sizeof '*))))))
    (unless (null-pointer? strings-pointer)
      (set! pointers (cons strings-pointer pointers)))
    (bytestructure-set! strarray-bs
                        'strings (pointer-address
                                  strings-pointer))
    (set-strarray-pointers! strarray pointers)))

(define (set-strarray-count! strarray count)
  (bytestructure-set! (strarray-bytestructure strarray)
                      'count count))

(define (string-list->strarray string-list)
  "Return a copy of STRING-LIST, a list of strings, as an STRARRAY
record."
  (let ((strarray (make-strarray))
        (count (length string-list)))
    (set-strarray-count! strarray count)
    (set-strarray-strings! strarray string-list)
    strarray))
