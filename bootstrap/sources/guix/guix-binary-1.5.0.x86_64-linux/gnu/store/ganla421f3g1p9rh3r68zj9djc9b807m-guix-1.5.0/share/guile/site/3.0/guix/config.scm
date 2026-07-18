;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2016, 2018-2019, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix config)
  #:export (%guix-package-name
            %guix-version
            %guix-bug-report-address
            %guix-home-page-url

            %channel-metadata

            %storedir
            %localstatedir
            %sysconfdir

            %store-directory
            %state-directory
            %store-database-directory
            %config-directory

            %system
            %git
            %gzip
            %bzip2
            %xz))

;;; Commentary:
;;;
;;; Compile-time configuration of Guix.  When adding a substitution variable
;;; here, make sure to equip (guix scripts pull) to substitute it.
;;;
;;; Code:

(define %guix-package-name
  "GNU Guix")

(define %guix-version
  "1.5.0")

(define %guix-bug-report-address
  "https://codeberg.org/guix/guix/issues/")

(define %guix-home-page-url
  "https://www.gnu.org/software/guix/")

(define %channel-metadata
  ;; When true, this is an sexp containing metadata for the 'guix' channel
  ;; this file was built from.  This is used by (guix describe).
  (let ((url    "https://git.guix.gnu.org/guix.git")
        (commit "230aa373f315f247852ee07dff34146e9b480aec")
        (intro  '("9edb3f66fd807b096b48283debdcddccfea34bad" . "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))
    (and url commit
         `(repository
           (version 0)
           (url ,url)
           (branch "master")                      ;XXX: doesn't really matter
           (commit ,commit)
           (name guix)
           ,@(if intro
                 `((introduction
                    (channel-introduction
                     (version 0)
                     (commit ,(car intro))
                     (signer ,(cdr intro)))))
                 '())))))

(define %storedir
  "/gnu/store")

(define %localstatedir
  "/var")

(define %sysconfdir
  "/etc")

(define %store-directory
  (or (and=> (getenv "NIX_STORE_DIR") canonicalize-path)
      %storedir))

(define %state-directory
  ;; This must match `NIX_STATE_DIR' as defined in `nix/local.mk'.
  (or (getenv "GUIX_STATE_DIRECTORY")
      (string-append %localstatedir "/guix")))

(define %store-database-directory
  (or (getenv "GUIX_DATABASE_DIRECTORY")
      (string-append %state-directory "/db")))

(define %config-directory
  ;; This must match `GUIX_CONFIGURATION_DIRECTORY' as defined in `nix/local.mk'.
  (or (getenv "GUIX_CONFIGURATION_DIRECTORY")
      (string-append %sysconfdir "/guix")))

(define %system
  "x86_64-linux")

(define %git
  "/gnu/store/xz7xygq040vx78snmla70y03h6x9yypg-git-minimal-2.52.0/bin/git")

(define %gzip
  "/gnu/store/ch4v61a0lw0f1hkm5adai5z42qpyf20k-gzip-1.14/bin/gzip")

(define %bzip2
  "/gnu/store/3m35da59fr3wwdy0imzi2jg29jd6lhlz-bzip2-1.0.8/bin/bzip2")

(define %xz
  "/gnu/store/gd41pn8r5723cyd6d79q9riqjbi070zz-xz-5.4.5/bin/xz")

;;; config.scm ends here
