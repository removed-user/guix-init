;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (git configuration)
  #:export (%libgit2
            %have-status-options-rename-threshold?
            %have-fetch-options-follow-redirects?
            %have-fetch-options-depth?
            %have-diff-options-oid-type?
            %have-config-entry-backend-type?
            %have-config-entry-free?
            %have-remote-callbacks-update-refs?
            %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
            %have-GIT_OPT_SET_HOMEDIR?
            %have-GIT_OPT_SET_USER_AGENT_PRODUCT?))

(define %libgit2
  "/gnu/store/fig6cd70970gg3mblim6wkdmwzk6w493-libgit2-1.9.1/lib/libgit2")

(define %have-status-options-rename-threshold?
  ;; True if 'git_status_options' has a 'rename_threshold' field.
  #true)

(define %have-fetch-options-follow-redirects?
  ;; True if 'git_fetch_options' has a 'follow_redirects' field.
  #true)

(define %have-fetch-options-depth?
  #true)

(define %have-diff-options-oid-type?
  #true)

(define %have-config-entry-backend-type?
  #true)

(define %have-config-entry-free?
  #false)

(define %have-remote-callbacks-update-refs?
  #true)

(define %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
  #true)

(define %have-GIT_OPT_SET_HOMEDIR?
  #true)

(define %have-GIT_OPT_SET_USER_AGENT_PRODUCT?
  #true)
