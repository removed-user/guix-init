;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007, 2023 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-Avahi.
;;;
;;; Guile-Avahi is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-Avahi is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (avahi client publish)
  :use-module (avahi client)
  :use-module ((avahi) #:select (%avahi-extension))
  :export (entry-group? make-entry-group
           free-entry-group! freed-entry-group?
           commit-entry-group reset-entry-group! entry-group-state
           empty-group-empty? entry-group-client add-entry-group-service!
           add-entry-group-service-subtype! update-entry-group-service!
           add-entry-group-address!

           alternative-service-name alternative-host-name

           entry-group-state->string
           entry-group-state/uncommited entry-group-state/registering
           entry-group-state/established entry-group-state/collision
           entry-group-state/failure

           publish-flag->string
           publish-flag/unique publish-flag/no-probe publish-flag/no-announce
           publish-flag/allow-multiple publish-flag/no-reverse
           publish-flag/no-cookie publish-flag/update
           publish-flag/use-wide-area publish-flag/use-multicast))

(unless (getenv "AVAHI_GUILE_CROSS_COMPILING")
  (load-extension %avahi-extension "scm_avahi_publish_init"))

;;; arch-tag: 36180c98-3262-40a6-a90c-eb8f283e628e
