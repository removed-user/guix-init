;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007, 2008  Ludovic Courtès <ludo@gnu.org>
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

(define-module (avahi client)
  :use-module (avahi)
  :export (client? make-client client-server-version
           client-host-name client-host-fqdn client-state

           client-state->string
           client-state/s-registering client-state/s-running
           client-state/s-collision client-state/failure
           client-state/connecting

           client-flag->string
           client-flag/ignore-user-config client-flag/no-fail))

(unless (getenv "AVAHI_GUILE_CROSS_COMPILING")
  (load-extension %avahi-extension "scm_avahi_client_init"))

;; Optional bindings, depending on the configuration.
(if (defined? 'set-client-host-name!) (export set-client-host-name!))


;;; arch-tag: 9dc3916b-12a3-4fde-aa91-95ee0969d8bf
