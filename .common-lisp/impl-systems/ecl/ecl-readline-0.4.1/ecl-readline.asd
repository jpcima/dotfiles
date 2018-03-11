;;;; -*- Lisp -*-
;;;;
;;;; ecl-readline.asd
;;;;
;;;; Copyright (C) 2008,2010 Jason Aquilina <jfa7600@yahoo.com.au>
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;;

;;;;
;;;; Readline support for ECL
;;;;
;;;; Ideas and code stolen from:
;;;;    CL-READLINE by Nikodemus Siivola <nikodemus@random-state.net>
;;;;    SB-READLINE by Miles Egan <miles@caddr.com>
;;;;

(asdf:defsystem ecl-readline
    :description "Readline support for ECL"
    :version "0.4.1"
    :author "Jason Aquilina"
    :components ((:file "ecl-readline")
		 (:file "ecl-completions" :depends-on ("ecl-readline"))))

;;;; End of File