;
;   Copyright 2011 Arnaud Betremieux <arno@arnoo.net>
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(in-package :cl-user)

(defpackage :clutch-asd
  (:use :cl :asdf))

(in-package :clutch-asd)

(defsystem :clutch
  :name "clutch"
  :maintainer "Arnaud Betremieux"
  :licence "GPL"
  :serial t
  :version 0.1
  :components ((:file "clutch"))
  :depends-on (:cl-ppcre
               :ironclad
               :uuid
               :flexi-streams
               #-abcl :cl-store
               #-abcl :drakma
               #+sbcl :sb-posix
               #+sbcl :closer-mop)
  )
