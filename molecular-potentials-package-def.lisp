;; Mirko Vukovic
;; Time-stamp: <2011-09-12 09:51:08 molecular-potentials-package-def.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :molecular-potentials
  (:use :cl :lisp-unit)
  (:import-from :alexandria
		:with-input-from-file
		:symbolicate)
  (:documentation "Provide objects that specify molecular potential
  parameters and methods to calculate the potentials"))