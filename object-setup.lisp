;; Mirko Vukovic
;; Time-stamp: <2011-09-12 13:37:39 object-setup.lisp>
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

(in-package :molecular-potentials)
(export '(V molecular-potential
	  species mass sigma epsilon/k doc))

(defgeneric V (specification r &rest rest)
  (:documentation "Calculate potential at distance `r'

Specifications is either a scalar for a hard-sphere potential or an
object that specifies the potential type (such as
hard-sphere-potential or Lennard-Jones-6/12-potential).

Currently, the hard-sphere potential can be specified by either a
scalar, or by its object.

Optional parameters can be used to specify other potential dependent
parameters"))


(defclass molecular-potential ()
  ((species :initarg :species
	    :reader species
	    :documentation "Optional string specifying the species")
   (mass :initarg :mass
	 :reader mass
	 :documentation "Species mass, in AMU")
   (sigma :initarg :sigma
	  :reader sigma
	  :documentation "Species cross-section in Angstrom")
   (epsilon/K :initarg :epsilon/K
	      :reader epsilon/K
	      :documentation "Potential-well depth, in Kelvin")
   (doc :initarg :doc
	:reader doc
	:documentation "Document data origin and other comments"))
  (:documentation "Base class for storing parameters of molecular potentials"))


(defun describe-slot (object slot stream)
  "Describe `object's `slot'

`slot' is a string whose uppercase symbol is the slot name

Output is sent to `stream'

Intended for use with `describe-object'"
  (let ((sym (symbolicate (string-upcase slot))))
    (when (slot-boundp object sym)
      (format stream "~&~a: ~a" slot (slot-value object sym)))))

