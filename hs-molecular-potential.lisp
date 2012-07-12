;; Mirko Vukovic
;; Time-stamp: <2011-09-12 13:38:47 hs-molecular-potential.lisp>
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
(export '(hard-sphere-potential
	  make-hard-sphere-potential))

(defclass hard-sphere-potential (molecular-potential)
  ()
  (:documentation "Hard sphere potential for a species

By definition, it's epsilon/K = 0 since this potential does not have
an attractive component.

Units are NOT SI:
- mass in AMU
- sigma in Angstrom"))

(defmethod make-instance :after ((self hard-sphere-potential) &key)
  (setf (slot-value self 'epsilon/K) 0d0))

(defmethod print-object ((self hard-sphere-potential) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when (slot-boundp self 'species)
      (species self))))

(defmethod describe-object ((self hard-sphere-potential) stream)
  (format stream "Hard-sphere potential")
  (describe-slot self "Species" stream)
  (describe-slot self "Mass" stream)
  (describe-slot self "Sigma" stream))

(defun make-hard-sphere-potential (mass diameter &optional species)
  "Create object of hard-sphere potential coefficients for
`species'."
  (if species
      (make-instance 'hard-sphere-potential
		     :species species
		     :mass mass
		     :sigma diameter)
      (make-instance 'hard-sphere-potential
		     :mass mass
		     :sigma diameter)))



(defmethod V ((Radius float) r &rest rest)
  (declare (ignore rest))
  (if (> r radius)
      0.0
      most-positive-short-float))

(defmethod V ((self hard-sphere-potential) r &rest rest)
  (declare (ignore rest))
  (V (sigma self) r))

(define-test V-hs
  (assert-number-equal 0 (V pi 3.2))
  (assert-true (not (zerop (V pi 3))))
  (let ((hsp (make-hard-sphere-potential 3.2 :species :x :mass 28)))))
