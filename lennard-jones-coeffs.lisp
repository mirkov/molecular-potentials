;; Mirko Vukovic
;; Time-stamp: <2011-10-20 10:06:48 lennard-jones-coeffs.lisp>
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

;; Lenard-Jones coefficients are used for calculating the Omega-xx
;; collision integrals.  The coefficients are read from a file and
;; stored in *lennard-jones-coeffs*.  They can be created as objects
;; using (make-lennard-jones-6/12-potential species &optional species2).
;;
;; Naming convention:
;; lj -- lennard-jones
;; coeffs -- coefficients

(export '(lennard-jones-6/12-potential
	  make-lennard-jones-6/12-potential
	  make-species-lennard-jones-6/12-potential
	  *lennard-jones-6/12-coeffs*
	  *test-lennard-jones-6/12-coeffs*))

(defparameter *data-directory*
  (asdf:system-source-directory :molecular-potentials)
  "Path to the data directory")

(defun read-data-file (file &optional (directory *data-directory*))
  "Return contents of `file' in `directory'

Signal error if file not found"
  (with-input-from-file (stream
			 (merge-pathnames file
					  directory)
			 :if-does-not-exist :error)
    (read stream)))
(defun read-lennard-jones-6/12-coeffs (&optional (file "lennard-jones-coeffs.dat"))
  "Read Lennard-Jones coefficients table"
  (read-data-file file))


(defparameter *lennard-jones-6/12-coeffs* (read-lennard-jones-6/12-coeffs)
  "table of Lennard Jones coefficients")

(defun species-lennard-jones-6/12-coeffs (species
			     &optional (db *lennard-jones-6/12-coeffs*))
  "return Lennard-Jones coefficients for `species'"
  (cdr (assoc species db)))

(defparameter *test-lennard-jones-6/12-coeffs*
  '((:X (:m 1.0) (:sigma 2.0) (:epsilon/K 4.0))
    (:Y (:m 2.0) (:sigma 3.0) (:epsilon/K 5.0)))
  "lennard-jones parameters for unit testing")

(defmacro with-test-coeffs (&body body)
  `(let* ((*lennard-jones-6/12-coeffs* *test-lennard-jones-6/12-coeffs*))
     ,@body))

(defclass lennard-jones-6/12-potential (molecular-potential)
  ()
  (:documentation "Lennard-Jones coefficients for a species

Units are NOT SI:
- mass in AMU
- sigma in Angstrom"))

(defmethod print-object ((self lennard-jones-6/12-potential) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when (slot-boundp self 'species)
      (species self))))

(defmethod describe-object ((self lennard-jones-6/12-potential) stream)
  (format stream "Lennard-Jones 6/12 potential")
  (describe-slot self "Species" stream)
  (describe-slot self "Mass" stream)
  (describe-slot self "Sigma" stream)
  (describe-slot self "epsilon/K" stream))

(defun make-lennard-jones-6/12-potential (mass sigma epsilon/K
					  &optional species)
  "Create object of Lennard-Jones 6-12 potential with coefficients
`mass', `sigma', and `epsilon/K'.  Optionally, also store the
`species' info"
  (if species
      (make-instance 'lennard-jones-6/12-potential
		     :species species
		     :mass mass
		     :sigma sigma
		     :epsilon/K epsilon/K)
      (make-instance 'lennard-jones-6/12-potential
		     :mass mass
		     :sigma sigma
		     :epsilon/K epsilon/K)))
  
(defun make-species-lennard-jones-6/12-potential (species)
  "Create object of Lennard-Jones 6-12 potential for `species'

`species' is a keyword, for a species stored in the file
*lennard-jones-6/12-coeffs* in the *data-directory*"
  (let ((coeffs (species-lennard-jones-6/12-coeffs species)))
    (make-instance 'lennard-jones-6/12-potential
		   :species species
		   :mass (cadr (assoc :m coeffs))
		   :sigma (cadr (assoc :sigma coeffs))
		   :epsilon/k (cadr (assoc :epsilon/k coeffs)))))

(define-test make-lennard-jones-6/12-potential
  (let* ((*lennard-jones-6/12-coeffs* *test-lennard-jones-6/12-coeffs*)
	 (lj-coeffs (make-lennard-jones-6/12-potential :X)))
    (assert-number-equal 1.0 (mass lj-coeffs))
    (assert-number-equal 2.0 (sigma lj-coeffs))
    (assert-number-equal 4.0 (epsilon/k lj-coeffs))))

