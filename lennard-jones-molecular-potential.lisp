;; Mirko Vukovic
;; Time-stamp: <2011-09-10 23:13:52 lj-molecular-potential.lisp>
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

(defmethod V ((lj-coeffs lennard-jones-6/12-potential) r &rest rest)
  (declare (ignore rest))
  (with-slots (epsilon/k sigma) lj-coeffs
    (let ((sigma/r^6 (expt (/ sigma r) 6)))
    (* 4.0 epsilon/k (- (expt sigma/r^6 2)
			sigma/r^6)))))

(define-test V-LJ
  ;; verifies defining characteristics of the potential
  (with-test-coeffs
    (let ((ljc (make-lennard-jones-6/12-potential :X)))
      ;; zero crossing at r=sigma
      (assert-number-equal 0.0 (V ljc (sigma ljc)))
      ;; potential minimum = -epsilon/k at r = sigma * 2^1/6
      (assert-number-equal (- (epsilon/k ljc)) (V ljc (* (expt 2 (/ 6)) (sigma ljc)))))))