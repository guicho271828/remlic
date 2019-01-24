#|

This file is a part of REMLIC project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

REMLIC is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

REMLIC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
REMLIC.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(uiop:define-package remlic
  (:use)
  (:mix :numcl 
        :trainable-object
        :alexandria
        :closer-mop
        :cl-maxsat :trivia :iterate)
  (:reexport :trainable-object)
  (:export

   rule

   cnf 
   dnf 
   eqv 
   cnf+
   dnf+
   eqv+
   
   eqv2
   eqv2+

   cnf-chg
   eqv-chg
   cnf+-chg
   eqv+-chg
   
   rule-shape
   constraints
   extract
   merge-rules/2
   
   ;; api
   mlic
   mlic-core
   mlic-iterative
   
   *accuracy*
   *sparsity*
   *diversity*
   *unlabeled-accuracy*

   PU-noise-regularizer
   diversity-regularizer
   sparsity-regularizer
   noise-regularizer
   
   validation-above-90
   validation-above-95
   validation-above-97))
(in-package :remlic)

;; blah blah blah.

#|
TODO:
+ preprocessing the real/finite-domain valued input/output vector to a bit vector.
  + MLIC paper proposed one-hot / threshold-based direct encoding method,
  + i.e. make a binary variable that encodes the fact x = v \in R,
  + but a better alternative is an ordered encoding: a boolean variable for x<=v \in R [Tamura 2009].
  + SAT encoding has several variants.

+ transforming the resulting rule into several forms:
  + multi-argument function
  + single argument function that takes a vector
  + single argument function that takes a matrix dataset
|#

(defun *2 (x) (* 2 x))
(defun *1.3 (x) (round (* 1.3 x)))

(defun validation-above-90 (iteration r train-accuracy val-accuracy)
  (declare (ignore r))
  (format t "~&Iteration ~4,,,a: train: ~10,,,A val: ~10,,,a" iteration train-accuracy val-accuracy)
  (< 0.9 val-accuracy))

(defun validation-above-95 (iteration r train-accuracy val-accuracy)
  (declare (ignore r))
  (format t "~&Iteration ~4,,,a: train: ~10,,,A val: ~10,,,a" iteration train-accuracy val-accuracy)
  (< 0.95 val-accuracy))

(defun validation-above-97 (iteration r train-accuracy val-accuracy)
  (declare (ignore r))
  (format t "~&Iteration ~4,,,a: train: ~10,,,A val: ~10,,,a" iteration train-accuracy val-accuracy)
  (< 0.97 val-accuracy))
