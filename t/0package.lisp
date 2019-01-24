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
(uiop:define-package :remlic.test
  (:use)
  (:mix :numcl :remlic
        :fiveam
        :clml.hjs.read-data
        :clml.data.r-datasets
        :cl-maxsat :trivia :alexandria :iterate))
(in-package :remlic.test)



(def-suite :remlic)
(in-suite :remlic)

;; run test with (run! test-name) 

(test basic
  (finishes
   (print
    (multiple-value-list
     (mlic (let ((r (uniform 0 2 '(12 14) 'bit)))
             (setf (aref r '(0 12 2) 0) 1
                   (aref r '(1 12 2) 0) 0)
             (print r)
             r)
           (let ((r (zeros 12 :type 'bit)))
             (setf (aref r '(0 12 2)) 1)
             (print r)
             r)
           1
           :verbose t)))))

(defun make-binary-random-rule (f)
  (assert (cl:<= 9 f))
  (let (acc)
    (iter (while (cl:< (length acc) 9))
          (pushnew (random f) acc))
    (symbol-macrolet ((r (pop acc)))
      (list (list r r r)
            (list r r r)
            (list r r r)))))



