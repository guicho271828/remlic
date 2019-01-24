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

(in-package :remlic.test)

(in-suite :remlic)

(defparameter *r* (make-binary-random-rule 30))

(defun generate-test-data (n f noise-ratio &key (rule *r*) posonly)
  (let* ((input  (zeros (list (* n 2) f) :type 'bit))
         (output (zeros (* n 2)          :type 'bit)))

    (iter (for i below (* 2 n))
          (when (if posonly
                    (or (evenp i) (< i n))
                    (evenp i))
            (setf (aref output i) 1)
            (iter (for clause in rule)
                  ;; Turn some bits in a CNF clause (== disjunction) true.
                  ;; Ensure that at least one bit turns true.

                  (for values =
                       (iter (for tmp = (bernoulli 1/3 (length clause)))
                             (while (every #'zerop tmp))
                             (finally
                              (return tmp))))
                  
                  (iter (for j in clause)
                        (for v in-vector values)
                        (setf (aref input i j) v)))))

    ;; flip when noise=1
    ;; input noise
    ;;     1     1 -> 0
    ;;     0     1 -> 1
    ;;     1     0 -> 1
    ;;     0     0 -> 0
    (bit-xor (flatten input)
             (bernoulli noise-ratio (* n 2 f))
             (flatten input))

    (bit-xor (flatten output)
             (bernoulli noise-ratio (* n 2))
             (flatten output))
    
    (format t "~&rule: ~a~%" rule)
    (list (aref input  (list 0 n))
          (aref output (list 0 n))
          (aref input  (list n t))
          (aref output (list n t)))))


(defun test-mlic (dataset k &key (method 'mlic))
  (destructuring-bind (train-input train-output val-input val-output) dataset
    (funcall method
             train-input
             train-output
             k
             :verbose t
             :timelimit 30
             :val-input val-input
             :val-output val-output)))

(test :remlic-binary-30
  (let ((dataset (generate-test-data 30 30 0.1)))
    (finishes (test-mlic dataset 2))
    (finishes (test-mlic dataset 6))
    (finishes (test-mlic dataset 12))))

