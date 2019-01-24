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

(defun iris-dataset ()
  (let* ((dd (get-r-dataset-directory))
         (iris (asarray (dataset-points
                         (get-dataset dd "datasets" "iris"
                                      :csv-type-spec '(integer
                                                       single-float
                                                       single-float
                                                       single-float
                                                       single-float
                                                       string)))))
         (iris (shuffle iris))
         (features (asarray (aref iris t '(1 5)) :type 'single-float))
         (str-class    (aref iris t -1))
         (categories (remove-duplicates str-class :test 'string=))
         (int-class (map-array (lambda (s)
                                 (position s categories :test 'string=))
                               str-class))

         (max (amax features :axes 0))
         (min (amin features :axes 0))
         (diff (- max min))
         (thresholds (+ (expand-dims min 1) (* (arange 0 1 1/10) (reshape diff '(t 1)))))
         (input (< thresholds (expand-dims features 2)))
         (output (= (expand-dims int-class 1) (arange (length categories)))))
    (list
     (aref features '(0 4))
     ;; str-class
     ;; categories
     (aref int-class '(0 4))
     ;; max
     ;; min
     ;; diff
     thresholds
     (aref input '(0 4))
     (aref output '(0 4))
     )
    (values input output)
    ))

(defun iris-multinomial-test (sample-num &optional (method 'mlic))
  (multiple-value-bind (input output) (iris-dataset)
    (print
     (multiple-value-list
      (funcall method
               (aref input `(0 ,sample-num))
               (aref output `(0 ,sample-num))
               4
               :verbose t)))))

(test :remlic-multinomial
  ;; (finishes (iris-multinomial-test 30))
  ;; (finishes (iris-multinomial-test 60))
  ;; (finishes (iris-multinomial-test 90))
  ;; (finishes (iris-multinomial-test 120))
  (finishes (iris-multinomial-test 140)))


