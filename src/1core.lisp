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
(in-package :remlic)

;; Note: Dont use CLOS for simple dispatching

(define-symbol-macro %%validate-input%%
    (progn
      (when (and val-input (null val-output))
        (error "val-output is missing while val-input is provided"))
      (normalize-regularization)
      (setf input (reshape input '(t -1)))
      (setf output (reshape output '(t -1)))
      (when val-input
        (setf val-input (reshape val-input '(t -1)))
        (setf val-output (reshape val-output '(t -1))))))

(defun %run-evaluation (evaluate r input output val-input val-output verbose)
  (if evaluate
      (values r
              (evaluate r input output :verbose verbose :batch t)
              (when val-input
                (evaluate r val-input val-output :verbose verbose :batch t)))
      r))

(defun mlic-core (input output rules
                  &key
                    (regularizers '(noise-regularizer))
                    (class 'cnf)
                    (evaluate t)
                    verbose
                    timelimit
                    val-input
                    val-output
                    (solver-args '(:maxsat-competition :year 2017 :track :complete :name :maxhs))
                    &allow-other-keys)
  %%validate-input%%
  (check-type input (array bit 2))
  (check-type output (array bit 2))
  (check-type rules fixnum)
  (when verbose
    (format t "~&Training: ~a -> ~a" (shape input) (shape output))
    (when val-input
      (format t "~&Test    : ~a -> ~a" (shape val-input) (shape val-output))))
  
  (trivial-timeout:with-timeout (timelimit)
    (let ((r (make-instance class :rules rules)))
      (train r input output
             :verbose verbose
             :val-input val-input
             :val-output val-output
             :regularizers regularizers
             :solver-args solver-args)
      (%run-evaluation evaluate r input output val-input val-output verbose))))



