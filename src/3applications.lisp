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




(defun mlic (input output rules &rest args
             &key
               ((:accuracy *accuracy*) *accuracy*)
               ((:sparsity *sparsity*) *sparsity*)
               ((:diversity *diversity*) *diversity*) ;no longer used oneshot
               ((:unlabeled *unlabeled-accuracy*) *unlabeled-accuracy*)
               (evaluate t)
               oneshot verbose val-input val-output &allow-other-keys)
  
  %%validate-input%%

  (flet ((call (output &optional verbose)
           (apply #'mlic-core input (reshape output '(t -1)) rules
                  :evaluate nil         ;don't evaluate 
                  :verbose verbose
                  args)))

    (let ((r (if oneshot
                 (call output verbose)    ;use the same verbosity
                 (reduce #'merge-rules
                         (mapcar #'call   ;cancel verbosity
                                 (unstack output :axis -1))))))
      (%run-evaluation evaluate r input output val-input val-output verbose))))



(defun mlic-iterative (input output rules
                       &rest args
                       &key
                         verbose
                         (initial-samples 10)
                         (max-samples (first (shape input)))
                         (callback #'validation-above-90)
                         (by #'*2)
                         (timelimit 900)
                         (method 'mlic)
                         (evaluate t)
                         val-input
                         val-output
                         &allow-other-keys)

  %%validate-input%%
  
  (let (best-r best-val-accuracy stopcriteria)
    (handler-case
        (trivial-timeout:with-timeout (timelimit)
          (iter (for samples initially initial-samples then (funcall by samples))
                (while (< samples max-samples))
                (when (and verbose (not (first-iteration-p)))
                  (format t "~&Increasing #samples to ~a" samples))
                (for i from 1)
                (for (values r train-accuracy val-accuracy)
                     = (apply method
                              (aref input `(0 ,samples))
                              (aref output `(0 ,samples))
                              rules 
                              :verbose nil
                              :timelimit nil
                              :val-input  (aref input `(,samples)) ;validation set
                              :val-output (aref output `(,samples)) ;validation set
                              args))
                
                (when verbose
                  (format t "~&Val accuracy current ~10,,,f best ~10,,,f" val-accuracy best-val-accuracy))
                (when (or (first-iteration-p)
                          (< best-val-accuracy val-accuracy))
                  (when verbose
                    (format t "--> ~10,,,f" val-accuracy))
                  (setf best-r r best-val-accuracy val-accuracy))
                
                (until (when (funcall callback i r train-accuracy val-accuracy)
                         (setf stopcriteria :callback)))))
      (trivial-timeout:timeout-error (c)
        (format t "~&Timed out!~%")
        (setf stopcriteria :timeout)
        (when (null best-r)
          (signal c))))

    (multiple-value-bind (r train test)
        (%run-evaluation evaluate best-r input output val-input val-output verbose)
      (values r train test stopcriteria))))

