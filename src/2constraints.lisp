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

;; base

(defun b (&rest args)
  (let ((b (var (format nil "~{~A~^-~}" args) "B")))
    (set b args)
    b))
(defun n (&rest args)
  (var (format nil "~{~A~^-~}" args) "N"))
(defun y (&rest args)
  (var (format nil "~{~A~^-~}" args) "Y"))

(defvar *accuracy* 1)
(defvar *sparsity* 1)
(defvar *diversity* 0)
(defvar *unlabeled-accuracy* 1)
(defun normalize-regularization ()
  (let* ((nonzero (remove 0 (list *accuracy*
                                  *sparsity*
                                  *diversity*
                                  *unlabeled-accuracy*)))
         (min (reduce #'min nonzero)))
    (setf (values *accuracy*
                  *sparsity*
                  *diversity*
                  *unlabeled-accuracy*)
          (values (round *accuracy* min)
                  (round *sparsity* min)
                  (round *diversity* min)
                  (round *unlabeled-accuracy* min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GF

(defgeneric rule-shape    (rule))
(defgeneric constraints   (rule input output)
  (:documentation "A generic function that returns a MaxSAT hard constraints"))
(defgeneric extract       (rule trues)
  (:documentation "A generic function that extracts the rule from the solution of MaxSAT query"))
(defgeneric merge-rules/2 (rule1 rule2)
  (:documentation "A generic function that merges two rules of the same class
into a rule that output a concatenation of the individual output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(defun missing (what class) (error "missing arg ~a while initializing a slot of ~A" what class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rule (trainable-object)
    ((rules :initarg :rules :initform (missing :rules 'rule))
     (input-shape  :initarg :input-shape )
     (output-shape :initarg :output-shape)
     (matrix       :initarg :matrix      :type array))
    ))

(defclass cnf    (rule) () )
(defclass dnf    (rule) () )
(defclass eqv    (rule) () 
  (:documentation "
a MLIC class that uses both the positive and the negative input.
In contrast, CNF class only uses the fact that certain feature is true."))
(defclass cnf+   (rule) () 
  (:documentation "a CNF subclass that is able to model the constant truth."))
(defclass dnf+   (rule) () 
  (:documentation "a DNF subclass that is able to model the constant truth."))
(defclass eqv+   (rule) () 
  (:documentation "an EQV subclass that is able to model the constant truth."))

(defclass eqv2    (eqv) () 
  (:documentation "
A variation of EQV without the hard constraint that prunes the conflicting rules.
Don't use this for production, only for the evaluation purpose."))
(defclass eqv2+   (eqv+) () 
  (:documentation "
A variation of EQV+ without the hard constraint that prunes the conflicting rules.
Don't use this for production, only for the evaluation purpose."))

(defclass cnf-chg (cnf) () 
  (:documentation "A CNF rule that models the change from the input."))
(defclass cnf+-chg (cnf+) () 
  (:documentation "A CNF+ rule that models the change from the input."))
(defclass eqv-chg (eqv) () 
  (:documentation "A EQV rule that models the change from the input."))
(defclass eqv+-chg (eqv+) () 
  (:documentation "A EQV+ rule that models the change from the input."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods

;;; initialization

;; (defmethod initialize-instance :before ((rule rule) &key &allow-other-keys))
;; (defmethod initialize-instance :after  ((rule rule) &key &allow-other-keys))

(defmethod train ((rule rule) input output &key regularizers solver-args &allow-other-keys)
  (ematch rule
    ((rule :input-shape  (place i)
           :output-shape (place o)
           :matrix       (place m))
     (setf i (rest (shape input))
           o (rest (shape output))
           m (zeros (rule-shape rule) :type 'bit))
     (let ((cnf  `(and ,@(constraints rule input output)))
           (wcnf (iter (for c in regularizers)
                       (appending
                        (funcall c rule input output)))))
       (multiple-value-bind (trues)
           (apply #'solve cnf (first solver-args) :soft-clauses wcnf (rest solver-args))
         (extract rule trues)))))
  rule)

(defmethod evaluate ((rule rule) input output &key batch verbose)
  (mean
   (amin
    (= output
       (predict rule input))
    :axes 1)))

;;; rule size

(defmethod extract ((rule rule) (trues list))
  (ematch rule
    ((rule :matrix m)
     (dolist (true trues)
       (when (boundp true)
         (setf (apply #'aref m (symbol-value true)) 1))))))

;;; constraints

(defmethod rule-shape ((rule cnf))
  (ematch rule
    ((rule input-shape output-shape rules)
     (list (reduce #'* output-shape)
           rules
           (reduce #'* input-shape)))))

(defmethod constraints ((rule cnf) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (when (= 1 (aref input i m))
                                                   ;; otherwise constantly 0
                                                   (collecting
                                                    (b k l m))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod predict ((rule cnf) input &key batch &allow-other-keys)
  (ematch rule
    ((rule matrix)
     ;; matrix: [output, rule, feature]
     (let* ((input (reshape input '(-1 1 1 t)))   ; [batch, 1, 1, feature]
            (res (logand input matrix))           ; [batch, out, rule, feature]
            (res (amax res :axes 3))              ; [batch, out, rule]
            (res (amin res :axes 2))              ; [batch, out]
            )
       res))))

(defmethod logical-form ((rule cnf) &key (map-fn #'identity))
  (ematch rule
    ((rule matrix)
     (iter (for a_i in (unstack matrix))
           (collecting
            (iter (for a_ij in (unstack a_i))
                  (when (first-iteration-p)
                    (collect 'and))
                  (collecting
                   (iter (for a_ijk in-vector a_ij with-index k)
                         (when (first-iteration-p)
                           (collect 'or))
                         (when (= 1 a_ijk)
                           (collect (funcall map-fn k)))))))))))


(defmethod rule-shape ((rule cnf+))
  (ematch rule
    ((rule input-shape output-shape rules)
     (list (reduce #'* output-shape)
           rules
           (1+ (reduce #'* input-shape))))))

(defmethod constraints ((rule cnf+) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for l below rules)
                 (iter (for m below in-features)
                       (in outer
                           (collecting
                            ;; not allowing the constant and
                            ;; a fluent feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l in-features)
                                       ,(b k l m)))))))
           
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ;; to allow the constant truth
                                         ,(b k l in-features)
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (when (= 1 (aref input i m))
                                                   ;; otherwise constantly 0
                                                   (collecting
                                                    (b k l m))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod predict ((rule cnf+) input &key batch &allow-other-keys)
  (ematch rule
    ((rule matrix)
     ;; matrix: [output, rule, feature+1]
     (let* ((input (reshape input '(-1 1 1 t)))       ; [batch, 1, 1, feature]
            (matrix-main  (aref matrix t t '(0 -2)))  ; [output, rule, feature]
            (matrix-const (aref matrix t t -1))       ; [output, rule]
            (res (logand input matrix-main))          ; [batch, out, rule, feature]
            (res (amax res :axes 3))                  ; [batch, out, rule]
            (res (max matrix-const res))              ; max [output, rule], [batch, out, rule] -> [batch, out, rule]
            (res (amin res :axes 2))                  ; [batch, out]
            )
       res))))

(defmethod logical-form ((rule cnf+) &key (map-fn #'identity))
  (ematch rule
    ((rule matrix :input-shape (list in-features))
     (iter (for a_i in (unstack matrix))
           (collecting
            (iter (for a_ij in (unstack a_i))
                  (when (first-iteration-p)
                    (collect 'and))
                  (unless (= 1 (aref a_ij in-features)) ; if true, skip because constant truth
                    (collecting
                     (iter (for a_ijk in-vector a_ij
                                ;; do not allow iterating to the extra index for the constant
                                with-index k below in-features)
                           (when (first-iteration-p)
                             (collect 'or))
                           (when (= 1 a_ijk)
                             (collect (funcall map-fn k))))))))))))



(defmethod rule-shape ((rule eqv))
  (ematch rule
    ((rule input-shape output-shape rules)
     (list (reduce #'* output-shape)
           rules
           (* 2 (reduce #'* input-shape))))))

(defmethod constraints ((rule eqv) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for l below rules)
                 (iter (for m below in-features)
                       (in outer
                           (collecting
                            ;; not allowing a positive and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (+ m in-features))))))))
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ in-features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod predict ((rule eqv) input &key batch &allow-other-keys)
  (ematch rule
    ((rule matrix)
     ;; matrix: [output, rule, feature+1]
     (let* ((input (reshape input '(-1 1 1 t)))       ; [batch, 1, 1, feature*2]
            (f (fourth (shape input)))                ;
            (matrix-pos (aref matrix t t `(0 ,f)))    ; [output, rule, feature]
            (matrix-neg (aref matrix t t `(,f -1)))   ; [output, rule, feature]
            (res+ (logand   matrix-pos input))        ; [batch, out, rule, feature]
            (res- (logandc2 matrix-neg input))        ; [batch, out, rule, feature]
            (res+ (amax res+ :axes 3))                ; [batch, out, rule]
            (res- (amax res- :axes 3))                ; [batch, out, rule]
            (res  (max res+ res-))                    ; [batch, out, rule]
            (res  (amin res :axes 2))                 ; [batch, out]
            )
       res))))

(defmethod logical-form ((rule eqv) &key (map-fn #'identity))
  (ematch rule
    ((rule matrix input-shape)
     (let ((in-features (reduce #'* input-shape)))
       (iter (for a_i in (unstack matrix))
             (collecting
              (iter (for a_ij in (unstack a_i))
                    (when (first-iteration-p)
                      (collect 'and))
                    (collecting
                     (iter (for a_ijk in-vector a_ij with-index k)
                           (when (first-iteration-p)
                             (collect 'or))
                           (when (= 1 a_ijk)
                             (collecting
                              (if (< k in-features)
                                  (funcall map-fn k)
                                  (funcall map-fn (lognot (- k in-features)))))))))))))))


(defmethod rule-shape ((rule eqv+))
  (ematch rule
    ((rule input-shape output-shape rules)
     (list (reduce #'* output-shape)
           rules
           (1+ (* 2 (reduce #'* input-shape)))))))

(defmethod constraints ((rule eqv+) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for l below rules)
                 (iter (for m below in-features)
                       (in outer
                           (collecting
                            ;; not allowing the constant and
                            ;; a positive feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (* 2 in-features)))))
                           (collecting
                            ;; not allowing the constant and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l (+ m in-features))
                                       ,(b k l (* 2 in-features)))))
                           (collecting
                            ;; not allowing a positive and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (+ m in-features))))))))
           (iter (for i below samples)
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ;; to allow the constant truth
                                         ,(b k l (* 2 in-features))
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ in-features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod predict ((rule eqv+) input &key batch &allow-other-keys)
  (ematch rule
    ((rule matrix)
     ;; matrix: [output, rule, feature+1]
     (let* ((input (reshape input '(-1 1 1 t)))       ; [batch, 1, 1, feature*2]
            (f (fourth (shape input)))                ;
            (matrix-pos (aref matrix t t `(0 ,f)))    ; [output, rule, feature]
            (matrix-neg (aref matrix t t `(,f -2)))   ; [output, rule, feature]
            (matrix-const (aref matrix t t -1))       ; [output, rule]
            (res+ (logand   matrix-pos input))        ; [batch, out, rule, feature]
            (res- (logandc2 matrix-neg input))        ; [batch, out, rule, feature]
            (res+ (amax res+ :axes 3))                ; [batch, out, rule]
            (res- (amax res- :axes 3))                ; [batch, out, rule]
            (res (max matrix-const res))              ; max [output, rule], [batch, out, rule] -> [batch, out, rule]
            (res  (max res+ res-))                    ; [batch, out, rule]
            (res  (amin res :axes 2))                 ; [batch, out]
            )
       res))))

(defmethod logical-form ((rule eqv+) &key (map-fn #'identity))
  (ematch rule
    ((rule matrix input-shape)
     (let ((in-features (reduce #'* input-shape)))
       (iter (for a_i in (unstack matrix))
             (collecting
              (iter (for a_ij in (unstack a_i))
                    (when (first-iteration-p)
                      (collect 'and))
                    ;; if true, skip because constant truth
                    (unless (= 1 (aref a_ij (* 2 in-features)))
                      (collecting
                       (iter (for a_ijk in-vector a_ij
                                  ;; do not allow iterating to the extra index for the constant
                                  with-index k below (* 2 in-features))
                             (when (first-iteration-p)
                               (collect 'or))
                             (when (= 1 a_ijk)
                               (collecting
                                (if (< k in-features)
                                    (funcall map-fn k)
                                    (funcall map-fn (lognot (- k in-features))))))))))))))))


(defmethod constraints ((rule eqv2) input output)
  "Constraint for the original MLIC rule plus constant truth"
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ in-features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod constraints ((rule eqv2+) input output)
  "Constraint for the original MLIC rule plus constant truth"
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ;; to allow the constant truth
                                         ,(b k l (* 2 in-features))
                                         ,@(iter (for m below in-features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ in-features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))


(defmethod rule-shape ((rule dnf))
  (ematch rule
    ((rule input-shape output-shape rules)
     (list (reduce #'* output-shape)
           rules
           (reduce #'* input-shape)))))

(defmethod constraints ((rule dnf) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples in-features))
      (array :dimensions (list (eq samples) out-features)))
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[in-features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(or ,@(iter (for l below rules)
                                     (collecting
                                      `(and
                                        ,@(iter (for m below in-features)
                                                ;; X_im and B_lm.
                                                (when (= 1 (aref input i m))
                                                  ;; otherwise constantly 0
                                                  (collecting
                                                   (b k l m))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod predict ((rule dnf) input &key batch &allow-other-keys)
  (ematch rule
    ((rule matrix)
     ;; matrix: [output, rule, feature]
     (let* ((input (reshape input '(-1 1 1 t)))   ; [batch, 1, 1, feature]
            (res (logand input matrix))           ; [batch, out, rule, feature]
            (res (amin res :axes 3))              ; [batch, out, rule]
            (res (amax res :axes 2))              ; [batch, out]
            )
       res))))

(defmethod logical-form ((rule dnf) &key (map-fn #'identity))
  (ematch rule
    ((rule matrix)
     (iter (for a_i in (unstack matrix))
           (collecting
            (iter (for a_ij in (unstack a_i))
                  (when (first-iteration-p)
                    (collect 'or))
                  (collecting
                   (iter (for a_ijk in-vector a_ij with-index k)
                         (when (first-iteration-p)
                           (collect 'and))
                         (when (= 1 a_ijk)
                           (collect (funcall map-fn k)))))))))))



(defmethod constraints ((rule cnf-chg) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples features))
      (array :dimensions (list (eq samples) (eq features))))
     (iter outer
           (for k below features)
           (iter (for i below samples)
                 (when (= (aref input i k) (aref output i k))
                   (next-iteration))
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ,@(iter (for m below features)
                                                 ;; X_im and B_lm.
                                                 (when (= 1 (aref input i m))
                                                   ;; otherwise constantly 0
                                                   (collecting
                                                    (b k l m))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod constraints ((rule cnf+-chg) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples features))
      (array :dimensions (list (eq samples) (eq features))))
     (iter outer
           (for k below features)
           (iter (for l below rules)
                 (iter (for m below features)
                       (in outer
                           (collecting
                            ;; not allowing the constant and
                            ;; a fluent feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l features)
                                       ,(b k l m)))))))
           (iter (for i below samples)
                 (when (= (aref input i k) (aref output i k))
                   (next-iteration))
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ;; to allow the constant truth
                                         ,(b k l features)
                                         ,@(iter (for m below features)
                                                 ;; X_im and B_lm.
                                                 (when (= 1 (aref input i m))
                                                   ;; otherwise constantly 0
                                                   (collecting
                                                    (b k l m))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod constraints ((rule eqv-chg) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples features))
      (array :dimensions (list (eq samples) (eq features))))
     (iter outer
           (for k below features)
           (iter (for l below rules)
                 (iter (for m below features)
                       (in outer
                           (collecting
                            ;; not allowing a positive and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (+ m features))))))))
           (iter (for i below samples)
                 (when (= (aref input i k) (aref output i k))
                   (next-iteration))
                 ;; (not n_i) -> (y_i <-> (forall 1<=l<=k: X_i or B_l))
                 ;; Note that (X_i or B_l) means (exists 1<=m<=[features]: X_im and B_lm), which is very weird but ~\(ツ)/~
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ,@(iter (for m below features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

(defmethod constraints ((rule eqv+-chg) input output)
  (ematch* (rule input output)
    (((rule :rules rules)
      (array :dimensions (list samples features))
      (array :dimensions (list (eq samples) (eq features))))
     (iter outer
           (for k below features)
           (iter (for l below rules)
                 (iter (for m below features)
                       (in outer
                           (collecting
                            ;; not allowing the constant and
                            ;; a positive feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (* 2 features)))))
                           (collecting
                            ;; not allowing the constant and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l (+ m features))
                                       ,(b k l (* 2 features)))))
                           (collecting
                            ;; not allowing a positive and
                            ;; a negative feature detector to
                            ;; be used at the same time
                            `(not (and ,(b k l m)
                                       ,(b k l (+ m features))))))))
           (iter (for i below samples)
                 (when (= (aref input i k) (aref output i k))
                   (next-iteration))
                 (let ((y
                        `(and ,@(iter (for l below rules)
                                      (collecting
                                       `(or
                                         ;; to allow the constant truth
                                         ,(b k l (* 2 features))
                                         ,@(iter (for m below features)
                                                 ;; X_im and B_lm.
                                                 (collecting
                                                  (if (= 1 (aref input i m))
                                                      ;; otherwise constantly 0
                                                      (b k l m)
                                                      (b k l (+ features m)))))))))))

                   (in outer
                       (collect  `(or ,(n k i)
                                      ,(if (= 1 (aref output i k)) ;true
                                           y
                                           `(not ,y)))))))))))

;;; merging

(defun merge-rules (&rest rules)
  (match rules
    (nil nil)
    ((list it) it)
    (_
     (reduce #'merge-rules/2 rules))))

(defmethod merge-rules/2 ((r1 rule) (r2 rule))
  (assert (eq (type-of r1) (type-of r2))
          nil
          "only the rules of the same type can be merged")
  (ematch* (r1 r2)
    (((rule :matrix a :output-shape o1 :rules r :input-shape i)
      (rule :matrix b :output-shape o2 :rules (= r) :input-shape (equal i)))
     (assert (= (length o1) (length o2)))
     (make-instance (type-of r1)
                    :input-shape i
                    :output-shape (mapcar #'+ o1 o2)
                    :rules r
                    :matrix (concatenate (list a b))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regularizers

(defun-ematch* noise-regularizer (rule input output)
  ((_
    (array :dimensions (list samples out-features))
    _)
   (unless (zerop *accuracy*)
     (format t "~&Parameters: ~@{~a~^ ~}" '*accuracy* *accuracy*)
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 (in outer
                     (collecting
                      `(,*accuracy* (not ,(n k i))))))))))

(defun-ematch* sparsity-regularizer (rule input output)
  (((rule :rules rules)
    (array :dimensions (list samples in-features))
    (array :dimensions (list (eq samples) out-features)))
   (unless (zerop *sparsity*)
     (format t "~&Parameters: ~@{~a~^ ~}" '*sparsity* *sparsity*)
     (iter outer
           (for k below out-features)
           (iter (for l below rules)
                 (iter (for m below in-features)
                       (in outer (collecting `(,*sparsity* (not ,(b k l m)))))))))))

(defun-ematch* diversity-regularizer (rule input output)
  (((rule :rules rules)
    (array :dimensions (list samples in-features))
    (array :dimensions (list (eq samples) out-features)))
   (unless (zerop *diversity*)
     (format t "~&Parameters: ~@{~a~^ ~}" '*diversity* *diversity*)
     (iter outer
           (for r1 below rules)
           (iter (for r2 from (1+ r1) below rules)
                 (iter (for k below out-features)
                       (in outer
                           (collect
                               `(,*diversity*
                                 (not (and ,@(iter (for i below in-features)
                                                   (collect
                                                       `(iff ,(b k r1 i) ,(b k r2 i)))))))))))))))

(defun-ematch* PU-noise-regularizer (rule input output)
  ((_
    (array :dimensions (list samples out-features))
    _)
   (unless (zerop *accuracy*)
     (format t "~&Parameters: ~@{~a~^ ~}" '*accuracy* *accuracy*)
     (format t "~&Parameters: ~@{~a~^ ~}" '*unlabeled-accuracy* *unlabeled-accuracy*)
     (iter outer
           (for k below out-features)
           (iter (for i below samples)
                 (in outer
                     (if (= 1 (aref output i k))
                         (collecting
                          `(,*accuracy* (not ,(n k i))))
                         (collecting
                          `(,*unlabeled-accuracy* (not ,(n k i)))))))))))

;; multinomial B index: (output rule input)
;; multinomial Y index: (output sample)
;; multinomial N index: (output sample)
