;; -*- Mode: Lisp; -*-

;;;; Algebra system for CPS
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1986-1993
;;;   Kenneth D. Forbus and Johan de Kleer
;;;   All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

#|
A problem consists of an equation in the unknown, X.  The goal is
to construct an equation that has just X on the left hand side,
and no occurences of X on the RHS.

A state in these problems has the form (= <lhs> <rhs>).  The
sides are just s-expressions corresponding to mathematical terms.
We provide a basic simplification program in another file to take
care of mundane stuff like collapsing constants.

This file contains the operator descriptions and CPS hooks.  The
function SETUP-ALGEBRA-PROBLEM sets up the algebra problem space.
|#

;; Make code more readable
(defmacro lhs (x) `(cadr ,x))
(defmacro rhs (x) `(caddr ,x))

(defun occurs-in? (exp1 exp2) 
  (cond ((equal exp1 exp2) t)
	((null exp2) nil)
	((listp exp2)
	 (or (occurs-in? exp1 (car exp2))
	     (occurs-in? exp1 (cdr exp2))))))

(defun has-unknown? (exp) (occurs-in? 'x exp))
(defun no-unknown? (exp) (not (occurs-in? 'x exp)))

;;;; Problem space interface procedures

(defun got-algebra-goal? (state)
  (and (eq (cadr state) 'x) ;; LHS = X
       (no-unknown? (rhs state)))) ;; No X's in RHS.

(defun find-algebra-operator (state operator)
;; Operators take the form (<name> <procedure>)
  (funcall (cadr operator) state))

(defun print-derivation-step (state op-instance)
  (format nil "~A, via ~A" state (car op-instance)))

;;;; Computing distance estimates for algebra problems
;; A reasonable heuristic is the sum of the depths of
;; occurrences of X in the expression tree.

(defun algebra-distance (expr)
  (labels ((sum-tree-depth
	    (exp depth)
	    (cond ((null exp) 0)
		  ((eq exp 'X) depth)
		  ((not (listp exp)) 0)
		  (t (+ (sum-tree-depth (car exp) (1+ depth))
			(sum-tree-depth (cdr exp) depth))))))
    (+ (sum-tree-depth (lhs expr) 1)
       (sum-tree-depth (rhs expr) 1))))

(defun setup-algebra-problem ()
  (make-problem
   :NAME 'Algebra
   :GOAL-RECOGNIZER 'got-algebra-goal?
   :OPERATOR-APPLIER 'find-algebra-operator
   :STATE-PRINTER #'(lambda (f) (format nil "~A" f))
   :SOLUTION-ELEMENT-PRINTER #'print-derivation-step
   :STATES-IDENTICAL? 'equal
   :DISTANCE-REMAINING 'algebra-distance
   :OPERATORS '((Isolate-Log try-isolate-log)
		(Isolate-Sum try-isolate-sum)
		(Isolate-Difference try-isolate-difference)
		(Isolate-Square try-isolate-square)
		(Collect-Product-Difference try-collect-prod-diff)
		(Attract-Log-Sum try-attract-log-sum)
		(Canonicalize try-canonicalization))))

;; A test case
(defvar *bundy* '(= (+ (log (+ x 1) E) (log (- x 1) E)) C))

;;;; Isolation techniques

(defun try-isolate-log (form &aux bindings)
  (setq bindings
	(match '(= (log (? arg has-unknown?)
			(? base no-unknown?))
		   (? rhs no-unknown?))
	       form))
  (unless (eq bindings :FAIL)
    `(,(cons `(isolate-log-instances ,form)
	     (simplify
	      (substitute-in `(= (? arg) (expt (? base) (? rhs)))
			     bindings))))))

(defun try-isolate-square (form &aux bindings)
  (setq bindings
	(match '(= (sqr (? arg has-unknown?))
		   (? rhs no-unknown?))
	       form))
  (unless (eq bindings :FAIL)
    `(,(cons `(isolate-square ,form)
	     (simplify (substitute-in `(= (? arg) (sqrt (? rhs)))
				      bindings))))))

(defun try-isolate-sum (form &aux bindings)
  (setq bindings
	(match '(= (+ (?? pre no-unknown?)
		      (? arg has-unknown?)
		      (?? post no-unknown?))
		   (? rhs no-unknown?))
	       form))
  (unless (eq bindings :FAIL)
    `(,(cons `(isolate-sum ,form)
	     (simplify
	      (substitute-in `(= (? arg)
				 (- (? rhs) (+ (?? pre) (?? post))))
			     bindings))))))

(defun try-isolate-difference (form &aux bindings)
  (setq bindings
	(match '(= (- (? arg1 has-unknown?)
		      (? arg2 no-unknown?))
		   (? rhs no-unknown?))
	       form))
  (unless (eq bindings :FAIL)
    `(,(cons `(isolate-difference ,form)
	     (simplify (substitute-in `(= (? arg1) (+ (? rhs) (? arg2)))
				      bindings))))))

;;; Collection Methods

;;; Sum collection:
;;; Given (U+V)*(U-V), turn it into U^2-V^2 
;;; Use only on "least dominating terms" in X.
;;; U must have an occurrence of X.

(defun find-least-dominating-terms (exp &aux result xts)
  (cond ((or (null exp) (not (listp exp))) nil)
	(t (setq xts (remove-if #'no-unknown? exp))
	   (cond ((cdr xts) (setq result (list exp))
		  (dolist (xt xts result)
		   (setq result
			 (nconc result
			  (find-least-dominating-terms xt)))))
		 (t (find-least-dominating-terms (car xts)))))))

(defun try-collect-prod-diff (form &aux bindings results)
 (dolist (ldt (find-least-dominating-terms form) results)
  (setq bindings
	(match '(* (+ (? v no-unknown?)
		      (? u has-unknown?))
		   (- (? u) (? v)))
	       ldt))
  (unless (eq bindings :FAIL)
   (push (cons `(collect-product-sum ,ldt)
	       (simplify
		(subst (substitute-in
			`(- (sqr (? U)) (sqr (? V)))
			bindings)
		       ldt form)))
	 results))))

;;; Attraction rule for logs
;;; (log U W) + (log V W) => (log (* U V) W)
;;; where U, V contain X and W doesn't.

(defun try-attract-log-sum (form &aux results bindings)
 (dolist (ldt (find-least-dominating-terms form)
	      results)
  (setq bindings
	(match '(+ (log (? u has-unknown?)
			(? w no-unknown?))
		   (log (? v has-unknown?)
			(? w)))
	       ldt))
  (unless (eq bindings :FAIL)
   (push (cons `(Attract-log-sum ,ldt)
	       (simplify
		(subst (substitute-in
			`(log (* (? U) (? V)) (? W))
			bindings)
		       ldt form)))
	 results))))

(defun try-canonicalization (form &aux result)
  (setq result (simplify form))
  (unless (equal result form)
    `(,(cons `(Canonicalization ,form)
	     result))))
