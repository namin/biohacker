;; -*- Mode: Lisp; -*-

;;;; Algebraic simplifier
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; This version is inspired by one of G.J. Sussman's scheme matchers.

(defvar *simplify-cache* (make-hash-table :TEST #'equal))

(defun simplify (exp)
  (or (gethash exp *simplify-cache*)
      (setf (gethash exp *simplify-cache*) 
	    (simplify-it exp *algebra-rules*))))

(defun clear-simplify-cache ()
  (clrhash *simplify-cache*))

(defun simplify-it (exp rules &aux result)
  (setq result
	(try-matcher-rules
	 (if (listp exp) (mapcar #'simplify exp)
	   exp)
	 rules))
  (if (equal result exp) result
    (simplify-it result rules)))

(defun try-matcher-rules (exp rules)
  (dolist (rule rules exp) ;; Return the original expression by default
    (let ((bindings (match (rule-pattern rule) exp nil)))
;;      FOR DEBUGGING
;;      (unless (eq bindings :FAIL)
;;      (format t "~% Matched ~A on ~A." exp rule)
;;      (dolist (binding bindings)
;;      (format t "~% ~A: ~A." (car binding)
;;                (var-value (list '? (car binding)) bindings))))
      (unless (eq bindings :FAIL)
	(when (check-predicate (rule-predicate rule) bindings)
	  (return-from try-matcher-rules
		       (substitute-in (rule-skeleton rule) bindings)))))))

(defun check-predicate (proc bindings)
  (unless proc (return-from check-predicate T))
  (eval (substitute-in proc bindings)))

(defun rule-pattern (rule) (car rule))
(defun rule-predicate (rule) (cadr rule))
(defun rule-skeleton (rule) (caddr rule))

;;;; Algebra utilities

(defun alg< (e1 e2) ;; Sort predicate for algebraic expressions
  (cond ((equal? e1 e2) nil)
	((consp e1)
	 (if (consp e2)
	     (if (equal? (car e1) (car e2))
		 (alg< (cdr e1) (cdr e2))
		 (alg< (car e1) (car e2)))
	     nil))
	((consp e2) t)
	((symbolp e1)
	 (if (symbolp e2)
	     (string< (symbol-name e1) (symbol-name e2))
	     nil))
	((symbolp e2) t)
	((and (numberp e1) (numberp e2)) (< e1 e2))
	(t (error "alg< cannot compare these: ~A, ~A." e1 e2))))

(defun alg= (e1 e2) (not (or (alg< e1 e2) (alg< e2 e1))))
 
(defun sorted? (list pred)
  (cond ((or (null list) (null (cdr list))) t)
	((funcall pred (cadr list) (car list)) nil)
	(t (sorted? (cdr list) pred))))

(defun +/*? (exp) (or (eq exp '+) (eq exp '*)))

(defun same-constant? (exp constant)
  (and (numberp exp)
       (if (floatp exp) (equal? exp (float constant))
	   (= exp constant))))

(defun zero? (exp) (same-constant? exp 0))
(defun one? (exp) (same-constant? exp 1))

;;;; Extra utilities

(defun occurs-in? (exp1 exp2) 
  (cond ((equal exp1 exp2) t)
	((null exp2) nil)
	((listp exp2)
	 (or (occurs-in? exp1 (car exp2))
	     (occurs-in? exp1 (cdr exp2))))))


;;;; Rules for algebraic simplification

(setq *algebra-rules* `(
;; Flush degenerate cases
(((? op +/*?) (? e)) nil (? e))
((+ (? zero zero?) (?? e)) nil (+ (?? e)))
((- (? zero zero?) (? e)) nil (- (? e)))
((- (? e) (? zero zero?)) nil (? e))
((- (? e) (? e)) nil 0)
((* (? one one?) (?? e)) nil (* (?? e)))
((* (? zero zero?) (?? e)) nil 0)
((expt (? e) (? zero zero?)) nil 1)
((expt (? e) (? one one?)) nil (? e))
((log (? one one?) (? base)) nil 0)
((log (? base) (? base)) nil 1)
((log (expt (? base) (? val)) (? base)) nil (? val))
((expt (? base) (log (? val) (? base))) nil (? val))
;; Equivalences involving powers
((* (? e) (? e)) nil (sqr (? e)))
((expt (? e) (? two ,#'(lambda (exp) (same-constant? exp 2))))
 nil (sqr (? e)))
((sqrt (sqr (? e))) nil (abs (? e)))
((sqr (sqrt (? e))) nil (? e))

;; Combine numerical constants
(((? op +/*?) (? e1 numberp) (? e2 numberp) (?? e3))
 nil
 ((? op) (:EVAL ((? op) (? e1) (? e2))) (?? e3)))
((- (- (? e1) (? e2))) nil (- (? e2) (? e1))) ;; strip
((- (? e1 numberp) (? e2 numberp)) nil (:EVAL (- (? e1) (? e2))))
((- (? e1 numberp)) nil (:EVAL (- (? e1))))
((- (? e1) (? e2 numberp)) nil (+ (- (? e2)) (? e1)))
((- (? e1 numberp) (+ (? e2 numberp) (?? e3)))
 nil (- (:EVAL (- (? e1) (? e2))) (+ (?? e3))))
((- (? e1 numberp) (- (? e2 numberp) (?? e3)))
 nil (+ (:EVAL (- (? e1) (? e2))) (?? e3)))
((+ (? e1 numberp) (- (? e2 numberp) (?? e3)))
 nil (- (:EVAL (+ (? e1) (? e2))) (?? e3)))
((sqr (? e1 numberp)) nil (:EVAL (* (? e1)  (? e1))))
((sqrt (? e1 numberp)) nil (:EVAL (sqrt (? e1))))
((expt (? e1 numberp) (? e2 numberp)) nil (:EVAL (expt (? e1) (? e2))))
((/ (? e1 numberp) (? e2 numberp)) nil (:EVAL (/ (? e1) (? e2))))
((* (? e1 numberp) (/ (? e2) (? e3 numberp))) nil
 (* (:EVAL (/ (? e1) (? e3))) (? e2)))
((/ (* (? e1 numberp) (? e2)) (? e3 numberp)) nil
 (* (:EVAL (/ (? e1 numberp) (? e3 numberp))) (? e2)))
((* (?? pre) (- (? term)) (?? post)) nil
 (* (?? pre) (* -1 (? term)) (?? post)))
((abs (? e numberp)) nil (:EVAL (abs (? e))))
((log (? x numberp) (? base numberp)) 
 nil (:EVAL (/ (log (? x)) (log (? base)))))
;; Flatten +,*
(((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
 nil
 ((? op) (?? e1) (?? e2) (?? e3)))
;; Combine like terms
((+ (?? pre) (* (? f1) (? thing)) (* (? f2) (? thing)) (?? post)) nil
 (+ (?? pre) (* (* (? f1) (? f2)) (? thing)) (?? post)))
((+ (?? pre) (* (? f1) (? thing)) (?? mid) (? thing) (?? post)) nil
 (+ (?? pre) (* (+ 1 (? f1)) (? thing)) (?? mid) (?? post)))
;; Canonicalize +,*
(((? op +/*?) (?? terms))
 (not (sorted? (quote (? terms)) #'alg<))
 ((? op) (:SPLICE (sort (quote (? terms)) #'alg<))))))
