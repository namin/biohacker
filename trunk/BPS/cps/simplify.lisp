;; -*- Mode: Lisp; -*-

;;;; Algebraic simplifier
;; Last edited 7/12/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This version is inspired by one of G.J. Sussman's scheme matchers.

(in-package :COMMON-LISP-USER)

(defvar *simplify-cache* (make-hash-table :TEST #'equal))

(defun simplify (exp)
  (or (gethash exp *simplify-cache*)
      (setf (gethash exp *simplify-cache*) 
	    (simplify-it exp *algebra-rules*))))

(defun simplify-it (exp rules &aux result)
  (setq result
	(try-matcher-rules
	 (if (listp exp) (mapcar #'simplify exp)
	   exp)
	 rules))
  (if (equal result exp) result
    (simplify-it result rules)))

(defun try-matcher-rules (exp rules)
 ;; Return the original expression by default
  (dolist (rule rules exp)
    (let ((bindings (match (rule-pattern rule) exp nil)))
;;      FOR DEBUGGING
;;      (unless (eq bindings :FAIL)
;;      (format t "~% Matched ~A on ~A." exp rule)
;;      (dolist (binding bindings)
;;      (format t "~% ~A: ~A." (car binding)
;;       (var-value (list '? (car binding)) bindings))))
      (unless (eq bindings :FAIL)
       (return-from try-matcher-rules
		    (substitute-in (rule-result rule)
                                   bindings))))))

(defun rule-pattern (rule) (car rule))
(defun rule-result (rule) (cadr rule))

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

;;;; Rules for algebraic simplification

(setq *algebra-rules* `(

;; Flush degenerate cases
(((? op +/*?) (? e)) (? e))
((+ (? zero zero?) (?? e)) (+ (?? e)))
((- (? zero zero?) (? e)) (- (? e)))
((- (? e) (? zero zero?)) (? e))
((- (? e) (? e)) 0)
((* (? one one?) (?? e)) (* (?? e)))
((* (? zero zero?) (?? e)) 0)
((expt (? e) (? zero zero?)) 1)
((expt (? e) (? one one?)) (? e))
((log (? one one?) (? base)) 0)
((log (? base) (? base)) 1)
((log (expt (? base) (? val)) (? base)) (? val))
((expt (? base) (log (? val) (? base))) (? val))
;; Equivalences involving powers
((* (? e) (? e)) (sqr (? e)))
((expt (? e) (? two ,#'(lambda (exp) (same-constant? exp 2))))
 (sqr (? e)))
((sqrt (sqr (? e))) (abs (? e)))
((sqr (sqrt (? e))) (? e))

;; Combine numerical constants
(((? op +/*?) (? e1 numberp) (? e2 numberp) (?? e3))
 nil
 ((? op) (:EVAL ((? op) (? e1) (? e2))) (?? e3)))
((- (? e1 numberp) (? e2 numberp)) (:EVAL (- (? e1) (? e2))))
((- (? e1 numberp)) (:EVAL (- (? e1))))
((sqr (? e1 numberp)) (:EVAL (* (? e1)  (? e1))))
((sqrt (? e1 numberp)) (:EVAL (sqrt (? e1))))
((expt (? e1 numberp) (? e2 numberp)) (:EVAL (expt (? e1) (? e2))))
((/ (? e1 numberp) (? e2 numberp)) (:EVAL (/ (? e1) (? e2))))
((abs (? e numberp)) (:EVAL (abs (? e))))
((log (? x numberp) (? base numberp)) 
 (:EVAL (/ (log (? x)) (log (? base)))))
;; Flatten +,*
(((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
 ((? op) (?? e1) (?? e2) (?? e3)))
(((? op +/*?) ((? op) (?? e1) (?? e2)) (?? e3))
 ((? op) (?? e1) (?? e2) (?? e3)))
;; Canonicalize +,*
(((? op +/*?) (?? terms
		  ,#'(lambda (terms) (not (sorted? terms #'alg<)))))
 ((? op) (:SPLICE (:EVAL (sort (quote (? terms)) #'alg<)))))))
