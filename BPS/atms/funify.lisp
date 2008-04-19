;; -*- Mode: Lisp; -*- 

;;; Extra pattern-matching facilities for FTRE
;;; Last edited: 1/29/93, KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus and Johan de Kleer,
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *bound-vars*))

(defun quotize (pattern)
  (cond ((null pattern) nil)
	((variable? pattern) pattern)
	((not (listp pattern)) (list 'QUOTE pattern))
	((eq (car pattern) :EVAL) (cadr pattern))
	(t `(cons ,(quotize (car pattern))
		  ,(quotize (cdr pattern))))))

(defmacro rlet (var-specs &rest body)
  ;; Provides means for lisp code in body to
  ;; add information to the rule's environment.
  (let ((*bound-vars*
	 (append (mapcar #'car var-specs) *bound-vars*)))
    `(let ,(mapcar
	    #'(lambda (let-clause)
		(list (car let-clause)
		      (if (and (listp (cadr let-clause))
			       (eq (car (cadr let-clause))
				   :EVAL))
			  (cadr (cadr let-clause))
			(quotize (cadr let-clause)))))
			    var-specs)
       ,@ (fully-expand-body body))))

;;; Finding free variables in a pattern

(defun pattern-free-variables (pattern)
  (pattern-free-vars1 pattern nil))

(defun pattern-free-vars1 (pattern vars)
  (cond ((null pattern) vars)
	((variable? pattern)
	 (if (or (member pattern vars)
		 (member pattern *bound-vars*))
	     vars
	     (cons pattern vars)))
	((atom pattern) vars)
	(t (pattern-free-vars1
	     (cdr pattern)
	     (pattern-free-vars1 (car pattern) vars)))))

;;;; Open-coding unification

(defun generate-match-body (pattern vars extra-test
				    &aux structure-tests var-alist
				    equal-tests binding-specs)
  (dolist (test (generate-unify-tests pattern vars nil 'P))
    (cond ((variable? (car test))
	   ;test looks like (?x (nth p) (nth p) ...)
	   (setq equal-tests
		 (append (generate-pairwise-tests (cdr test))
			 equal-tests))
	   (if extra-test 
	       (push (cons (car test) (car (last test)))
		     var-alist))
	   (push (car (last test)) binding-specs))
	  (t (push test structure-tests))))
  (setq extra-test (sublis var-alist extra-test))
  (when (pattern-free-variables extra-test)
    (error "Rule test includes free variable: ~A"
	   extra-test))
  (values (append structure-tests equal-tests
		  (if extra-test (list extra-test)))
	  binding-specs))

(defun generate-pairwise-tests (tests)
  (cond ((or (null tests) (null (cdr tests))) nil)
	(t (cons (list 'EQUAL (car tests) (cadr tests))
		 (generate-pairwise-tests (cdr tests))))))

;;; Generate a list of explicit tests for matching 
;;; the given pattern. Assumes that the pattern
;;;    to be tested will be in variable "P".
;;; Tests are returned in backward order.
;;; (generate-unify-tests '(foo ?x) nil nil 'P)
;;;     returns:    '((NULL (CDR (CDR P)))
;;;                   (EQUAL ?X (CAR (CDR P)))
;;;                   (CONSP (CDR P))
;;;                   (EQUAL (QUOTE FOO) (CAR P))
;;;                   (CONSP P))

(defun generate-unify-tests (pattern vars tests path)
  (cond ((null pattern)
	 	;this is the end
	 (cons `(null ,path) tests))
	((member pattern vars)	                  
         ;; must see if the pattern has been bound elsewhere,
	 ;; and if it has, test to see if the element here is
         ;; consistent with that earlier binding.
	 (let ((previous (assoc pattern tests)))
	   (cond (previous ;add this position to test it
		  (push path (cdr previous))
		  tests)
		 (t (cons (list pattern path) tests)))))
	;; if variable, it must be bound so test
	;; against the current value.
	((variable? pattern) (cons `(equal ,pattern ,path)
				   tests))
	;; if not a list, then see if equal
	((numberp pattern)
	 (cons `(and (numberp ,path) (= ,pattern ,path))
	       tests))
	((atom pattern) (cons `(equal ',pattern ,path) tests))
	;; recurse on a list
	(t (generate-unify-tests (cdr pattern) vars
		 (generate-unify-tests (car pattern) vars
				       ;avoid lisp errors
				       (cons `(consp ,path)
					     tests)
				       	    ;extend the path
				       (list 'car path))
		 ;extend path in other direction
		 (list 'cdr path)))))		
