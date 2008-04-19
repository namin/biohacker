;; -*- Mode: Lisp; -*-

;;;; Natural deduction examples for FTRE.
;;;;  Modified: forbus on Tue Apr 2 10:31:13 1996

;;; Copyright (c) 1993-1996 Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *nd-rules* "fnd")

(defun setup-ftre (title &key (debugging nil)
		      (debugging-contexts nil)
		      (max-depth 5)) 
  (in-ftre (create-ftre title :DEBUGGING debugging
			:DEBUGGING-CONTEXTS debugging-contexts
			:MAX-DEPTH max-depth))
  (bps-load-file *ftre-path* *nd-rules*))

(defun ex1 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests NI, CE, OI, and contradiction detection.
  (setup-ftre "Ex 1" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(implies p q))
		     (assert! '(not q))
		     (assert! '(show (not p)))))))

(defun ex2 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests CI and OE.
  (setup-ftre "Ex 2" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(or (not P) R))
		     (assert! '(implies R Q))
		     (assert! '(show (implies P Q)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More examples

(defun ex3 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests AE and AI.
  (setup-ftre "Ex 3" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(and A B))
		     (assert! '(and B C))
		     (assert! '(show (and A B C)))))))

(defun ex4 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests BI and "Star Trek" problem
  (setup-ftre "Ex 4" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! 'contradiction)
		     (assert! '(show (iff P Q)))))))

(defun ex5 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests indirect proof.
  (setup-ftre "Ex 5" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(implies (not p) q))
		     (assert! '(not q))
		     (assert! '(show p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yet more examples

(defun ex6 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 6" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(iff (and R L (not P)) J))
		     (assert! '(implies (not A) (not R)))
		     (assert! '(not A))
		     (assert! '(show (not J)))))))

(defun ex7 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 7" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(implies (and J C) P))
		     (assert! '(iff (and M C) (or P J)))
		     (assert! 'J)
		     (assert! '(show (and P M)))))))

(defun ex8 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))     ;; A tough one
  (setup-ftre "Ex 8" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts
	:MAX-DEPTH 5)
  (time (run-forms *ftre*
		   '((assert! '(show (implies (implies p q)
					      (or (not p) q))))))))

(defun ex9 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 9" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre* '((assert! '(or (and F G) (and G B)))
			    (assert! '(implies F (not G)))
			    (assert! '(show B))))))

