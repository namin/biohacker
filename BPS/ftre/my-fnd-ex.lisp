;; -*- Mode: Lisp; -*-

;;;; Natural deduction examples for FTRE.
;;;;  Modified: forbus on Tue Apr 2 10:31:13 1996
;;;;  Modified: namin for exercises 17 and 18

;;; Copyright (c) 1993-1996 Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *nd-rules* "my-fnd")

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
		   '((assert! '(premise (implies p q)))
		     (assert! '(premise (not q)))
		     (assert! '(goal (not p))))))
  (solved? *ftre*))

(defun ex2 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests CI and OE.
  (setup-ftre "Ex 2" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise (or (not P) R)))
		     (assert! '(premise (implies R Q)))
		     (assert! '(goal (implies P Q))))))
  (solved? *ftre*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More examples

(defun ex3 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests AE and AI.
  (setup-ftre "Ex 3" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise (and A B)))
		     (assert! '(premise (and B C)))
		     (assert! '(goal (and A B C))))))
  (solved? *ftre*))

(defun ex4 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests BI and "Star Trek" problem
  (setup-ftre "Ex 4" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise contradiction))
		     (assert! '(goal (iff P Q))))))
  (solved? *ftre*))

(defun ex5 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests indirect proof.
  (setup-ftre "Ex 5" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise (implies (not p) q)))
		     (assert! '(premise (not q)))
		     (assert! '(goal p)))))
  (solved? *ftre*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yet more examples

(defun ex6 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 6" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise (iff (and R L (not P)) J)))
		     (assert! '(premise (implies (not A) (not R))))
		     (assert! '(premise (not A)))
		     (assert! '(goal (not J))))))
  (solved? *ftre*))

;; not solved
(defun ex7 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 7" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre*
		   '((assert! '(premise (implies (and J C) P)))
		     (assert! '(premise (iff (and M C) (or P J))))
		     (assert! '(premise J))
		     (assert! '(goal (and P M))))))
  (solved? *ftre*))

;; not solved
(defun ex8 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))     ;; A tough one
  (setup-ftre "Ex 8" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts
	:MAX-DEPTH 5)
  (time (run-forms *ftre*
		   '((assert! '(goal (implies (implies p q)
					      (or (not p) q)))))))
  (solved? *ftre*))

(defun ex9 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 9" :DEBUGGING debugging
	:DEBUGGING-CONTEXTS debugging-contexts 
	:MAX-DEPTH max-depth)
  (time (run-forms *ftre* '((assert! '(premise (or (and F G) (and G B))))
			    (assert! '(premise (implies F (not G))))
			    (assert! '(goal B)))))
  (solved? *ftre*))

