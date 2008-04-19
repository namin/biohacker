;; -*- Mode: Lisp; -*-

;;;; Simple shakedown procedure for JTRE 
;;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun shakedown-jtre ()
  (in-jtre (create-jtre "Test One"))
  (dolist (form '((rule ((:INTERN (foo ?x) :VAR ?f :TEST (numberp ?x))
			 (:INTERN (bar ?y) :VAR ?g :TEST (numberp ?y)))
			(rassert! (mumble ?x ?y) (Test-intern ?f ?g)))
		  (format t "~% :INTERN rule defined okay.")
		  (rule ((:IN (foo ?x) :VAR ?f
			      :TEST (not (numberp ?x)))
			 (:IN (bar ?y) :VAR ?g
			      :TEST (not (numberp ?y))))
			(rassert! (grumble ?x ?y)
			 (:TEST-in ?f ?g)))
		  (format t "~% :IN rule defined okay.")
		  (referent '(foo 1) t)
		  (cond ((fetch '(foo 1))
			 (format t "~% Referent worked okay."))
			(t (error "Referent failed.")))
		  (referent '(bar 1) t)
		  (run-rules)
		  (format t "~% No errors during attempted rule execution.")
		  (cond ((fetch '(mumble 1 1))
			 (format t "~%:INTERN rule fired okay."))
			(t (error "~% :INTERN rule failed to fire.")))
		  (referent '(foo a) t)
		  (referent '(bar a) t)
		  (run-rules)
		  (when (some #'(lambda (fact) (in? fact))
			      (fetch '(grumble ?p ?q)))
			(format t "~%Premature triggering of :IN rule."))
		  (uassume! '(foo a) :USER)
		  (uassume! '(bar a) :USER)
		  (cond ((in? '(grumble a a))
			 (format t "~% :IN rule worked okay."))
			(t (format t "~%:IN rule failed to fire.")))
		  (uassume! '(foo 1) :USER)
		  (uassume! '(bar 1) :USER)
		  (unless (in? '(mumble 1 1))
			  (format t "~% Reference or JTMS failure.")))
		:OKAY)
	  (print (eval form))))
