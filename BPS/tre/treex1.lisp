;; -*- Mode: LISP; -*-

;;;; Test cases for TRE
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun ex1 (&optional (debugging nil))
  (in-tre (create-tre "Ex1" :DEBUGGING debugging)) 
  (run-forms *TRE*
    '(
      ;; A simple version of Modus Ponens
      (rule (implies ?ante ?conse)
	    (rule ?ante
		  (assert! ?conse)))
      ;; A simple version of negation elimination
      (rule (not (not ?x)) (assert! ?x))
      (assert! '(implies (human Turing) (mortal Turing)))
      (assert! '(not (not (human Turing))))))
  (show-data))

(defun ex2 (&optional (debugging nil))
  (in-tre (create-tre "Ex2" :DEBUGGING debugging))
  ;; Rules can be used to interface with other representations
  (setq *parts* nil)
  (run-forms *TRE*
    '( ;; Creates parts index, say for graphics system
      (rule (has-part ?sys ?part)
	    (let ((entry (assoc ?sys *parts*)))
	      (unless entry
		      (push (setq entry (cons ?sys nil))
			    *parts*))
	      (pushnew ?part (cdr entry))))
      ;; Parts of a car
      (rule (car ?c)
	    (assert! `(has-part ,?c (Engine ,?c)))
	    (assert! `(has-part ,?c (Body ,?c)))
	    (assert! `(has-part ,?c (Chasis ,?c))))
      ;; Parts of a workstation
      (rule (workstation ?c)
	    (assert! `(has-part ,?c (Disk ,?c)))
	    (assert! `(has-part ,?c (Screen ,?c)))
	    (assert! `(has-part ,?c (CPU-box ,?c)))
	    (assert! `(has-part ,?c (Keyboard ,?c))))
      (assert! '(car Ariel))
      (assert! '(Workstation Hal-9000))))
    (pprint *parts*))

(defun ex3 (&optional (debugging nil))  
  (in-tre (create-tre "Ex3" :DEBUGGING debugging))
  ;; You may want to run this one last.
  (run-forms *TRE* '(
    (rule (integer ?x)
	  (when (numberp ?x)
	    (assert! `(integer ,(1+ ?x)))))
    (assert! `(integer 0)))))

