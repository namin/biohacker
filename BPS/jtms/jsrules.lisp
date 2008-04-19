;;; -*- Mode: LISP; -*-

;;;; Basic rules for JSAINT.
;;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern
;;; University, and Johan de Kleer, Xerox Corporation.
;;; All Rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Expand pointers
(rule ((:IN (AND-SUBGOALS ?parent ?children) :VAR ?def))
      (dolist (child ?children)
	(rlet ((?child (:EVAL child)))
	      (rassert! (PARENT-OF ?child ?parent :AND)
			(:DEF-OF-AND ?def))
	      (rule ((:IN (failed ?child) :VAR ?delinquent))
		    (rassert! (failed ?parent)
			      (:AND-FAILURE ?def ?delinquent)))))
      (assert! `(solved ,?parent)
		`(:AND-SUCCESS ,?def
		  ,@ (mapcar #'(lambda (child)
				 `(SOLVED ,child))
			     ?children))))

(rule ((:IN (OR-SUBGOALS ?parent ?children) :VAR ?def
	    :TEST ?children))
      (dolist (child ?children)
	(rlet ((?child (:EVAL child)))
	      (rassert! (PARENT-OF ?child ?parent :OR)
			(:DEF-OF-OR ?def))
	      (rule ((:IN (SOLVED ?child) :VAR ?winner))
		    (rassert! (SOLVED ?parent)
			      (:OR-SUCCESS ?winner ?def)))))
      (assert! `(FAILED ,?parent)
	       `(:OR-FAILURE ,?def
			     ,@ (mapcar #'(lambda (child)
					    `(FAILED ,child))
					?children))))

(rule ((:IN (PARENT-OF ?child ?parent ?type) :VAR ?lineage))
      (rassert! (RELEVANT ?child)
		(:STILL-WORKING-ON (OPEN ?parent) ?lineage)))

(rule ((:IN (SOLUTION-OF ?problem ?answer) :VAR ?found))
      (rassert! (SOLVED ?problem) (:FOUND-ANSWER ?found)))

(rule ((:IN (OR-SUBGOALS (Integrate ?expr) NIL) :VAR ?no-ideas))
      (rassert! (FAILED (Integrate ?expr)) (:NO-METHODS ?no-ideas)))

(rule ((:IN (SOLVED ?problem))) ;; Can only happen once
      (retract! `(OPEN ,?problem) :EXPAND-AGENDA-ITEM t))

(rule ((:IN (FAILED ?problem))) 
      (retract! `(OPEN ,?problem) :EXPAND-AGENDA-ITEM t))
