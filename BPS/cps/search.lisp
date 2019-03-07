;; -*- Mode: Lisp; -*-

;;; CPS, the Classical Problem Solver
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This program implements the classical "problem space"
;;; model of AI problem solving.

#|
A problem is specified by an initial state, a set of operators that
act on states to produce new states, and a means of recognizing when
the goal has been achieved.  In this implementation the PROBLEM struct
holds this information.  To provide maximum flexiblity, we assume
nothing about states, but define the problem space in terms of a set
of procedures.
|#

(in-package :COMMON-LISP-USER)

(defvar *debug-cps* nil) ;; prints extra information for debugging.

(defstruct (problem (:PRINT-FUNCTION
		     (lambda (pr str ignore)
		       (format str "<Problem: ~A>" (pr-name pr))))
		    (:CONC-NAME pr-))
  name  ;; Something recognizable by user (person or system)
  (goal-recognizer nil)
  ;; Procedure which returns nil if argument state isn't a goal
  (operator-applier nil)
  ;; Finds all ways an operator can apply to a state. 
  ;; Result is a list ((<operator instance> . <new state>)...)
  (operators nil) ;; List of operators which may be used.
  (states-identical? nil)
  ;; Takes two states and returns nil if they are not identical
  (path-filter nil)
  ;; Optional, returns nil if the given path doesn't make sense.
  ;; Allows encoding of extra domain-specific constraints.
  (distance-remaining nil)
  ;; Optional, returns estimate of distance from state to goal.
  (state-printer nil)  ;; Produces a string describing a state.
  (solution-element-printer nil)
  ;; Takes an operator instance and a state, and produces a string
  ;; suitable for human consumption.  Used in displaying results.
  )

(defstruct (path (:PRINT-FUNCTION
		 (lambda (inst str ignore)
		  (format str "<path ~a>" 
			(path-current inst)))))
  (pr nil)            ; The problem it is part of.
  (current nil)       ; The current state
  (so-far nil)        ; Alternating states and operator instances
  (distance nil))     ; Used in advanced versions

;;;; CPS using breadth-first search
;;; returns three values, the final state, the path, and the number
;;; of states examined.

(defun bsolve (initial pr)
  (do ((queue (list (make-path :CURRENT initial
			       :SO-FAR (list initial)
			       :PR pr))
	      (append (cdr queue) new-paths))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined))) ;gather statistics
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps* 
	(format t "~% CPS: Found goal state: ~A"
		(funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))	
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~% CPS: State explored: ~A"
	      (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~% CPS: New operator instances:")
      (print-new-paths new-paths))))

;;; Extend paths using domain-specific procedures

(defun extend-path (path &aux new-paths new-path pr)
  (setq pr (path-pr path))
  (dolist (op (pr-operators pr) new-paths)
   (dolist (op-pair (funcall (pr-operator-applier pr)
			     (path-current path)
			     op))
	   ;; There can be more than one instantiation of the operator
	   ;; for each state, hence this inner loop.
	   (setq new-path (make-path
			   :PR (path-pr path)
			   :CURRENT (cdr op-pair) ;new state
			   :SO-FAR (cons (cdr op-pair) 
					 (cons (car op-pair) ;op instance
					       (path-so-far path)))))
	   (unless (path-has-loop? new-path) ;avoid loops
		   (unless (and (pr-path-filter pr)
				;use domain guidance if available
				(funcall (pr-path-filter pr) new-path))
			   (push new-path new-paths))))))

(defun path-has-loop? (ipath)
  ;;Go backwards down path to see if a state is
  ;;duplicated.  Must skip over operator instances.
  (do ((path (cddr (path-so-far ipath)) (cddr path))
       (state (path-current ipath))
       (pr (path-pr ipath)))
      ((null path))
    (if (funcall (pr-states-identical? pr) state (car path))
	(return t))))

(defun print-new-paths (new-paths)
  (dolist (new-path new-paths)
    (format t "~%  ~A" (cadr (path-so-far new-path))))
  (format t "."))

(defun print-answer (path &optional (stream *standard-output*)
			 &aux rpath pr)
  (setq rpath (reverse (path-so-far path))
	pr (path-pr path))
  (format stream "~%Initial state: ~A."
	  (funcall (pr-state-printer pr) (car rpath)))
  (do ((path (cdr rpath) (cddr path))
       (step 1 (1+ step)))
      ((null path) (format stream "~% Done."))
    (format stream "~%~D.  ~A" step
	    (funcall (pr-solution-element-printer pr)
		     (cadr path) (car path)))))
