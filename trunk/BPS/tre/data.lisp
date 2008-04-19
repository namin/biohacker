;; -*- Mode: Lisp; -*-

;;; Database for Tiny Rule Engine
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; This simple version uses "car indexing" to store facts and rules
;; which might match.  Unification provides the actual matching.

(proclaim '(special *TRE* *ENV*))

(defstruct (dbclass (:PRINT-FUNCTION (lambda (d st ignore)
				     (format st "<Dbclass ~D>"
					     (dbclass-name d)))))
     name		;a symbol
     tre                ;The TRE it belongs to
     facts		;facts of this dbclass
     rules)		;rules applicable to this dbclass

(defun show-data (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
	       (dolist (datum (dbclass-facts dbclass))
		       (incf counter)
		       (format stream "~%~A" datum)))
	   (tre-dbclass-table *TRE*))
  counter)

;;;; Installing new facts

(defun assert! (fact &optional (*TRE* *TRE*))
  (when (insert fact *tre*)  ;; when it isn't already there
    (try-rules fact *tre*))) ;; run the rules on it.

(defun insert (fact tre &aux dbclass)
  (setq dbclass (get-dbclass fact tre)) ;Question: Why not use PUSHNEW here?
  (unless (member fact (dbclass-facts dbclass) :TEST #'equal)
	  (debugging-tre "~% ~A: Inserting ~A into database." tre fact)
	  (push fact (dbclass-facts dbclass))))

(defun get-dbclass (fact tre &aux dbclass val)
  (cond ((listp fact) (get-dbclass (car fact) tre))
	((variable? fact)
	 ;; We might be in the environment of some rule, so must
	 ;; check the variable's bindings.
	 (cond ((boundp fact) (get-dbclass (symbol-value fact) tre))
	       ((setq val (assoc fact *ENV*))
		(get-dbclass (cdr val) tre))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((setq dbclass (gethash fact (tre-dbclass-table tre))) dbclass)
	       ;; Nothing found, so build it.
	       (t (setq dbclass (make-dbclass :NAME fact :TRE tre
					  :FACTS nil :RULES nil))
		  (setf (gethash fact (tre-dbclass-table tre)) dbclass)
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

;;;; Fetching data

(defun fetch (pattern &optional (tre *TRE*) &aux bindings unifiers)
  ;; Returns the list of facts which unify with the pattern.
  (dolist (candidate (get-candidates pattern tre) unifiers)
    (setq bindings (unify pattern candidate))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

(defun get-candidates (pattern tre) (dbclass-facts (get-dbclass pattern tre)))
