;; -*- Mode: Lisp; -*-

;;; Definitions for TGizmo
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (tgizmo (:PRINT-FUNCTION tgizmo-printer))
  (title "")
  (measurements nil)   ;; Measurements to be explained.
  (scenario nil)       ;; Backpointer to scenario file
  (ltre nil)
  (debugging nil) ;; Now a list of symbols, to allow finer control.
  (quantities nil)  ;; List of quantities
  (comparisons nil) ;; list of compared numbers
  (comp-cycles nil) ;; Cycles of comparisons.
  (influence-order nil) ;; Order of updating for influence resolution
  ;; Flags indicating when caches need updating
  (update-ineqs? nil) ;; When non-nil, inequality processing needed.
  (nstates 0)     ;; Counter for states
  (states nil)         ;; States found during search
  )

(defun tgizmo-printer (tg st ignore)
  (declare (ignore ignore))
  (format st "<TGizmo ~A>" (tgizmo-title tg)))

(defun create-tgizmo (title &key (debugging nil)
			    (scenario nil)
			    (measurements nil))
  (let ((tg (make-tgizmo :TITLE title
			 :LTRE (create-ltre (concatenate 'string
							 "LTRE of " title))
			 :DEBUGGING debugging
			 :SCENARIO scenario
			 :MEASUREMENTS measurements)))
    (push 'IR-CWA-Contradiction-Handler (ltms-contradiction-handlers
					 (ltre-ltms (tgizmo-ltre tg))))
    (in-tgizmo tg)
    tg))

(defvar *tgizmo* nil) ;; Default tgizmo

(defmacro debugging-tgizmo (key msg &rest args)
  `(when (member ,key (tgizmo-debugging *tgizmo*))
	 (format t ,msg ,@ args)))

(defmacro when-debugging-tgizmo (key &rest code)
  `(when (member ,key (tgizmo-debugging *tgizmo*))
	 ,@ code))

(defun change-tgizmo (tg &key (debugging :NADA))
  (unless (eq debugging :NADA) (setf (tgizmo-debugging tg) debugging)))

(defun in-tgizmo (tg) 
  (setq *tgizmo* tg)
  (in-ltre (tgizmo-ltre tg))
  tg)

(defmacro with-tgizmo (tg &rest forms) `(let ((*tgizmo* ,tg)) ,@ forms))

;; The STATE struct allows solutions to be cached.

;; It should hold enough information to allow all the relevant
;;    conclusions that were in the LTMS when it was created to
;;    be re-derived.  (Although perhaps not with the same support!)

(defstruct (state
	    (:PRINT-FUNCTION tg-state-printer))
  (title nil)
  (individuals nil)
  (view-structure nil)
  (process-structure nil)
  (comparisons nil)
  (Ds-values nil))

(defun tg-state-printer (tg st ignore)
  (declare (ignore ignore))
  (format st "<State ~A>" (state-title tg)))

;;;; Some useful utilities

(defun tg-fetch (form &optional (status nil) (*tgizmo* *tgizmo*)
		      &aux matches)
  (with-LTRE (tgizmo-ltre *tgizmo*)
	     (setq matches (fetch form))
	     (case status
		   (:TRUE (remove-if-not #'true? matches))
		   (:FALSE (remove-if-not #'false? matches))
		   (:KNOWN (remove-if-not #'known? matches))
		   (:UNKNOWN (remove-if-not #'unknown? matches))
		   (t matches))))

(defun tg-true? (form &optional (*tgizmo* *tgizmo*))
  (with-LTRE (tgizmo-ltre *tgizmo*)
	     (true? form)))

(defun tg-false? (form &optional (*tgizmo* *tgizmo*))
  (with-LTRE (tgizmo-ltre *tgizmo*)
	     (false? form)))

(defun tg-false-forms? (forms &optional (*tgizmo* *tgizmo*))
  (dolist (form forms T)
	  (cond ((and (listp form) (eq (car form) :NOT))
		 (unless (tg-true? (cadr form))
			 (return-from tg-false-forms? nil)))
		(t (unless (tg-false? (cadr form))
			   (return-from tg-false-forms? nil))))))

(defun tg-run-rules (&optional (*tgizmo* *tgizmo*))
  (with-LTRE (tgizmo-ltre *tgizmo*) (run-rules)))

(defun same-elements? (l1 l2) ;; Generally useful
  (null (set-exclusive-or l1 l2 :TEST #'equal)))

;;;; Some fancy print routines

(defun number-string (form)
  (cond ((listp form) 
	 (format nil "~A[~A(~A)]" (car form) (caadr form)
		 (cadadr form)))
	(t (format nil "~A" form))))

(defun Ds-string (form)
  (cond ((listp form) 
	 (format nil "Ds[~A(~A)]" (car form) (cadr form)))
	(t (format nil "~A" form))))

(defun Ds-value-string (q rel)
  (format nil "~A=~A" (Ds-string q)
	  (case rel
		((:= =) 0)
		((:> >) 1)
		((:< <) -1)
		((:<= <=) "{-1,0}")
		((:>= >=) "{0,1}")
		(:BT "irrelevant")
		(:?? "{-1,0,1}"))))

(defun ineq-string (rel n1 n2)
  (format nil "~A ~A ~A"
	  (number-string n1) rel
	  (number-string n2)))
