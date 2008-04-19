;; -*- Mode: Lisp; -*-

;;;; TGizmo PS/VS operations
;;;; Last Edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun load-scenario (sfile &optional (*tgizmo* *tgizmo*))
  (with-tgizmo *tgizmo*
	       (with-ltre (tgizmo-ltre *tgizmo*)
			  (load sfile))
	       (tg-run-rules)
	       (use-transitivity *tgizmo*)))

(defun gather-vps (&optional (*tgizmo* *tgizmo*))
  (tg-fetch `(Active ?x)))

(defun psvs-choice-sets (&optional (*tgizmo* *tgizmo*))
  (mapcar #'(lambda (a-s) `(,a-s (:NOT ,a-s)))
	  (gather-vps)))

(defun search-PSVS (thunk &optional (*tgizmo* *tgizmo*))
  (DD-Search (psvs-choice-sets)
	     `(unwind-protect 
		  (progn (when-debugging-tgizmo :PSVS-DDS
			  (format t "~% =======================")
			  (show-psvs))
			 ,thunk
			 (when-debugging-tgizmo :PSVS-DDS
			   (format t "~% =======================")))
		(retract-IR-CWAs))))

(defun show-psvs (&optional (*tgizmo* *tgizmo*))
;;; good for examining the current state
  (dolist (active-s (tg-fetch '(Active ?x)))
	  (format t "~% ~A is ~A."
		  (cadr active-s)
		  (case (label-of active-s)
			(:TRUE "active")
			(:FALSE "inactive")
			(t "??")))))
