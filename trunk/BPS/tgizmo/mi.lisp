;; -*- Mode: Lisp; -*- 

;;;; Measurement Interpretation system
;;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991-1992, Kenneth D. Forbus, Northwestern University,
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *tgizmo-laws-file*
  #+ILS  "/u/bps/code/tgizmo/laws"
  #+PARC "virgo:/virgo/dekleer/bps/code/tgizmo/laws"
  #+MCL "Macintosh HD:BPS:tgizmo:laws.fasl")

(defvar *domain-file* #+UNIX "/u/bps/code/tgizmo/tnst.lisp"
  #+PARC "virgo:/virgo/dekleer/bps/code/tgizmo/tnst"
  #+MCL "Macintosh HD:BPS:tgizmo:tnst.fasl")

(defun mi (scenario measurements
		    &key (debugging nil)
		    (debugging-dds nil)
		    (title nil)
		    (domain *domain-file*))
  (with-tgizmo
   (setq *tgizmo* 
   (create-tgizmo 
    (if title title (format nil "MI of ~A" scenario))
    :DEBUGGING debugging :SCENARIO scenario
    :MEASUREMENTS measurements))
   (with-LTRE (tgizmo-ltre *tgizmo*)
    (load *set-rule-file*)
    (setq *debug-dds* debugging-dds)
    (load *tgizmo-laws-file*)
    (load domain)
    (load-scenario scenario)
    (dolist (d measurements)
	    (assume! d :MEASURED))
    (find-states *tgizmo*))
   (values *tgizmo* (length (tgizmo-states *tgizmo*)))))

(defun find-states (&optional (*tgizmo* *tgizmo*))
  (setf (tgizmo-nstates *tgizmo*) 0)
  (setf (tgizmo-states *tgizmo*) nil)
  (Search-PSVS `(Resolve-Completely
		 '(push (snapshot (incf (tgizmo-nstates *tgizmo*)))
			(tgizmo-states *tgizmo*)))))

(defun debug-find-states
  (&optional (thunk 
	      '(progn (push (snapshot (incf (tgizmo-nstates *tgizmo*)))
			    (tgizmo-states *tgizmo*))
		      (print (tgizmo-nstates *tgizmo*))
		      (when (tg-fetch '(Active ?x) :UNKNOWN)
			    (show-state (car (tgizmo-states *tgizmo*)))
			    (break "~% Some status assignments unknow at state ~D"
				   (tgizmo-nstates *tgizmo*)))))
	     (*tgizmo* *tgizmo*))
  (Search-PSVS `(Resolve-Completely ',thunk)))
