;-*- Mode: LISP; Syntax: Common-lisp; Package: USER-*-

;;; Copyright (c) 1986-1993 Kenneth D. Forbus, Johan de Kleer and 
;;; Xerox Corporation.  All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Load the file atms
;;; Load the file atcon
;;; Load the file gde
;;; Load the file condef

(defun standard-poly ()
  (setq *atcon* (create-atcon "Poly"))
  (create 'p 'poly)
  (set-parameter (>> a p) 3)
  (set-parameter (>> b p) 2)
  (set-parameter (>> c p) 2)
  (set-parameter (>> d p) 3)
  (set-parameter (>> e p) 3)
  (format T "~% Measured f to be 10")
  (set-parameter (>> f p) 10)
  (print-minimal-conflicts)
  (print-minimal-diagnoses)
  (print-smallest-diagnoses)
  (score-measurements (smallest-diagnoses))
  (format T "~% Measured g to be 12")
  (set-parameter (>> g p) 12)
  (print-minimal-conflicts)
  (print-minimal-diagnoses)
  (print-smallest-diagnoses)
  (score-measurements (smallest-diagnoses))
  'DONE)

(defun ole-string (node &aux value)
  (setq value (tms-node-datum node))
  (cond ((stringp value) value)
	((value-string value))
	((eq (cell-name (value-cell value)) 'OK)
	 (format nil "~A" (constraint-pretty-name
			    (cell-owner (value-cell value)))))
	(t (format nil "~A = ~A" (cell-pretty-name (value-cell value))
		   (value-datum value)))))


(defun standard-2bit ()
  (setq *atcon* (create-atcon "ole"))
  (create 'add '2-bit-adder)
  (change-atms (atcon-atms *atcon*) :node-string 'ole-string)
  (set-parameter (>> a bit0 add) 0)
  (set-parameter (>> b bit0 add) 0)
  (set-parameter (>> a bit1 add) 0)
  (set-parameter (>> b bit1 add) 0)
  (set-parameter (>> ci bit0 add) 0)
  (set-parameter (>> q bit1 add) 1)
;  (assume-parameter (>> co bit1 add) 1)
  (diagnose))
