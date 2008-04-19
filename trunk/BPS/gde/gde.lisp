;-*- Mode: LISP; Syntax: Common-lisp; Package: USER-*-

;; TGDE --- Trivial General Diagnostic Engine.
;; Version 1 of 12/27/91.  Requires ATCON Version 2.
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.
 
;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun print-minimal-conflicts ()
  (dolist (conflicts (atms-nogood-table (atcon-atms *atcon*)))
    (dolist (conflict (cdr conflicts))
      (format T "~% Minimal conflict: ~A" (env-string conflict)))))

;;; Print in standard format.
(defun print-minimal-diagnoses ()
  (let* ((atms (atcon-atms *atcon*))
	 (diagnoses (interpretations atms nil (atms-assumptions atms))))
    (format T "~%There are ~D minimal diagnoses:" (length diagnoses))
    (mapc #'print-diagnosis diagnoses)))

(defun smallest-diagnoses (&aux atms diagnoses (smallest-size 0))
  (setq atms (atcon-atms *atcon*)
	diagnoses (interpretations atms nil (atms-assumptions atms)))
  (unless diagnoses (return-from smallest-diagnoses nil))
  (dolist (diagnosis diagnoses)
    (if (> (env-count diagnosis) smallest-size)
	(setq smallest-size (env-count diagnosis))))
  (remove-if #'(lambda (env) (< (env-count env) smallest-size))
	     diagnoses))

(defun print-smallest-diagnoses (&aux diagnoses)
  (setq diagnoses (smallest-diagnoses))
  (format T "~%There are ~D minimum cardinality diagnoses:"
	  (length diagnoses))
  (mapc #'print-diagnosis diagnoses))

(defun print-diagnosis (env &aux assumptions faults printer atms)
  (setq assumptions (env-assumptions env)
	atms (atcon-atms *atcon*)
	printer (atms-node-string atms))
  (dolist (a (atms-assumptions (atcon-atms *atcon*)))
    (unless (member a assumptions :test #'eq)
      (push (funcall printer a) faults)))
  (format T "~%{~{~A~^,~}}" (sort faults #'string-lessp)))

(defun score-measurements (diagnoses &aux probes)
  (dolist (cell (atcon-cells *atcon*))
    (unless (eq (cell-name cell) 'OK)
      (push (cons cell (compute-cost cell diagnoses)) probes)))
  (setq probes (sort probes '< :key #'cdr))
  (dolist (probe probes)
    (format T "~%Measuring ~A has cost ~A" (car probe) (cdr probe)))
  probes)

(defun compute-cost (cell diagnoses
		     &aux (cost 0) count (misses 0) (base 0))
  (when (> (length (cell-domain cell)) 0)
    (dolist (d diagnoses)
      (unless (dolist (node (cell-nodes cell))
		(if (in-node? node d) (return T)))
	(incf misses)))
    (setq base (/ misses (float (length (cell-domain cell))))))
  (dolist (node (cell-nodes cell))
    (setq count base)
    (dolist (diagnosis diagnoses)
      (if (in-node? node diagnosis) (incf count)))
    (unless (= count 0) (incf cost (* count (log count 2)))))
  cost)

(defun diagnose (&aux diagnoses probes result probe)
  (do nil (nil)
    (setq diagnoses (smallest-diagnoses))
    (when (null (cdr diagnoses))
      (format T "~%Correct diagnosis is: ")
      (print-diagnosis (car diagnoses))
      (return nil))
    (format T "~%There are ~D minimum cardinality diagnoses:"
	    (length diagnoses))
    (mapc #'print-diagnosis diagnoses)
    (setq probes (score-measurements diagnoses))
    (do ((probes probes (cdr probes)))
	((null (cdr probes)))
      (unless (= (cdar probes) (cdadr probes))
	(rplacd probes nil)))
    (cond ((cdr probes)
	   (do nil (nil)
	       (do ((j 0 (1+ j))
		    (probes probes (cdr probes)))
		   ((null probes))
		 (format T "~%~D : ~A" j (caar probes)))
	     (format T "~%Enter integer of point measured: ")
	     (if (setq probe (nth (print (read)) probes)) (return T))))
	  (t (setq probe (car probes))))
    (format T "~%Please enter result of measuring ~A:" (car probe))
    (setq result (read))
    (set-parameter (car probe) result)))