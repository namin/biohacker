;; -*- Mode: Lisp; -*-

;;;; Candidate generation via constraint suspension
;; Last edited: 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;;; Candidate generation algorithm

(defun generate-candidates (*tcon* inputs outputs
                            &optional (debugging? nil)
		            &aux diffs candidates
			    hypotheses result)
  ;; Step 1: Insert inputs and collect discrepancies
  (setup-inputs inputs *tcon*)
  (setq diffs (find-discrepancies inputs outputs *tcon*))
  (when debugging? (format T "~% ~D discrepancies found."
			   (length diffs))
   (when diffs
    (dolist (diff diffs)
     (format t "~%  ~A was ~A, should be ~A."
	     (caar diff) (cdr diff) (cdar diff)))))
  (unless diffs (return-from generate-candidates :OKAY))
  ;; Step 2: Select initial candidates
  (do ((ds diffs (cdr ds))
       (contributors nil)
       (any? nil))
      ((null ds))
      (setq contributors 
	    (find-contributors (car ds) debugging?))
      (if any?
	  (setq candidates
		(intersection contributors candidates))
	(setq candidates contributors any? t)))
  (when debugging? 
   (format t "~% Candidates are ~A." candidates)) 
  ;; Step 3: Evaluate candidate consistency
  (dolist (candidate candidates)
   (when (setq result
	       (evaluate-candidate candidate outputs
				   debugging?))
	 (push result hypotheses)))
  (retract-inputs (mapcar 'car inputs))
  hypotheses)

;;;; Network manipulations and discrepancy finding

(defun setup-inputs (inputs tcon)
 (dolist (input-entry inputs)
  (set! (eval (car input-entry)) 
	(cdr input-entry) tcon))
 (enforce-constraints tcon))

(defun retract-inputs (inputs &aux cell)
 (dolist (cspec inputs)
  (setq cell (eval cspec))
  (forget! cell (cell-tcon cell)))
 (enforce-constraints (cell-tcon cell)))

(defun find-discrepancies (inputs outputs *tcon*
				  &aux cell diffs)
 (dolist (output-entry outputs diffs)
  (setq cell (eval (car output-entry)))
  (cond ((not (known? cell))
	 (error "~A not determined by inputs ~A."
		(cell-pretty-name cell)
		(mapcar 'car inputs)))
	((coincidence? (cell-value cell) 
		       (cdr output-entry) *tcon*))
	(t (push (cons output-entry (cell-value cell))
		 diffs)))))

;;;; Support for candidate generation

(defun find-contributors (diff &optional (debugging? nil)
			       &aux candidates cell marker)
  (setq cell (eval (caar diff))
	marker (list cell))
  (when (or (not (known? cell))
	    (ground-justification?
	     (cell-informant cell)))
   (error "Output ~A not computed!" cell))
  (do ((queue (list cell)
	      (nconc (cdr queue) new-cells))
       (qc nil)
       (new-cells nil nil))
      ((null queue)
       (if debugging?
	   (format t
  "~% Contributors to ~A being ~A instead of ~A: ~%    ~A"
  (caar diff) (cdr diff) (cdar diff) candidates))
       candidates)
   (setq qc (car queue))      
   (unless (eq (getf (cell-plist qc) :MARKER) marker)
    (setf (getf (cell-plist qc) :MARKER) marker)
    (unless (ground-justification? (cell-informant qc))
     (unless (eq (constraint-name (cdr (cell-informant qc)))
		 '1<=>2)
	     (pushnew (cdr (cell-informant qc)) candidates))
     (setq new-cells (rule-uses (cell-informant qc)))))))

(defun evaluate-candidate (candidate outputs
			     &optional (debugging? nil)
			     &aux result)
 (when debugging? (format t "~% Suspending ~A.." candidate))
 (suspend-constraint candidate)
 (unwind-protect 
  (setq result
   (catch 'eval-candidate-tag
    (with-contradiction-handler *tcon*
	'suspension-contra-handler
	(dolist (output-entry outputs)
		(set! (eval (car output-entry))
		      (cdr output-entry) *tcon*))
	(enforce-constraints *tcon*)
	;; If here, must not have got a contradiction
	;; so record symptoms
	(delete nil 
		(mapcar #'(lambda (cell)
			   (if (known? cell)
			       (cons cell
				     (cell-value cell))))
			(constraint-cells candidate))))))
     (progn (retract-inputs (mapcar 'car outputs))
	    (unsuspend-constraint candidate)))
 (if debugging? 
     (if result (format t "~% .. ~A possible." candidate)
       (format t "~% .. ~A exonerated." candidate)))
 (if result (cons candidate result)))

(defun suspension-contra-handler (cell newval newsetter tcon)
  (declare (ignore cell newval newsetter tcon))
  (throw 'eval-candidate-tag nil))

;;;; Constraint suspension

(defun suspend-constraint (con)
 ;; Retracts conclusions from the constraint and
 ;; temporarily unwires it from the network.
  (suspend-constraint1 con)
  (enforce-constraints (constraint-tcon con)))

(defun suspend-constraint1 (con &aux part)
 (dolist (part-entry (constraint-parts con))
  (cond ((cell? (setq part (cdr part-entry)))
	 (unwire-cell part con))
	(t (suspend-constraint1 part)))))

(defun unwire-cell (cell con &aux role)
 ;; Retract if necessary and then unwire it
  (if (and (known? cell)
	   (listp (cell-informant cell))
	   (eq (cdr (cell-informant cell)) con))
      (forget! cell (cell-informant cell)))
  (setq role (rassoc con (cell-roles cell)))
  (unless role 
   (error "~A not part of ~A!" cell con))
  (setf (cell-roles cell)
   (delete role (cell-roles cell) :COUNT 1))
  (setf (getf (cell-plist cell) :SUSPENDED) role))

(defun unsuspend-constraint (con)
  (unsuspend-constraint1 con)
  (enforce-constraints (constraint-tcon con)))

(defun unsuspend-constraint1 (con &aux part)
 (dolist (part-entry (constraint-parts con))
  (cond ((cell? (setq part (cdr part-entry)))
	 (rewire-cell part con))
	(t (unsuspend-constraint1 part)))))

(defun rewire-cell (cell con &aux role)
 (setq role (getf (cell-plist cell) :SUSPENDED))
 (unless role
  (error "~A of ~A not really suspended." cell con))
 (setf (getf (cell-plist cell) :SUSPENDED) nil)
 (push role (cell-roles cell))
 (beg! cell))

;;;; Test procedure

(defvar *suspend-file*
  #+ILS "/u/bps/code/tcon/polybox"
  #+PARC "virgo:/virgo/dekleer/bps/code/tcon/polybox"
  #+MCL "Macintosh HD:BPS:tcon:polybox"
  #+ACLPC "E:\\code\\tcon\\polybox")

(defun setup-cs-example (&optional (debugging? nil))
  (create-tcon "Polybox Example" :DEBUGGING debugging?
	       :PROTOTYPE-FILE *suspend-file*)
  (create 'ex 'Polybox-Example))

(defvar *in1* '(((>> a ex) . 3)((>> b ex) . 2)
		((>> c ex) . 2)((>> d ex) . 3)
		((>> e ex) . 3)))

(defvar *out1* '(((>> f ex) . 10)((>> g ex) . 12)))

(defvar *out2* '(((>> f ex) . 3)((>> g ex) . 3)))

(defvar *out3* '(((>> F EX) . 8) ((>> G EX) . 16)))

(defun test-constraint-suspension
  (&optional (debugging? nil)
	     (inputs *in1*)
	     (outputs *out1*))
  (setup-cs-example debugging?)
  (generate-candidates *tcon* inputs outputs t))

