;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;;; Indirect proof mechanism for LTRE
;;; Last Edited 1/29/93, by KDF

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *LTRE*))

(defun try-indirect-proof (fact &optional (*LTRE* *LTRE*))
  (unless (known? fact)
    (with-contradiction-handler (ltre-ltms *ltre*)
      #'(lambda (contradictions ltms &aux assumptions)
	  (setq assumptions
		(assumptions-of-clause
		 (car contradictions)))
	  (let ((the-node
		 (find (datum-tms-node (referent fact T))
		       assumptions)))
	    (when the-node
	      (let ((status (tms-node-label the-node)))
		(retract-assumption the-node)
		(add-nogood the-node status
			    assumptions)))))
      ;; Assume the negation
       (assuming `((:NOT ,fact)) *LTRE*
		 (run-rules)))
   (known? fact)))

;; Ex 4 in Chapter 10

(defun assumptions-and-clauses-of-clause (in-clause &aux)
  (do ((clause-queue (list in-clause)
		     (nconc (cdr clause-queue) new-clauses))
       (mark (list nil))
       (node nil)
       (new-clauses nil nil)
       (assumptions nil)
       (clauses (list in-clause)))
      ((null clause-queue) (values assumptions clauses))
    (dolist (term-pair (clause-literals (car clause-queue)))
      (setq node (car term-pair))
      (unless (eq (tms-node-mark node) mark)
	(unless (eq (tms-node-label node) (cdr term-pair))
	  (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node))
		 (push node assumptions))
		((null (tms-node-support node)) (ltms-error "Node is unknown" node))
		(t 
		 (push (tms-node-support node) clauses)
		 (push (tms-node-support node) new-clauses))))
	(setf (tms-node-mark node) mark)))))

(defun clause->formula (clause)
  (cons ':OR (mapcar #'(lambda (x)
			 (if (eq (cdr x) :FALSE) `(:NOT ,(car x))
			   (car x)))
		     (clause-literals clause))))

(defun try-indirect-proof-better (fact &optional (*LTRE* *LTRE*))
  (unless (known? fact)
    (with-contradiction-handler (ltre-ltms *ltre*)
      #'(lambda (contradictions ltms &aux contradiction ante-formulas formula)
	  (setq contradiction (car contradictions))
	  (multiple-value-bind 
	      (assumptions clauses) 
	      (assumptions-and-clauses-of-clause contradiction)
	    (let ((the-node
		   (find (datum-tms-node (referent fact T))
			 assumptions)))
	      (when the-node
		(let ((status (tms-node-label the-node)))
		  (retract-assumption the-node)
		  (setq ante-formulas 
			(mapcar #'clause->formula
				(remove-duplicates clauses)))
		  (setq formula 
			`(:IMPLIES (:AND ,@ante-formulas)
				   ,(ecase status
				      (:TRUE `(:NOT ,the-node))
				      (:FALSE the-node))))
		(add-nogood the-node status
			    assumptions
			    (list :IMPLIED-BY formula 'INDIRECT-PROOF)))))))
      ;; Assume the negation
       (assuming `((:NOT ,fact)) *LTRE*
		 (run-rules)))
   (known? fact)))

;;;; Example of indirect proof

(defun indirect-proof-example ()
  (in-ltre (create-ltre "Indirect Proof Example"))
  (assert! '(:OR p q) 'user)
  (assert! '(:IMPLIES p r) 'user)
  (assert! '(:IMPLIES q r) 'user)
  (known? 'r)
  (try-indirect-proof-better 'r)
  (known? 'r))


