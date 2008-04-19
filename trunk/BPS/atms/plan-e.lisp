;; -*- Mode: Lisp; -*- 

;;;; ATMS-based Envisioner for planning problems
;; Last edited: 1/29/93, KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun envision (&optional (*plnpr* *plnpr*)
		 &aux states)
  (setq states (solutions (plnpr-atre *plnpr*)
			  (plnpr-basis-set *plnpr*)))
  (setf (getf (plnpr-plist *plnpr*) :STATES) states)
  (setf (getf (plnpr-plist *plnpr*) :TRANSITIONS)
	(apply-all-operators states)))

(defun apply-all-operators (states)
  (mapcar
   #'(lambda (state &aux entry)
       (dolist (op-inst (find-applicable-operators state)
			entry)
        (push (cons op-inst (apply-operator state op-inst))
	      entry))
       (push state entry)) states))

(defun show-envisionment (&optional (*plnpr* *plnpr*)
			    (stream *standard-input*)
			  &aux states trans-table)
  (setq states (getf (plnpr-plist *plnpr*) :STATES))
  (cond ((null states) 
	 (format stream "~%The state space is empty."))
	(t (format stream
		   "~% ~D states have been generated:"
		   (length states))
	   (dolist (state states)
	     (print-env state stream))
	   (format stream "~%Transition Table:")
	   (setq trans-table
		 (getf (plnpr-plist *plnpr*) :TRANSITIONS))
	   (if (null trans-table) (format stream " empty.")
	     (dolist (state-entry trans-table)
	      (format stream "~%  ~A: " (car state-entry))
	      (dolist (pair (cdr state-entry))
               (format stream "~%   ~A -> ~A"
		       (car pair) (cdr pair))))))))

;;;; Finding plans by searching the envisionment

(defun find-plan (start goals &optional (*plnpr* *plnpr*))
  (let ((goal-states (fetch-states goals))
	(start-states (fetch-states start)))
    (debug-plnpr t "~%Initial states are ~A." start-states)
    (debug-plnpr t "~%Goal states are ~A." goal-states)
    (do ((queue (mapcar #'(lambda (state)
			    (list state)) start-states)
		(nconc (cdr queue) new-sprouts))
	 (new-sprouts nil nil)
	 (transitions (getf (plnpr-plist *plnpr*)
			    :TRANSITIONS))
	 (found? nil))
	((or found? (null queue))
	 (setf (getf (plnpr-plist *plnpr*) :PLAN) found?))
      (cond ((member (caar queue) goal-states) ;got it
	     (setq found? (car queue)))
	    (t (dolist (transition
			 (cdr (assoc (caar queue)
				     transitions)))
		 (unless (member (cdr transition)
				 (cdar queue)) ;avoid loops
		  (debug-plnpr t
	            "~% Can reach ~A via ~A from ~A."
		    (cdr transition) (car transition)
		    (caar queue))
		   (push (cons (cdr transition)
			       (cons (car transition)
				     (car queue)))
			 new-sprouts))))))))