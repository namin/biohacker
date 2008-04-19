;; -*- Mode: Lisp; -*-

;;;; ATMS-based planner using ATRE + ATMS
;; Last edited: 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (plnpr (:PREDICATE plnpr?)
		  (:PRINT-FUNCTION print-plnpr))
  (title "") ;; String for printing 
  (atre nil) ;; ATRE it uses
  (basis-set nil) ;; choice sets defining states
  (operators nil) ;; List of allowed operators
  (plist nil))    ;; Cache for intermediate results
  
(defun print-plnpr (p st ignore) (declare (ignore ignore))
  (format st "<PlnPr: ~A>" (plnpr-title p)))

(defvar *plnpr* nil)

(defmacro With-PlnPr (plnpr &rest forms)
  `(let ((*plnpr* ,plnpr)) ,@ forms))

(defun in-plnpr (x) (setq *plnpr* x))

(defmacro debug-plnpr (stream format-string &rest args)
  `(if (getf (plnpr-plist *plnpr*) :DEBUGGING)
       (format ,stream ,format-string ,@ args)))

(defun create-planning-problem (title basis-set
				      &aux plnpr)
  (setq plnpr (make-plnpr
	       :TITLE title 
	       :ATRE (create-atre
		      (format nil "ATRE(~A)" title))
	       :BASIS-SET basis-set))
  (in-plnpr plnpr))

(defun setup-choice-sets (&optional (*plnpr* *plnpr*)
				    &aux informant) 
  (setq informant
	(intern (format nil "BASIS SET(~A)"
			(plnpr-title *plnpr*))
		'keyword))
  (with-atre (plnpr-atre *plnpr*)
     (dolist (choice-set (plnpr-basis-set *plnpr*))
      (dolist (choice choice-set)
       (assume-if-needed choice informant)))
     (run-rules)))

(defun set-debug-plnpr (state &optional (*plnpr* *plnpr*))
  (setf (getf (plnpr-plist *plnpr*) :DEBUGGING) state))

;;;; Defining operators

(defstruct (operator
	    (:PREDICATE Operator?)
	    (:PRINT-FUNCTION operator-printer))
  (form nil)
  (preconditions nil)
  (add-list nil)
  (delete-list nil))

(defun operator-printer (n st ignore)
  (declare (ignore ignore))
  (format st "<Operator ~A>" (operator-form n)))

(defmacro DefOperator (form &rest keywords-and-values)
  (let ((test (cadr (member :TEST keywords-and-values)))
	(preconditions
	  (cadr (member :PRECONDITIONS keywords-and-values)))
	(add-list
	  (cadr (member :ADD-LIST keywords-and-values)))
	(delete-list
	  (cadr (member :DELETE-LIST keywords-and-values))))
    `(progn (let ((entry (assoc ',(car form)
				(plnpr-operators *plnpr*)))
		  (op (make-operator
			:FORM ',form
			:PRECONDITIONS ',preconditions
			:ADD-LIST ',add-list
			:DELETE-LIST ',delete-list)))
	      (cond (entry (setf (cdr entry) op))
		    (t (push (cons ',(car form) op)
			     (plnpr-operators *plnpr*)))))
    ;; make rule that determines when it is applicable
     (rule :INTERN ,preconditions
	,(cond (test 
		`(when ,test
		  (rassert! (applicable ,form)
		    (:OP-PCS-SATISFIED ,@ preconditions))))
	       (t `(rassert! (applicable ,form)
		     (:OP-PCS-SATISFIED ,@ preconditions))))))))

(defun find-applicable-operators (state &optional (*plnpr* *plnpr*)
					&aux result)
  (dolist (candidate (fetch `(applicable ?x) (plnpr-atre *plnpr*))
		     result)
	  (if (in? candidate state)
	      (push (cadr candidate) result))))

(defun fetch-operator (op-name)
  (cdr (assoc op-name (plnpr-operators *plnpr*))))

;;;; Applying an operator to a state

(defun apply-operator (state op-inst)
  (let ((operator (fetch-operator (car op-inst)))
	(vals (cdr op-inst))
	(assumptions (env-assumptions state))
	(bindings nil)
	(add-list nil)
	(delete-list nil)
	(atms (atre-atms *atre*))) 
    ;;; First substitute the values for the variables and create
    ;;; the appropriate add list and delete list
    (setq bindings (mapcar #'(lambda (var val) (cons var val))
			   (cdr (operator-form operator)) vals))
    (setq add-list (sublis bindings
			   (operator-add-list operator)))
    (setq delete-list (sublis bindings
			      (operator-delete-list operator)))
    (debug-plnpr t "~%   Applying ~A to ~A." op-inst state)
    (debug-plnpr t "~%      Add list: ~A" add-list)
    (debug-plnpr t "~%      Delete list: ~A" delete-list)
    ;;; Remove delete-list assumptions.
    (setq assumptions
	  (remove-if #'(lambda (a) 
			 (member (datum-lisp-form
				   (tms-node-datum a))
				 delete-list
				 :TEST #'equal)) assumptions))
    (dolist (new add-list)
      (setq assumptions
	    (ordered-insert (get-tms-node new) assumptions
			    #'assumption-order)))
    (find-or-make-env assumptions atms)))

;;;; Examining problem-related information

(defun fetch-states (facts &optional (*plnpr* *plnpr*))
  (solutions (plnpr-atre *plnpr*)
	     (append (mapcar #'list facts)
		     (plnpr-basis-set *plnpr*))))

(defun satisfies-goal? (state goals
			      &optional (*plnpr* *plnpr*))
 (let ((answer (catch 'got-one
		 (with-atre (plnpr-atre *plnpr*)
		     (check-goals goals state nil)))))
    (if (eq (car answer) :WINNER) (values t (cdr answer))
      (values nil nil))))

(defun check-goals (goals state bindings)
  (cond ((null goals) ;; Accept any solution
	 (throw 'got-one (cons :WINNER bindings)))
	(t (dolist (candidate (fetch (car goals)))
	    (when (in? candidate state)
             (let ((new-bindings
		    (unify (car goals) candidate
			    bindings)))
	       (unless (eq new-bindings :FAIL)
                (check-goals (cdr goals) state
			    new-bindings))))))))

(defun show-plan (plan)
  (do ((steps (reverse plan) (cddr steps)))
      ((null steps))
    (print-env (car steps))
    (when (cadr steps) (format t "~%  then, by ~A, "
			       (cadr steps)))))