;; -*- Mode: Lisp; -*-

;;;; Rule package for the Tiny Rule Engine
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Rules have the form (rule <trigger> . <body>).
;; Whenever a pattern arrives in the database which unifies with
;; <trigger>, the rule is queued for eventual execution.  When a
;; rule is dequeued, the body is evaluated in the environment
;; defined by the unification.  Importantly, rules can be nested.

(proclaim '(special *TRE* *ENV*)) 
(defvar *ENV* nil)		; Environment for rules

(defstruct (rule (:PRINT-FUNCTION (lambda (r st ignore)
				    (format st "<Rule ~D>"
					    (rule-counter r)))))
     counter		;Integer to provide unique "name"
     Dbclass		;Dbclass it is linked to.
     trigger		;pattern it runs on.
     body		;code to be evaluated in local environment.
     environment)	;binding envirionment.

;;;; Interface for rules

(defun show-rules (&optional (stream *standard-output*) &aux counter)
  (setq counter 0)
  (maphash #'(lambda (key dbclass)
	       (dolist (rule (dbclass-rules dbclass))
		       (incf counter)
		       (format stream "~%  ")
		       (print-rule rule stream)))
	   (tre-dbclass-table *TRE*))
  counter)

(defun print-rule (rule &optional (stream *standard-output*))
  (format stream "Rule ~A: ~A; ~A"       ;don't show body, too big
	  (rule-counter rule)
	  ;; Plug in the variables, to show how much has been done.
	  (sublis (rule-environment rule)
		  (rule-trigger rule))
	  (rule-environment rule)))

;;;; Building and installing rules

;; Sugar for the user (or other programs!)
(defmacro rule (trigger &rest body) `(add-rule ',trigger ',body))

(defun add-rule (trigger body &aux rule dbclass)
  ;; First build the struct
  (setq rule (make-rule :TRIGGER trigger
			:BODY body
			:COUNTER (incf (tre-rule-counter *TRE*))
			:ENVIRONMENT *ENV*))
  ;; Now index it
  (setq dbclass (get-dbclass trigger *TRE*))
  (push rule (dbclass-rules dbclass))
  (setf (rule-dbclass rule) dbclass)
  (debugging-tre "~% TRE: New rule: ~A" (print-rule rule nil))
  ;; Go into the database and see what it might trigger on.
  (dolist (candidate (get-candidates trigger *TRE*))
    (try-rule-on rule candidate *TRE*)))

(defun try-rules (fact tre)
  ;; This is called by the database system when it adds something.
  (dolist (rule (get-candidate-rules fact tre))
    (try-rule-on rule fact tre)))

(defun get-candidate-rules (fact tre)
  (dbclass-rules (get-dbclass fact tre)))

(defun try-rule-on (rule fact tre &aux bindings)
  ;; If the trigger matches, queue it up.
  (setq bindings (unify fact (rule-trigger rule)
			(rule-environment rule)))
  (unless (eq bindings :FAIL)
    (enqueue (cons (rule-body rule) bindings) tre)))

;;;; Executing rules

(defun run-rules (tre) ;; Called externally
    (do ((rule-pair (dequeue tre) (dequeue tre))
         (counter 0 (1+ counter)))
        ((null rule-pair)
	 (debugging-tre  "~%    ~A rules run."  counter))
        (run-rule rule-pair tre)))

;; Ideally, all rules triggered will be executed, and the
;; results will be independent of the order of execution.
;; Thus a simple LIFO queue suffices.

(defun enqueue (new tre) (push new (tre-queue tre)))
(defun dequeue (tre) (pop (tre-queue tre)))

(defun run-rule (pair tre)
  ;; Here pair is (<body> . <bindings>).  The LET makes
  ;; the bindings available to nested rules.
  (let ((*ENV* (cdr pair))
	(*TRE* tre))
    (incf (tre-rules-run tre))
    ;; Now we build a form that creates the right environment.
    ;; We will see better ways to do this later.
    (eval `(let ,(mapcar #'(lambda (binding)
			     `(,(car binding)
			       ',(sublis (cdr pair)
					 (cdr binding))))
			 (cdr pair))
	     ,@ (car pair)))))
