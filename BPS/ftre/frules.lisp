;; -*- Mode: Lisp; -*-

;;; This file is frules.lisp
;;;;  Modified: forbus on Tue Apr 2 10:19:28 1996

;;; Copyright (c) 1988-1991, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *ftre* *bound-vars* *rule-procedures*))

(defstruct (rule (:PRINT-FUNCTION ftre-rule-printer))
  id		        ;unique "name"
  Dbclass		;Dbclass it is linked to.
  matcher		;procedure that performs the match.
  body		        ;procedure that does the rule's work.
  assumption?)          ;Does it make an assumption?

(defun ftre-rule-printer (r st ignore)
  (declare (ignore ignore))
  (format st "<Rule ~D>" (rule-id r)))

(defvar *file-counter* 0)
(defvar *file-prefix* "")

(defmacro Rule-File (prefix)
  `(eval-when (compile load eval)
     (setq *file-counter* 0)
     (setq *file-prefix* ,prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building and installing rules 

(defmacro rule (triggers &rest body)
  (do-rule (parse-triggers triggers) body nil))
(defmacro a-rule (triggers &rest body)
  (do-rule (parse-triggers triggers) body T))
;; Restriction: An a-rule can have only one trigger!

(defun parse-triggers (trigger-list)
 (cond ((null trigger-list) nil)
       (t (multiple-value-bind (var test new-triggers)
	   (parse-trigger-options (cdr trigger-list)
				  nil nil)
	   (cons (list (car trigger-list) var test)
		 (parse-triggers new-triggers))))))

(defun parse-trigger-options (triggers var test)
  (case (car triggers)
	(:VAR (parse-trigger-options
	       (cddr triggers) (cadr triggers) test))
	(:TEST (parse-trigger-options
		(cddr triggers) var (cadr triggers)))
	(t (values var test triggers))))

(defun do-rule (triggers body asn?)
  (let ((*rule-procedures* nil)
	(*bound-vars* nil)
	(index-form nil))
    (when (and asn? (cdr triggers))
      (error
       "~% a-rules can only have one trigger:~%~A,~%~A."
	     triggers body))
    (setq index-form
	  (build-rule (car triggers)
		      (subst 'internal-rule
			     'rule
			     (make-nested-rule
			      (cdr triggers) body)) asn?))
  ;; Returning this ensures that all procedure definitions
  ;; are executed before any indexing occurs.
  `(progn ,@ *rule-procedures* ,index-form)))

(defmacro internal-rule (triggers-in &rest body)
  (let ((triggers (parse-triggers triggers-in)))
    `(add-internal-rule
      ,(car triggers)
      ,(make-nested-rule (cdr triggers) body))))

(defun make-nested-rule (triggers body)
  (cond ((null triggers) body)
	(t `((add-internal-rule
	       ,(car triggers)
	       ,(make-nested-rule (cdr triggers) body))))))

(defmacro add-internal-rule (trigger body)
  ;; The form to index this rule must appear in
  ;; the body of the rule which directly contains it.
  (build-rule trigger body nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building a rule

(defun build-rule (trigger body asn?
			   &aux match-procedure 
			   body-procedure)
 (multiple-value-bind (pattern var test)
		      (parse-rule-trigger trigger)
  (setq match-procedure
	(generate-match-procedure pattern var test))   
  (setq body-procedure
	(generate-body-procedure pattern var body))
  (push match-procedure *rule-procedures*)
  (push body-procedure *rule-procedures*)
  `(insert-rule
     (get-dbclass ,(get-trigger-dbclass pattern) *ftre*)
     ;return form to index rule
     (function ;the match procedure for rule
       ,(if *bound-vars*
	    `(lambda (p)
	       (,(cadr match-procedure) p ,@ *bound-vars*))
	  (cadr match-procedure)))
     (function ;;the body procedure
      ,(if *bound-vars*
	    (let ((tv (nreverse
			(pattern-free-variables trigger))))
	      `(lambda ,tv
		 (,(cadr body-procedure) ,@ tv
		  ;(fn-name parameters)
		  ,@ (scratchout tv *bound-vars*))))
	  (cadr body-procedure)))
     ,asn?)))

(defun parse-rule-trigger (trigger)
  ;; A trigger has the form (<pattern> <options>)
  ;;  where <options> can be empty, or 
  ;;  :TEST <code> and/or :VAR <var>, where <code> must be
  ;;  non-nil for the match to succeed, and <var> will be
  ;;  bound to the whole pattern.
  (cond ((variable? trigger) trigger)
	((listp trigger) (values-list trigger))
	(t (error "Invalid expression in trigger: ~A." trigger))))

(defun get-trigger-dbclass (trigger)
  (cond ((variable? trigger)
	 (if (member trigger *bound-vars*)  trigger
	     (error "~%Trigger dbclass is unbound -- ~A."
		    trigger)))
	((atom trigger)  (list 'QUOTE trigger))
	(t (get-trigger-dbclass (car trigger)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generating the body procedure

;;; Macro for generate-body-procedure
;;; (macros must be defined before use or compiler dies)
(defmacro with-pushed-variable-bindings (new-bindings
					  &rest body)
 `(let ((*bound-vars* (append ,new-bindings
		       (scratchout ,new-bindings
				   *bound-vars*))))
     ,@ body))

(defun generate-body-procedure (pattern var body
				&aux newly-bound env)
  (setq newly-bound (pattern-free-variables pattern))
  (if var (push var newly-bound))
  (setq body (with-pushed-variable-bindings
	       newly-bound (fully-expand-body body)))
  (setq env (append newly-bound
		    (scratchout newly-bound *bound-vars*)))
  `(defun ,(generate-rule-procedure-name pattern) ,env
     ,@ body))

(defun generate-match-procedure (pattern var test)
  (multiple-value-bind (tests binding-specs)
;; Construct a defun specialized to match the given pattern.
;; That procedure will return NIL if no match,
;;   (values T <binding-spec>) if match is successful.
        (generate-match-body 
	 pattern (pattern-free-variables pattern) test)
    `(defun ,(generate-rule-procedure-name pattern)
       (P ,@ *bound-vars*)
       ;;first arg, P, is the pattern
       (if (and ,@ tests)
	   (values T
	    ,(if (and (null var) (null binding-specs)) nil
	       `(list ,@ (if var '(P))
		      ,@ (reverse binding-specs))))))))

(defun scratchout (l1 l2)
  ;non-destructive and order-preserving
  (dolist (el1 l1 l2) (setq l2 (remove el1 l2))))

(defun generate-rule-procedure-name (pattern)
  (intern (format nil "~A-~A-~A" *file-prefix* pattern (incf *file-counter*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recursive macroexpansion

(proclaim '(special *macros-to-expand*))

(setq *macros-to-expand*
      '(rule a-rule rlet rassert! internal-rule add-internal-rule
	with-pushed-variable-bindings))

(defun fully-expand-body (body)
  (cond ((null body) nil)
	((not (listp body)) body)
	((symbolp (car body))
	 (cond ((member (car body) *macros-to-expand*)
		(fully-expand-body (macroexpand body)))
	       (t (cons (car body)
			(fully-expand-body (cdr body))))))
	(t (cons (fully-expand-body (car body))
		 (fully-expand-body (cdr body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Running rules

(defun insert-rule (dbclass matcher body asn? &aux rule)
  (with-ftre (dbclass-ftre dbclass)
   (setq rule (make-rule
	       :MATCHER matcher
	       :BODY body
	       :DBCLASS dbclass
	       :ID (incf (ftre-rule-counter *ftre*))
	       :ASSUMPTION? asn?))
  ;; Index it
  (cond ((= (ftre-depth *ftre*) 0)
	 (push rule (dbclass-rules dbclass)))
	(t (push rule (ftre-local-rules *ftre*))))
  (dolist (candidate
	   (get-candidates (dbclass-name dbclass) *ftre*))
    (try-rule-on rule candidate))))

(defun try-rules (fact ftre)
  (dolist (rule (get-candidate-rules fact ftre))
    (try-rule-on rule fact)))

(defun get-candidate-rules (fact ftre)
  (append (ftre-local-rules ftre)
	  (dbclass-rules (get-dbclass fact ftre))))

(defun try-rule-on (rule fact)
  (with-ftre (dbclass-ftre (rule-dbclass rule))
   (multiple-value-bind (okay? bindings)
     (funcall (rule-matcher rule) fact)
     (when okay?
	   (enqueue *ftre* (cons (rule-body rule) bindings)
		    (rule-assumption? rule))))))

(defun run-rules (*ftre*)
  (do ((form (dequeue *ftre*) (dequeue *ftre*))
       (counter 0 (1+ counter)))
      ((null form)
       (debugging-ftre "~%  ~A(~A): ~A rules run."
		       *ftre* (ftre-depth *ftre*) counter))
    (incf (ftre-rules-run *ftre*))
    (with-ftre *ftre* ;; Compare this to regular TRE!
	     (apply (car form) (cdr form)))))

(defun enqueue (ftre new asn?)
  (if asn? (push new (ftre-asn-queue ftre))
    (push new (ftre-normal-queue ftre))))

(defun dequeue (ftre)
  (if (ftre-normal-queue ftre)
      (pop (ftre-normal-queue ftre))
      (pop (ftre-asn-queue ftre))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Displaying rules
(defun show-rules (&optional (stream *standard-output*)
			     &aux counter)
  (setq counter 0)
  (format stream "~% In global context:")
  (maphash #'(lambda (key dbclass)
	       (dolist (rule (dbclass-rules dbclass))
		       (incf counter)
		       (print-rule rule stream)))
	   (ftre-dbclass-table *ftre*))
  (format stream "~%  ~D global rules." counter)
  (when (> (ftre-depth *ftre*) 0)
	(format stream "~% In current context:")
	(dolist (rule (reverse (ftre-local-rules *ftre*)))
		(unless (numberp rule) (incf counter))
		(print-rule rule stream)))
    counter)

(defun print-rule (rule &optional
			(stream *standard-output*))
  (format stream "~% ~A(~A): ~A, ~A"
	  rule (if (rule-assumption? rule)
		   "Y" "N")
	  (rule-matcher rule) (rule-body rule)))

(defun get-rule (id &optional (ftre *ftre*))
  (maphash #'(lambda (key dbclass)
	       (dolist (rule (dbclass-rules dbclass))
		 (when (= (rule-id rule) id)
		   (return-from get-rule (values rule)))))
	   (ftre-dbclass-table ftre)))
