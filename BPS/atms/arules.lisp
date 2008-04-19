;; -*- Mode: Lisp; -*-

;;; Tiny Rule Engine, ATMS interface: Rules module
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;; University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *atre* *rule-procedures* *bound-vars*
		    *in-nodes* *imp-nodes*))

(defvar *bound-vars* nil)      ;; Tracks lexical environment
(defvar *rule-procedures* nil) ;; while defining rules
(defvar *in-nodes* nil)        ;; Part of rule triggering
(defvar *imp-nodes* nil)       ;; environment.

(defvar *file-counter* 0)
(defvar *file-prefix* "")

(defmacro Rule-File (prefix)
  `(eval-when (compile load eval)
     (setq *file-counter* 0)
     (setq *file-prefix* ,prefix)))

;;;; Defining rules
;;; <condition> =  :INTERN | :IN | :IMPLIED-BY
;;; If you want to mix trigger types, use nested calls to RULE.
;; Trigger syntax is 
;; (<pattern1> . <options for pattern1>
;;     <pattern2> <options for pattern2> ...)
;;  and <options> can be empty, or 
;;  :TEST <code> and/or :VAR <var>, where <code> must be
;;  non-nil for the match to succeed, and <var> will be
;;  bound to the whole pattern.
;; e.g., ((Queen ?x1 ?y1) :VAR ?f1
;;        (Queen ?x1 ?y2) :VAR ?f2 :TEST (not (= ?y1 ?y2)))

(defmacro rule (condition trigger-list &rest body)
  (do-rule condition (parse-triggers trigger-list) body))

(defun parse-triggers (trigger-list)
  (cond ((null trigger-list) nil)
	(t (multiple-value-bind (var test new-triggers)
	    (parse-trigger-options (cdr trigger-list) nil nil)
	    (cons (list (car trigger-list) var test)
		  (parse-triggers new-triggers))))))

(defun parse-trigger-options (triggers var test)
  (case (car triggers)
	(:VAR (parse-trigger-options
	       (cddr triggers) (cadr triggers) test))
	(:TEST (parse-trigger-options
		(cddr triggers) var (cadr triggers)))
	(t (values var test triggers))))

;;;; Orchestrating the rule expansion

(defun do-rule (condition triggers body)
  (let ((*rule-procedures* nil)
	(*bound-vars* nil)
	(index-form nil))
    (setq index-form
	  (build-rule 
	   condition (car triggers)
	   (subst 'internal-rule 'rule
		  (make-nested-rule
		   condition (cdr triggers) body))))
  `(progn ,@ *rule-procedures* ,index-form)))

(defmacro internal-rule (condition triggers-in &rest body)
  (let ((triggers (parse-triggers triggers-in)))
    `(add-internal-rule
      ,condition ,(car triggers)
      ,(make-nested-rule condition (cdr triggers) body))))

(defun make-nested-rule (condition triggers body)
  (cond ((null triggers) body)
	(t `((add-internal-rule ,condition
	       ,(car triggers)
	       ,(make-nested-rule
		 condition (cdr triggers) body))))))

(defmacro add-internal-rule (condition trigger body)
  (build-rule condition trigger body))

;;;; Building rules

(defun build-rule (condition trigger body
			     &aux match-procedure
			     body-procedure)
  (let ((pattern (car trigger))
	(var (cadr trigger))
	(test (caddr trigger)))
   (setq match-procedure
	 (generate-match-procedure
	  pattern var test condition))   
   (setq body-procedure
	 (generate-body-procedure
	  pattern condition var body))
   (push match-procedure *rule-procedures*)
   (push body-procedure *rule-procedures*)
   `(insert-rule
     (get-dbclass ,(get-trigger-dbclass pattern))
     ;return form to index rule
       ,(if *bound-vars* ;the match function for rule
	    `(function (lambda (p)
	       (,(cadr match-procedure) p ,@ *bound-vars*)))
	  `(function ,(cadr match-procedure)))
     (function ;;the body function for rule
       ,(if (or *bound-vars*
		(not (eq condition :INTERN)))
	    (let ((tv (nreverse
			(pattern-free-variables trigger))))
	      (unless (eq condition :INTERN)
		      (push 'TRIGGER-NODE tv))
	      `(lambda ,tv
		 (,(cadr body-procedure) ,@ tv
		  ;(fn-name parameters)
		  ,@ (scratchout tv *bound-vars*))))
	      (cadr body-procedure)))
     *in-nodes* 
     *imp-nodes*))) 

(defun get-trigger-dbclass (trigger)
  (cond ((null trigger) (error "Null trigger in ATRE rule"))
	((variable? trigger)
	 (if (member trigger *bound-vars*)  trigger
	     (error "~%Trigger dbclass is unbound -- ~A."
		    trigger)))
	((symbolp trigger)  (list 'QUOTE trigger))
	((listp trigger) (get-trigger-dbclass (car trigger)))
	(t (error
  "ATRE rule trigger must be symbol or list: ~A" trigger))))

;;;; Generating the body procedure

(defmacro with-pushed-variable-bindings (new-bindings
					  &rest body)
;; generate-body-procedure needs this
  `(let ((*bound-vars* (append ,new-bindings
			       (scratchout ,new-bindings
					   *bound-vars*))))
     ,@ body))

(defun generate-body-procedure (pattern condition var body
				    &aux newly-bound env fname)
  (setq newly-bound (pattern-free-variables pattern))
  (if var (push var newly-bound))
  (setq body (with-pushed-variable-bindings
	       newly-bound (fully-expand-body body)))
  (setq env (append newly-bound
		    (scratchout newly-bound *bound-vars*)))
  (unless (eq condition :INTERN) (push 'trigger-node env))
  (setq fname (generate-rule-procedure-name pattern))
  `(defun ,fname ,env
     ,@ (cond ((eq condition :INTERN) body) ;; Just do it
	      (t ;; Must check and see if the node's belief state
	         ;; matches the rule's requirements
	       `((cond ((,(case condition
			   (:IN 'tms-node-label)
			   (:IMPLIED-BY 'tms-node-label)
			   (t (error "~A bad condition -- GBF"
				     condition)))
			 TRIGGER-NODE) ,@ body)
	           (t (push (list ',fname ,@ env)
			    (tms-node-rules TRIGGER-NODE)))))))))

(defun generate-match-procedure (pattern var test condition)
  (multiple-value-bind (tests binding-specs)
   (generate-match-body
    pattern (pattern-free-variables pattern) test)
   `(defun ,(generate-rule-procedure-name pattern)
      (P ,@ *bound-vars*)
       ;;first arg, P, is the pattern
       (if (and ,@ tests)
	   (values
	    T ,(if (and (null var) (null binding-specs)) nil
		 `(list ,@ (if var '(P))
			,@ (reverse binding-specs)))
		   ,condition)))))

(defun scratchout (l1 l2)
  ;non-destructive and order-preserving
  (dolist (el1 l1 l2) (setq l2 (remove el1 l2))))

(defun generate-rule-procedure-name (pattern)
  (intern (format nil "~A-~A-~A" 
		  *file-prefix* pattern (incf *file-counter*))))

;;;; Recursive macroexpansion

(defvar *macros-to-expand*
  '(rule internal-rule add-internal-rule with-pushed-variable-bindings
    rlet rassert! rnogood! with-focus with-ATRE))

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


;;;; Running rules

(defun insert-rule (dbclass matcher body in-nodes imp-nodes
			    &aux rule atre)
  (setq atre (dbclass-atre dbclass))
  (setq rule (make-rule
	      :MATCHER matcher
	      :ATRE atre
	      :BODY body
	      :DBCLASS dbclass
	      :COUNTER (incf (atre-rule-counter atre))
	      :IN-NODES in-nodes
	      :IMP-NODES imp-nodes))
  ;; Index it
  (push rule (atre-rules atre))
  (push rule (dbclass-rules dbclass))
  (dolist (candidate (dbclass-facts dbclass))
	  (try-rule-on rule candidate)))

(defun try-rules (datum)
  (dolist (rule (dbclass-rules (datum-dbclass datum)))
    (try-rule-on rule datum)))

(defun try-rule-on (rule datum &aux a)
  (setq a (datum-atre datum))
  (multiple-value-bind (okay? bindings condition)
      (funcall (rule-matcher rule) (datum-lisp-form datum))
    (when okay?
      (when (or (eq condition :IN)
		(eq condition :IMPLIED-BY))
	(push (datum-tms-node datum) bindings))
      (enqueue (list (rule-body rule) bindings
		     (case condition
		       (:IN (cons
			     (cons (datum-tms-node datum)
				   (rule-in-nodes rule))
			     (rule-imp-nodes rule)))
		       (:IMPLIED-BY
			(cons (rule-in-nodes rule)
			      (cons (datum-tms-node datum)
				    (rule-imp-nodes rule))))
		       (:INTERN
			(cons (rule-in-nodes rule)
			      (rule-imp-nodes rule)))))
	        a))))

(defun run-rules (&optional (*atre* *atre*))
  (setf (atre-queue *atre*)
	(nconc (atre-queue *atre*)
	       (atre-in-rules *atre*)))
  (setf (atre-in-rules *atre*) nil)
  (do ((form (dequeue *atre*) (dequeue *atre*))
       (counter 0 (1+ counter)))
      ((null form)
       (debugging-atre "~%    ~A rules run."  counter)
       (values counter
	       (incf (atre-rules-run *atre*) counter)))
      (execute-rule form *atre*)))

;;;; Executing rules, checking for appropriate conditions

(defun execute-rule (queued-rule atre)
  ;; Now is (<procedure> <arguments> <node list>)
  ;; Check the node list before executing, to make sure
  ;; all the belief conditions are satisifed.
  (let ((*in-nodes* (car (third queued-rule)))
	(*imp-nodes* (cdr (third queued-rule))))
    (unless (in-triggers-ready? *in-nodes* atre)
      ;; Re-queue under ATRE for checking.
      ;; ****** Introduce temporary nodes?  Cache rules
      ;; ****** on justifications?
      (push queued-rule (atre-in-rules atre))
      (return-from EXECUTE-RULE nil))
    (unless (implied-by-triggers-ready? *imp-nodes* atre)
      (push queued-rule (atre-imp-rules atre))
      (return-from EXECUTE-RULE nil))
    ;; Let's do it
    (apply (car queued-rule) (cadr queued-rule))))

(defun in-triggers-ready?
  (nodes atre &optional (env (atms-empty-env
			      (atre-atms atre))))
  (cond ((env-nogood? env) nil) ;; Combination was nogood
	((null nodes) t) ;; Nothing else to combine
	(t (dolist (new (tms-node-label (car nodes)))
	     (let ((u (union-env new env)))
	       (if (in-triggers-ready? (cdr nodes) atre u)
		   (return-from IN-TRIGGERS-READY? t)))))))

(defun implied-by-triggers-ready? (nodes atre)
  (or (null nodes) ;; No triggers, no problem
      (and (focus-okay? atre)
	   (every #'(lambda (n) 
		      (in-node? n (atre-focus atre)))
		  nodes))))

(defun rules-waiting? (atre) (atre-queue atre))

(defun enqueue (new a) (push new (atre-queue a)))

(defun dequeue (atre)
  (if (atre-queue atre) (pop (atre-queue atre))))

;;;; Display routines

(defun show-rules (&optional (atre *atre*)
			     (stream *standard-output*)
			&aux counter dist inc imp in
			queued)
  (setq counter 0)
  (dolist (dbclass (atre-dbclasses atre))
    (setq inc (length (dbclass-rules dbclass)))
    (when (> inc 0)
      (push (cons (dbclass-name dbclass) inc) dist)
      (incf counter inc)))
  (setq in (length (atre-in-rules atre))
	imp (length (atre-imp-rules atre))
	contra 0 queued (length (atre-queue atre)))
  (setq counter (+ in imp counter))
  (format stream "~% ~A has ~D rules in all."
	  (atre-title atre) counter)
  (format stream "~%  ~A queued."
	  (if (> queued 0) queued "None"))
  (if (> (+ in imp contra) 0)
      (format stream "  Pending: ~A in, ~A implied-by."
	      (if (> in 0) in "No") (if (> imp 0) imp "No"))
      (format stream "  None pending."))
  (when dist
    (format stream "~% Cached under dbclasses:")
    (dolist (entry dist)
      (format stream "~%    ~A: ~D"
	      (car entry) (cdr entry))))
  atre)

(defun print-rules (&optional (atre *atre*)
			      (stream *standard-output*)
			      &aux counter)
  (setq counter 0)
  (format t "~%The rules in ~A are:" (atre-title atre))
  (dolist (rule (atre-rules atre))
	  (incf counter)
	  (print-rule rule stream))
    counter)

(defun print-rule
  (rule &optional (stream *standard-output*))
  (format stream "~% ~A: ~A, ~A"
	  rule (rule-matcher rule) (rule-body rule)))

(defun test-rule-expansion ()
 (pprint (macroexpand
	  '(rule :IN  ((implies ?p ?q) :VAR ?f1 ?p)
		 (rassert! ?q (:CE ?f1 ?p))))))

(defun get-rule (num &optional (atre *atre*))
  (dolist (rule (atre-rules atre))
    (when (= (rule-counter rule) num)
	  (return-from GET-RULE rule))))
