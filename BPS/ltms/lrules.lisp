;; -*- Mode: Lisp; -*-

;;;; Rule package for LTRE
;;;; Last Edited 4/27/95, by KDF

;;; Copyright (c) 1986 - 1995 Kenneth D. Forbus,
;;; Northwestern University, and Johan de Kleer,  Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; The RULES field of the LTRE no longer exists, since 

(in-package :COMMON-LISP-USER)

(proclaim '(special *LTRE*))

(defstruct (rule (:PRINT-FUNCTION rule-print-procedure))
  counter      ; Unique ID for easy lookup
  ltre         ; The LTRE it is part of
  dbclass        ; Dbclass of associated pattern
  matcher      ; Procedure that performs the match
  body)        ; Procedure that does the rules' work

(defun rule-print-procedure (r st ignore)
  (declare (ignore ignore))
  (format st "<Rule ~D>" (rule-counter r)))

;;; The next three variable are used during rule construction.
(defvar *bound-vars* nil)     ;; Keeps track of lexical envirionment
(defvar *end-forms* nil)      ;; Caches procedure definitions
(defvar *rule-indexing* nil)  ;; Caches rule indexing forms.
;; For adding a file-specific prefix to internal procedure names.
(defvar *file-counter* 0)
(defvar *file-prefix* "")

(defmacro Rule-File (prefix)
  `(eval-when (compile load eval)
     (setq *file-counter* 0)
     (setq *file-prefix* ,prefix)))

;;;; Building and installing rules

(defmacro rule (triggers &rest body) (do-rule triggers body))

(defun do-rule (triggers body)
  (let ((*end-forms* nil)     ;; accumulates procedure definitions
	(*rule-indexing* nil) ;; accumulates indexing forms
	(*LTRE* *LTRE*))      ;; The LTRE the rule will use
    ;; Uncomment these if you have a serious rule expansion problem to track down.
    ;;(format t "~% Adding rule with triggers: ") (pprint triggers)
    ;;(format t "~% And body: ") (pprint body)
  (add-rule (car triggers)
	    (subst 'internal-rule
		   'rule
		   (make-nested-rule (cdr triggers) body)))
  ;; Returning this ensures that all procedure definitions
  ;; are executed before any indexing occurs.
  `(progn ,@ *end-forms* ,@ *rule-indexing*)))

(defmacro internal-rule (triggers &rest body)
  ;; All but the rule corresponding to the outermost
  ;; trigger are internal rules.
  `(add-internal-rule  ,(car triggers)
		       ,(make-nested-rule (cdr triggers) body)))

(defun make-nested-rule (triggers body)
  (cond ((null triggers) body)
	(t `((add-internal-rule ,(car triggers)
	       ,(make-nested-rule (cdr triggers) body))))))

(defun add-rule (trigger body)
  ;; Must be executed after functions are defined.
  (push (build-rule trigger body) *rule-indexing*)
  nil)

(defmacro add-internal-rule (trigger body)
  ;; The form to index this rule must appear in
  ;; the body of the rule which directly contains it.
  (build-rule trigger body))

;;;; Building a rule involves analyzing the trigger to see what
;;   special-purpose unifier is needed, constructing a function
;;   to do the work of the body, and building a form to index it.

(defun build-rule (trigger body &aux match-procedure body-procedure)
  (multiple-value-bind (pattern condition var test)
		       (parse-rule-trigger trigger)
   (setq match-procedure (generate-match-procedure pattern var test condition))   
   (setq body-procedure (generate-body-procedure pattern condition var body))
   (push match-procedure *end-forms*)
   (push body-procedure *end-forms*)
   `(insert-rule
     (get-dbclass ,(get-trigger-dbclass pattern))
     ;return form to index rule
     (function ;the match function for rule
       ,(if *bound-vars*
	    `(lambda (p)
	       (,(cadr match-procedure) p ,@ *bound-vars*))
	  (cadr match-procedure)))
     (function ;;the body function for rule
       ,(if (or *bound-vars*
		(not (eq condition :INTERN)))
	    (let ((tv (nreverse
			(pattern-free-variables trigger))))
	      (unless (eq condition :INTERN) (push 'TRIGGER-NODE tv))
	      `(lambda ,tv
		 (,(cadr body-procedure) ,@ tv
		  ;(fn-name parameters)
		  ,@ (scratchout tv *bound-vars*))))
	      (cadr body-procedure))))))

(defun parse-rule-trigger (trigger)
  ;; Trigger syntax is now (<condition> <pattern> .  <options>)
  ;; where <condition> =  :INTERN | :TRUE | :FALSE
  ;;  and <options> can be empty, or 
  ;;  :TEST <code> and/or :VAR <var>, where <code> must be
  ;;  non-nil for the match to succeed, and <var> will be
  ;;  bound to the whole pattern.
  (values (cadr trigger)
	  (cond ((member (car trigger) '(:INTERN :TRUE :FALSE)) (car trigger))
		(t (error "~% Unknown belief condition ~A in trigger ~A."
			  (car trigger) trigger)))
	  (cadr (member :VAR (cddr trigger)))
	  (cadr (member :TEST (cddr trigger)))))

(defun get-trigger-dbclass (trigger)
  (cond ((variable? trigger)
	 (if (member trigger *bound-vars*)  trigger
	     (error "~%Trigger dbclass is unbound -- ~A."
		    trigger)))
	((atom trigger)  (list 'QUOTE trigger))
	(t (get-trigger-dbclass (car trigger)))))

;;;; Generating the body function

;;; Macro for generate-body-procedure
;;; (macros must be defined before use or compiler dies)
(defmacro with-pushed-variable-bindings (new-bindings
					  &rest body)
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
	       `((cond ((,(ecase condition
				 (:TRUE 'true-node?)(:FALSE 'false-node?))
			 TRIGGER-NODE) ,@ body)
		       (t (push (list ',fname ,@ env)
				,(ecase condition
				       (:TRUE `(tms-node-true-rules TRIGGER-NODE))
				       (:FALSE `(tms-node-false-rules TRIGGER-NODE)))))))))))

(defun generate-match-procedure (pattern var test condition)
  (multiple-value-bind (tests binding-specs)
       ;;make special tests to check for this pattern
        (generate-match-body pattern (pattern-free-variables pattern)
           (if var (list (cons var 'P))) test) ;; Seed with :VAR binding
    `(defun ,(generate-rule-procedure-name pattern) (P ,@ *bound-vars*)
       ;;first arg, P, is the pattern
       (if (and ,@ tests)
	   (values T (list ,@ (if var '(P)) ,@ (reverse binding-specs))
		   ,(unless (eq condition :INTERN) t))))))

(defun scratchout (l1 l2)
  ;non-destructive and order-preserving
  (dolist (el1 l1 l2) (setq l2 (remove el1 l2))))

(defun generate-rule-procedure-name (pattern)
  (intern (format nil "~A-~A-~A" 
		  *file-prefix* pattern (incf *file-counter*))))

;;;; Recursive macroexpansion

(defvar *macros-to-expand*
  '(rule internal-rule add-internal-rule with-pushed-variable-bindings
    rlet rassert! rretract! assuming with-LTRE with-contradiction-handler
    with-contradiction-check without-contradiction-check))

(defun fully-expand-body (body)
  ;; New version -- only expands when it is clear that expansion is needed.
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

(defun insert-rule (dbclass matcher body &aux rule *LTRE*)
  (setq *LTRE* (dbclass-ltre dbclass))
  (setq rule (make-rule :MATCHER matcher
			:BODY body
			:DBCLASS dbclass
			:COUNTER (incf (ltre-rule-counter *LTRE*))))
  ;; Index it
  (push rule (dbclass-rules dbclass))
  (dolist (candidate (dbclass-facts dbclass))
	  (try-rule-on rule candidate)))

(defun try-rules (datum)
  (dolist (rule (dbclass-rules (datum-dbclass datum)))
    (try-rule-on rule datum)))

(defun try-rule-on (rule datum &aux *LTRE*)
  (setq *LTRE* (datum-ltre datum))
  (multiple-value-bind (okay? bindings node?)
      (funcall (rule-matcher rule) (datum-lisp-form datum))
    (when okay?
	  (when node?
		(push (datum-tms-node datum) bindings))
      (enqueue (cons (rule-body rule) bindings)
	       *LTRE*))))

(defun run-rules (&optional (*LTRE* *LTRE*))
  (do ((form (dequeue *LTRE*) (dequeue *LTRE*))
       (counter 0))
      ((null form)
       (debugging-ltre "~%    ~A rules run."  counter)
       (incf (ltre-rules-run *LTRE*) counter)
       counter)
    (apply (car form) (cdr form))
    (incf counter)))

(defun run-one-rule (&optional (*LTRE* *LTRE*))
  (let ((rule (dequeue *LTRE*)))
    (when rule
      (debugging-ltre "~%     Executing single rule.")
      (incf (ltre-rules-run *LTRE*))
      (apply (car rule) (cdr rule)))
    (rules-waiting? *LTRE*)))

(defun rules-waiting? (&optional (*LTRE* *LTRE*))
  (not (null (ltre-queue *LTRE*))))

(defun enqueue (new *LTRE*)
  (push new (ltre-queue *LTRE*)))

(defun dequeue (ltre) (pop (ltre-queue ltre)))

;;;; Display routines

(defun show-rules (&optional (*LTRE* *LTRE*) (stream *standard-output*) &aux counter)
   (setq counter 0)
   (format t "~%The rules in ~A are:" (ltre-title *LTRE*))
   (map-dbclass #'(lambda (dbclass)
                     (dolist (rule (dbclass-rules dbclass))
                        (incf counter)
                        (print-rule rule stream))))
   counter)  

(defun print-rule (rule &optional (stream *standard-output*))
  (format stream "~% ~A: ~A, ~A"  rule 
	  (rule-matcher rule) (rule-body rule)))

(defun test-rule-expansion ()
 (pprint (macroexpand
	  '(rule ((:TRUE (foo ?p ?q) :VAR ?f1)
		  (:FALSE (bar ?q ?r) :VAR ?f2)
		  (:INTERN (interesting ?r) :VAR ?f3))
		 (rassert! (:TAXONOMY ?p ?q ?f3) (:RANDOM-TEST ?f1 ?f2))))))

(defun get-rule (num &optional (*LTRE* *LTRE*))
  (dolist (rule (ltre-rules *LTRE*))
    (when (= (rule-counter rule) num) (return-from GET-RULE rule))))
