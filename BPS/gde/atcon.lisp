;;;-*- Mode: Lisp; Syntax: Common-lisp; -*-

;;; ATCON --- Trivial Constraint Language hooked to an ATMS, 
;;; Version 4 of 9/4/92. Requires ATMS version 59.

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (atcon (:print-function (lambda (atcon stream ignore)
				    (format stream "<ATCON: ~A>"
					    (atcon-title atcon)))))
  (title nil)                   ; For printing
  (cells nil)                   ; List of cells in network
  (queue nil)                   ; Rule-node pairs waiting to run.
  (constraints nil)             ; List of constraints in network
  (user-parts nil)              ; List of parts owned by user.
  (atms nil)                    ; The ATMS associated with it.
  (nearly-equal nil)            ; Ascertains if two values are the same.
  (disjunctions nil)            ; User supplied disjunctions.
  (debugging nil)               ; Debugging flag.
  (delay nil)                   ; This ATCON delays rules.
  (executions 0))               ; Number of rule executions.

(defstruct (cell (:predicate cell?)
		 (:print-function
		   (lambda (st str ignore)
		     (format str "<Cell ~A>" (cell-pretty-name st)))))
  atcon                         ; What interpreter it is in.
  name                          ; Print name
  owner                         ; The constraint it belongs to
  (nodes nil)                   ; The cell's nodes.
  users                         ; Users of this cell.
  domain)                       ; Allowable expressions.

(defstruct (value (:predicate value?)
                  (:print-function (lambda (st str ignore)
                                     (format str "<value:~A=~A>"
                                             (cell-name (value-cell st))
                                             (value-datum st)))))
  (datum nil)                   ; The problem-solver datum.
  (cell nil)                    ; Pointer back to cell it is in.
  (processed nil)               ; Rule's which have processed this value.
  (string nil))                 ; User supplied string.		

(defstruct (constraint
	     (:predicate constraint?)
	     (:print-function (lambda (st str ignore)
				(format str "<Constraint ~A>"
			(constraint-name st)))))
	   atcon        ; What interpreter it is in.
	   name         ; Print name.
	   owner        ; The constraint it belongs to.
	   parts        ; The parts which comprise it
	   prototype)   ; Backpointer.

(defstruct (prototype
	     (:print-function (lambda (st str ignore)
				(format str "<Prototype ~A>"
					(prototype-name st)))))
           name           ; Print name
           parts          ; Parts for this kind of constraint
           creation-form  ; Extra work
           cells)         ; Value-holding parts.
           

(defstruct (rule (:print-function (lambda (st str ignore)
				    (format str "<rule ~A->~A>"
                                            (rule-uses st)
                                            (rule-sets st)))))
  (uses nil)
  (sets nil)
  (body nil))

(proclaim '(special *atcon*))

(defmacro with-network (atcon &body forms)
  `(let ((*atcon* ,atcon)) ,@ forms))

(defvar *prototypes* (make-hash-table)) ; All known prototypes.

(defvar *temporary-datum* "Temporary")

(defmacro debugging-atcon (atcon msg &rest args)
  `(when (atcon-debugging ,atcon)
     (format *trace-output*
	     ,msg ,@args)))

(defun create-atcon (title &key (nearly-equal #'default-nearly-equal)
				(debugging nil)
				(delay T))
  (make-atcon :TITLE title
              :ATMS (create-atms "For ATCON"
                                 :ENQUEUE-PROCEDURE #'consider-node
                                 :NODE-string #'CELL-VALUE-STRING)
              :USER-PARTS (make-hash-table)
              :NEARLY-EQUAL nearly-equal
              :DEBUGGING debugging
              :DELAY delay))

(defun nearly-equal? (exp1 exp2 &optional (atcon *atcon*))
  (or (equal exp1 exp2)
      (funcall (atcon-nearly-equal atcon) exp1 exp2)))

(defun default-nearly-equal (exp1 exp2)
  (and (numberp exp1)
       (numberp exp2)
       (< (abs (- exp1 exp2)) 1.0e-3)))

;;; Defining prototypes.

(defmacro constraint (name part-list &rest body)
  (constraint-1 name part-list body nil))

(defmacro assume-constraint (name part-list &rest body)
  (constraint-1 name part-list body '((OK ASSUMPTION))))

(eval-when (compile load eval)
  (defun constraint-1 (name part-list body new-part)
    `(let ((self (make-prototype :name ',name
				 :parts ',(nconc part-list new-part))))
       (macrolet ((>> (&rest args)
		      `(nested-lookup ',(reverse args) self)))
	 ,@ (analyze-prototype-body body (caar new-part))
		 (setf (gethash ',name *prototypes*) self)))))

(eval-when (compile load eval) 
  (defun analyze-prototype-body (body ok &aux creation-time now)
    (dolist (form body)
      (cond ((and (listp form) (eq (car form) 'formulae))
	     (if ok (dolist (spec (cdr form))
		      (rplaca (cdr spec) (nconc (cadr spec) (list 'OK)))))
	     (setq now (append (process-constraint-rules (cdr form)) now)))
	    (t (push form creation-time))))
    (when creation-time
      (push `(setf (prototype-creation-form self)
		   (function (lambda (self) self ,@ creation-time)))
	    now))
    now))

(eval-when (compile load eval)
  (defun process-constraint-rules (rule-specs &aux rules 
				   cells rule-name cell-entry)
    (dolist (rule-spec rule-specs)
      (unless (cadr rule-specs) (error "No antecedents"))
      (setq rule-name (gensym))
      (push `(,rule-name (make-rule :SETS ',(car rule-spec)
				    :USES (reverse ',(cadr rule-spec))
				    :BODY (function (lambda ,(cadr rule-spec)
						      ,@ (cddr rule-spec)))))
	    rules)
      (dolist (cell (cadr rule-spec))
	(setq cell-entry (assoc cell cells))
	(unless cell-entry
	  (setq cell-entry (list cell))
	  (push cell-entry cells))
	(push rule-name (cdr cell-entry))))
    `((let ,rules
	(setf (prototype-cells self)
	      (list .
		    ,(mapcar #'(lambda (slot) `(list ',(car slot) ,@(cdr slot)))
			     cells)))))))

;;; Creating constraints.
(defun create (name type &optional supplied-parts (atcon *atcon*))
  (create1 name type supplied-parts atcon :USER))

(defun create1 (name type supplied-parts atcon owner &aux cell)
  (case type
    (CELL (create-cell name atcon supplied-parts owner))
    (ASSUMPTION
      (setq cell (create-cell name atcon nil owner))
      (assume-node (lookup-node cell
                                (if (symbolp owner) name
                                    (constraint-name owner))))
      cell)
    (t (create-prototype name type supplied-parts atcon owner))))

(defun create-prototype (name type supplied-parts atcon owner
			 &aux prototype constraint)
  (or (setq prototype (gethash type *prototypes*))
      (error "~A prototype not defined: CREATE1, ~A, ~A."
             type name atcon))
  (setq constraint (make-constraint :name name
                                    :atcon atcon
                                    :owner owner
                                    :prototype prototype
                                    :atcon atcon
                                    :parts nil))
  (do ((prototype-part (prototype-parts prototype) (cdr prototype-part))
       (supplied-part supplied-parts (cdr supplied-part)))
      ((null prototype-part))
    (push (cons (caar prototype-part)
		(cond ((eq (cadar prototype-part) 'CELL)
		       (cond ((car supplied-part)
			      (add-role (caar prototype-part) constraint (car supplied-part))
			      (car supplied-part))
			     (t (create1 (caar prototype-part)
					 'CELL
					 (cddar prototype-part)
					 atcon
					 constraint))))
		      ((car supplied-part)
		       (unless (eq (cadar prototype-part) 'ASSUMPTION)
			 (error "Incorrect supplied part."))
		       (car supplied-part))
		      (t (create1 (caar prototype-part)
				  (cadar prototype-part)
				  (mapcar #'(lambda (part-name)
					      (lookup-part part-name
							   constraint))
					  (cddar prototype-part))
				  atcon
				  constraint))))
	  (constraint-parts constraint)))
  (if (prototype-creation-form prototype)
      (funcall (prototype-creation-form prototype) constraint))
  (push constraint (atcon-constraints atcon))
  (if (eq owner :user)
      (setf (gethash name (atcon-user-parts atcon)) constraint))
  constraint)

(defun create-cell (name atcon domain owner &aux cell)
  (setq cell (make-cell :name name
			:atcon atcon
			:owner owner
			:domain domain))
  (if (eq owner :user)
      (setf (gethash name (atcon-user-parts atcon)) cell))
  (add-role name owner cell)
  (push cell (atcon-cells atcon))
  cell)

;;; Accessors.
(defmacro >> (&rest args)
  `(nested-lookup ',(reverse (butlast args))
                  (gethash ',(car (last args))
                           (atcon-user-parts *atcon*))))
(defun nested-lookup (indicators obj)
  (dolist (indicator indicators obj)
	  (unless obj
            (error "Indicator path bombed out at ~A: ~A of ~A"
                   indicator indicators obj))
	  (setq obj (lookup-part indicator obj))))

(defun lookup-part (part-name obj)
  (cdr (assoc part-name (constraint-parts obj))))



;;; Equality system.
(defmacro == (first second)
  `(process== ,first ,second))

(defun process== (first second)
  (cond ((and (cell? first) (cell? second))
         (create1 (gensym "==") '== (list first second)
		  *atcon* (cell-owner first)))
	((and (constraint? first) (constraint? second)
	      (eq (constraint-prototype first)
		  (constraint-prototype second)))
         (mapc #'process== (constraint-parts first)
	                   (constraint-parts second)))))

(Constraint == ((Cell1 cell) (Cell2 cell))
            (formulae (Cell1 (Cell2) Cell2)
                      (Cell2 (Cell1) Cell1)))

;;; Setting and accessing cell values.

(defun set! (cell datum informant antecedents)
  (justify-node informant (lookup-node cell datum) antecedents))

(defun lookup-node (cell datum &aux value node)
  (dolist (node (cell-nodes cell))
    (if (nearly-equal? datum (value-datum (tms-node-datum node))
		       (cell-atcon cell))
	(return-from lookup-node node)))
  ;; Go through process of creating the value.
  (setq value (make-value :DATUM datum :CELL cell)
        node (tms-create-node (atcon-atms (cell-atcon cell)) value))
  (dolist (old-node (cell-nodes cell))
    (nogood-nodes 'UNIQUE-VALUE (list old-node node)))
  (push node (cell-nodes cell))
  (if (and (cell-domain cell)
	   (not (member datum (cell-domain cell) :test #'eq)))
      (make-contradiction node))
  (dolist (user (cell-users cell))
    (push (cons user node) (tms-node-rules node)))
  node)

(defun known? (cell &optional (env nil))
  (if env 
      (dolist (node (cell-nodes cell))
	(when (in-node? node env)
	  (return-from known? node)))
      (remove-if-not #'(lambda (v) (in-node? v nil))
		     (cell-nodes cell))))

(defun assume-parameter (cell expression &optional string &aux node)
  (setq node (lookup-node cell expression))
  (setf (value-string (tms-node-datum node)) string)
  (assume-node node)
  (fire-constraints (cell-atcon cell)))

(defun set-parameter (cell value)
  (set! cell value :USER nil)
  (fire-constraints (cell-atcon cell)))


;;; Rule execution.
(defun consider-node (rule-object)
  (push rule-object (atcon-queue (constraint-atcon (cdar rule-object)))))

(defun add-role (cell-name constraint cell &aux signal users)
  (unless (symbolp constraint)
    (setq users (cdr (assoc cell-name
			    (prototype-cells (constraint-prototype
					       constraint)))))
    (when users
      (setq signal (cons users constraint))
      (push signal (cell-users cell))
      (dolist (node (cell-nodes cell))
        (if (and (in-node? node nil) (null (tms-node-rules node)))
            (consider-node (cons signal node))
            (push (cons signal node) (tms-node-rules node)))))))

(defun fire-constraints (&optional (atcon *atcon*))
  (do ((rule-object)
       (node))
      ((null (atcon-queue atcon)))
    (setq rule-object (pop (atcon-queue atcon))
	  node (cdr rule-object))
    (if (eq (tms-node-datum node) *temporary-datum*)
        (if (and (in-node? node nil)
                 (or (atcon-delay atcon)
                     (has-complete-external-support
                       (just-antecedents (car (tms-node-justs node)))
                       (cdr (just-informant
			      (car (tms-node-justs node)))))))
            (fire-delayed-constraint (car rule-object) (cdr rule-object))
            (push rule-object (tms-node-rules node)))
        (if (and (in-node? node nil)
                 (or (atcon-delay atcon)
                     (has-external-support node (cdar rule-object))))
            (fire-constraint (car rule-object) (cdr rule-object))))))

(defun has-external-support (node constraint)
  (or (tms-node-assumption? node)
      (dolist (just (tms-node-justs node))
	(unless (and (consp (just-informant just))
		     (equal constraint (cdr (just-informant just))))
	  (if (in-antecedent? (just-antecedents just)) (return T))))))

(defun has-complete-external-support (nodes constraint &aux empty-list)
  (setq empty-list (list (atms-empty-env (tms-node-atms (car nodes)))))
  (dolist (env (weave nil empty-list nodes))
    (unless
      (dolist (node nodes)
        (unless
	  (or (tms-node-assumption? node)
	      (dolist (just (tms-node-justs node))
		(unless (and (consp (just-informant just))
			     (equal constraint (cdr (just-informant just))))
		  (if (supporting-antecedent? (just-antecedents just) env)
		      (return T)))))
	  (return T)))
      (return T))))

(defun fire-constraint (rule-pair new-node 
                                &aux atcon function set-cell
				     constraint informant)
  (setq constraint (cdr rule-pair)
	atcon (constraint-atcon constraint))
  (dolist (rule (car rule-pair))
    (setq informant (cons rule constraint)
	  set-cell (lookup-part (rule-sets rule) constraint)
	  function (rule-body rule))
    (dolist (invoke-nodes (rule-weave (rule-uses rule)
				      new-node
				      constraint))
      (if (or (not (atcon-delay atcon))
	      (has-complete-external-support invoke-nodes constraint))
	  (execute-rule function invoke-nodes atcon informant set-cell)
	  (let ((t-node (tms-create-node (atcon-atms atcon)
					 *temporary-datum*)))
	    (debugging-atcon atcon
			     "~%Delayed: ~A to ~A."
			     function
			     invoke-nodes)
	    (justify-node informant t-node invoke-nodes)
	    (push (cons (cons (list function set-cell) constraint)
			t-node)
		  (tms-node-rules t-node))))))
  (push constraint (value-processed (tms-node-datum new-node))))

(defun fire-delayed-constraint (rule-pair new-node
				&aux atcon function antecedents)
  (setq atcon (constraint-atcon (cdr rule-pair))
	function (first (car rule-pair))
	antecedents (just-antecedents (car (tms-node-justs new-node))))
  (debugging-atcon atcon
		   "~%Executing delayed ~A on ~A"
		   function antecedents)
  (execute-rule function antecedents
		atcon (just-informant (car (tms-node-justs new-node)))
		(second (car rule-pair))
		new-node))

(defun execute-rule (function nodes atcon informant set-cell
		     &optional trigger &aux result)
  (incf (atcon-executions atcon))
  (setq result (apply function
		      (mapcar #'(lambda (node)
				  (value-datum (tms-node-datum node)))
			      nodes)))
  (case result
    (:DISMISS (if trigger (remove-node trigger)))
    (:LOSE (if trigger
	       (make-contradiction trigger)
	       (nogood-nodes informant nodes)))
    (t (if trigger (remove-node trigger))
       (set! set-cell result informant nodes))))

(defvar *results*)

(defun rule-weave (uses new-node constraint &aux *results*)
  (rule-weave-1 uses nil new-node constraint)
  *results*)

(defun rule-weave-1 (uses invoke-list new-node constraint &aux cell)
  (unless uses
    (push invoke-list *results*)
    (return-from rule-weave-1))
  (setq cell (lookup-part (car uses) constraint))
  (if (and new-node (eq cell (value-cell (tms-node-datum new-node))))
      (rule-weave-1 (cdr uses) (cons new-node invoke-list)
                        nil constraint)
      (dolist (node (cell-nodes cell))
        (if (member constraint (value-processed (tms-node-datum node))
                    :test #'eq)
            (rule-weave-1 (cdr uses) (cons node invoke-list)
			  new-node constraint)))))

;;; Constructing solutions.

(defun solutions (&optional (atcon *atcon*) &aux atms solutions)
  (setq atms (atcon-atms atcon)
	solutions (interpretations atms (atcon-disjunctions atcon)
				        (atms-assumptions atms)))
  (format T "~%The solutions are:")
  (mapc #'print-env solutions))

(defmacro disjunction (&rest descriptors &aux body)
  (dolist (descriptor descriptors)
    (push `(push (lookup-node ,(car descriptor) ',(cadr descriptor))
		 nodes)
	  body))
  `(let ((nodes nil))
     ,@ body
	(push nodes (atcon-disjunctions *atcon*))))

;;; Interrogatives.
(defun pretty-name (thing)
  (if (cell? thing)
      (cell-pretty-name thing)
      (format nil "~A" (cons '>> (constraint-pretty-name thing)))))

(defun cell-pretty-name (cell) 
  (if (constraint? (cell-owner cell))
      (format nil "~A"
	      (cons '>> (cons (cell-name cell)
			      (constraint-pretty-name
				(cell-owner cell)))))
      (format nil "~A" (cell-name cell))))

(defun constraint-pretty-name (con)
  (if (constraint? (constraint-owner con))
      (cons (constraint-name con)
	    (constraint-pretty-name (constraint-owner con)))
      (list (constraint-name con))))

(defun label-string (node &aux envs)
  (dolist (env (tms-node-label node)) (push (env-string env) envs))
  (format nil "{~{~A~}}" envs))
    
(defun cell-value-string (node &aux value)
  (setq value (tms-node-datum node))
  (cond ((stringp value) value)
	((value-string value))
	((eq (cell-name (value-cell value)) 'OK)
	 (format nil "~A" (value-datum value)))
	(t (format nil "~A = ~A" (cell-pretty-name (value-cell value))
		   (value-datum value)))))

(defun what-is (cell &key (env nil) (indent "") (stream t))
  (cond (env
	 (let ((nodes (remove-if-not #'(lambda (node)
                                         (in-node? node env))
                                     (cell-nodes cell))))
	   (cond ((cdr nodes) (error "~A has multiple values under ~A: ~A"
				      cell env nodes))
		 (nodes
		  (format stream "~%~A ~A = ~A."
			  indent (cell-pretty-name cell)
			  (value-datum (tms-node-datum (car nodes)))))
		 (t (format stream "~%~A ~A is unknown."
			    indent (cell-pretty-name cell))))))
	((cell-nodes cell)
	 (format stream "~%~A ~A = " indent (cell-pretty-name cell))
	   (dolist (node (cell-nodes cell))
	     (if (tms-node-label node)
		 (format stream "[~A,~A]" (value-datum (tms-node-datum node))
			 (label-string node)))))
	(t (format stream "~%~A ~A is unknown."
			    indent (cell-pretty-name cell)))))


(defun constraint-values (constraint &key (env nil) (recursive? nil) 
			  (indent "") (stream t)
			  &aux parts nindent)
  (setq parts (prototype-parts (constraint-prototype constraint)))
  (dolist (part parts)
    (when (eq 'cell (cadr part))
      (what-is (lookup-part (car part) constraint) :indent indent
	       :stream stream :env env)))
  (when recursive? 
    (setq nindent (concatenate 'string " " indent))
    (dolist (part parts)
      (unless (eq 'cell (cadr part))
	(setq part (lookup-part (car part) constraint))
	(format stream "~%~A ~A:" indent (pretty-name part))
	(constraint-values part :recursive? t 
			        :indent nindent :env env
				:stream stream)))))

(defun show-network (&optional (atcon *atcon*) env)
  (dolist (cell (atcon-cells atcon)) (what-is cell :env env)))

(defun print-solutions (&optional (atcon *atcon*))
  (dolist (solution (solutions))
    (format T "~% Cell values for solution ~A")
    (print-env solution)
    (show-network atcon solution)))

(defun why (cell expression &aux node)
  (setq node (lookup-node cell expression))
  (dolist (env (tms-node-label node))
    (format T "~% ~A under environment ~A:"
	    (cell-value-string node) (env-string env))
    (dolist (reason (explain-node node env))
      (cond ((listp reason)
	     (format T "~%  Assuming that ~A."
		     (cell-value-string (cdr reason))))
	    (t (format T "~%  ~A via ~A"
		       (cell-value-string (just-consequence reason))
		       (if (consp (just-informant reason))
			   (cdr (just-informant reason))
			   (just-informant reason)))
	       (when (just-antecedents reason)
		 (format T " and inputs:")
		 (dolist (ante (just-antecedents reason))
		   (format T "~%      ~A" (cell-value-string ante))))
	       (format T "."))))))

