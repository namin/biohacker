;-*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;;; Tiny Constraint Language
;; Last edited: 1/29/93, by KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Constraint languages restrict reference to gain efficiency.
;;   This is a simplified version of Forbus' CONLAN, with all
;;   efficiency tricks removed. 

(defstruct (tcon (:PRINT-FUNCTION
		  tcon-struct-printer))
  (title nil)                   ; For printing
  (prototypes nil)              ; List of prototypes
  (cells nil)                   ; the network's cells
  (constraints nil)             ; the network's constraints
  (beg-queue nil)               ; Cells which need values
  (help-queue nil)              ; Rules ready to execute
  (coincidence-handler nil)     ; Are two values same?
  (contradiction-handler nil)   ; similar to JTMS.
  (debugging nil))              ; 

(defun tcon-struct-printer (tcon stream ignore)
  (declare (ignore ignore))
  (format stream "<TCON: ~A>" (tcon-title tcon)))

(proclaim '(special *tcon* $self))
(defvar *tcon* nil)     ; Current interpreter.
(defvar $self nil)	; Canonical name,  used internally
(defvar $informant nil) ; For debugging

(defstruct (cell
	     (:PREDICATE cell?)
	     (:PRINT-FUNCTION cell-printer))
           name             ;Print name
           tcon             ;What interpreter it is in.
           owner            ;The constraint it belongs to
           (value :UNKNOWN) ;The value
           informant        ;What rule delivered the value
           roles            ;What part it plays in various constraints
           plist)           ;Usual place to store things

(defun cell-printer (st str ignore)
  (declare (ignore ignore))
  (format str "<Cell ~A>" (cell-pretty-name st)))

(defstruct (constraint
	     (:PREDICATE constraint?)
	     (:PRINT-FUNCTION constraint-printer))
	   name		; Print name
	   tcon         ; What interpreter it is in.
	   owner	; The constraint it belongs to
	   parts	; The parts which comprise it
	   prototype)	; Backpointer

(defun constraint-printer (st str ignore)
  (declare (ignore ignore))
  (format str "<Constraint ~A>" (constraint-name st)))

(defstruct (prototype
	     (:PRINT-FUNCTION prototype-printer))
           name           ; Print name
           tcon           ; What interpreter it is part of
           parts          ; Parts for this kind of constraint
           creation-forms ; Extra work
           cells          ; Value-holding parts 
           rules)         ; Rules for this kind of constraint

(defun prototype-printer (st str ignore)
  (declare (ignore ignore))
  (format str "<Prototype ~A>" (prototype-name st)))

;;; Initialization 

(defun create-tcon (title &key (debugging nil)
			  (prototype-file nil)
			  (contradiction-handler 'default-contradiction-handler)
			  (coincidence-handler 'default-coincidence-handler))
  (let ((tcon (make-tcon :TITLE title
			   :DEBUGGING debugging
			   :COINCIDENCE-HANDLER coincidence-handler
			   :CONTRADICTION-HANDLER contradiction-handler)))
    (setq *tcon* tcon)
    (eval `(Constraint == ((Cell1 cell) (Cell2 cell))
		       (formulae (Cell1 (Cell2) Cell2)
				 (Cell2 (Cell1) Cell1))))
    (when prototype-file
	  (load-prototypes prototype-file tcon))
    (setq *tcon* tcon)))

(defun load-prototypes (file-name &optional (tcon *tcon*))
  (let ((*tcon* tcon)) (load file-name)))

(defvar *default-defs*
  #+ILS "/u/bps/code/tcon/condef.lisp"
  #+PARC "virgo:/virgo/dekleer/bps/code/tcon/condef.lisp"
  #+MCL "Macintosh HD:BPS:tcon:condef")

(defun change-tcon (tcon &key (debugging nil)
			 (contradiction-handler nil))
  (setf (tcon-debugging tcon) debugging)
  (if contradiction-handler
      (setf (tcon-contradiction-handler tcon)
	    contradiction-handler)))

;;;; Defining prototypes
;; A prototype specifies its parts, both what they are
;; and the relationships between them.  Some of the
;; relationships are enforced by FORMULAE, rules internal
;; to the constraint.

(defmacro Constraint (name part-list &rest body)
  `(progn 'compile
     (let ((pr (make-prototype
		 :NAME ',name
		 :TCON *tcon*
		 :PARTS ',part-list)))
       ,@ (analyze-prototype-body body 'pr)
       (push pr (tcon-prototypes *tcon*))
       pr)))

(defun get-prototype (pname tcon)
  (find pname (tcon-prototypes tcon) :KEY #'(lambda (p) (prototype-name p))))

(defun analyze-prototype-body (body name &aux creation-time now)
  (dolist (form body)
    (when (listp form)
      (case (car form)
	(FORMULAE
	  (setq now (append (process-constraint-rules
			      (cdr form) name) now)))
	(t (push (localize-references form) creation-time)))))
  (cons `(setf (prototype-creation-forms ,name)
	       ',creation-time)
	now))

(defun localize-references (form)
  ;; Exploit fact that $self is always bound at creation time,
  ;; and that all references must be local.
  (cond ((null form) form)
	((not (listp form)) form)
	((eq (car form) '>>) ;; A reference
	 (nconc form (list '$self)))
	(t (cons (localize-references (car form))
		 (localize-references (cdr form))))))

;;;; Expanding FORMULAE definitions
 
(defun process-constraint-rules
  (rule-specs name
	      &aux rule-counter rules cells
	      rule-name cell-entry subentry)
;; This function builds up two alists, one which defines
;; the rules belonging to a class of constraints and the
;; other which describes the cells used by these constraints.
;; For simplicity, no consistency checking will be provided.
  (setq rule-counter 0)
  (dolist (rule-spec rule-specs)
 ;;The format of a rule is (<sets> (<input cells>) . <body>)
    (setq rule-name
	  (intern (format nil "RULE-~A" (incf rule-counter))))
    (push (cons rule-name
		(list (cons 'SETS (car rule-spec))
		      (cons 'USES (cadr rule-spec))
		      (cons 'BODY (eval `(function (lambda ,(cadr rule-spec)
				                     ,@ (cddr rule-spec)))))))
	  rules)
    ;; Now must install index information for cells
    (setq cell-entry (assoc (car rule-spec) cells))
    (unless cell-entry ;; build if necessary
	    (setq cell-entry (list (car rule-spec)))
	    (push cell-entry cells))
    ;; Add the suppliers to tell what to queue when begging.
    (setq subentry (assoc 'SUPPLIERS (cdr cell-entry)))
    (cond (subentry (setf (cdr subentry)
			  (cons rule-name (cdr subentry))))
	  (t (setq subentry (list 'SUPPLIERS rule-name))
	     (setf (cdr cell-entry)
		   (cons subentry (cdr cell-entry)))))
    ;; Add the USERS to propagate forgetfulness
    (dolist (cell (cadr rule-spec))
      (setq cell-entry (assoc cell cells))
      (unless cell-entry
	      (setq cell-entry (list cell))
	      (push cell-entry cells))
      (setq subentry (assoc 'USERS (cdr cell-entry)))
      (cond (subentry (setf (cdr subentry)
			    (cons rule-name (cdr subentry))))
	    (t (setq subentry (list 'USERS rule-name))
	       (setf (cdr cell-entry)
		     (cons subentry (cdr cell-entry)))))))
  `((setf (prototype-cells ,name) ',cells)
    (setf (prototype-rules ,name) ',rules)))

;;;; Creating constraints

(defun create (name type &optional (tcon *tcon*))
  (let (($self :USER) ;; Entry point
	(*tcon* tcon))
  (create1 name type tcon)))

(defun create1 (name type tcon &aux constraint prototype)
  (cond ((eq type 'CELL) (create-cell name tcon))
	(t (setq prototype (get-prototype type tcon))
	   (unless prototype (error "~A prototype not defined: CREATE1, ~A, ~A."
				    type name tcon))
	   (setq constraint (make-constraint
			      :NAME name
			      :TCON tcon
			      :OWNER $self
			      :PROTOTYPE prototype
			      :TCON tcon
			      :PARTS nil))
	   (let (($self constraint))
	     (setf (constraint-parts constraint)
		   (mapcar
		     #'(lambda (pair)
			 (cons (car pair)
			       (create1 (car pair) (cadr pair) tcon)))
		     (prototype-parts prototype)))
	     (dolist (form (prototype-creation-forms prototype))
		     (eval form)))
	   (push constraint (tcon-constraints tcon))
	   constraint)))

(defun create-cell (name tcon &aux cell)
  (setq cell (make-cell
	       :NAME name
	       :TCON tcon
	       :OWNER $self
	       :VALUE :UNKNOWN
	       :ROLES (list (cons name $self))))
  (push cell (tcon-cells tcon))
  cell)

;;;; Accessors
;; The operator ">>" refers to parts of structured objects.
;; To prevent equating objects across constraint interpreters,
;;  we leave the interpreter pointer implicit.  

(defmacro >> (&rest args)
  `(nested-lookup ',(reverse (butlast args)) ',(car (last args))))

(defun lookup-global (name tcon)
  (let ((con (if (eq name '$self) (symbol-value name)
		 (find name (tcon-constraints tcon)
		       :KEY #'(lambda (con) (constraint-name con))))))
    (if con con
      (find name (tcon-cells tcon) :KEY #'(lambda (cell) (cell-name cell))))))

(defun nested-lookup (indicators obj)
  (setq obj (lookup-global obj *tcon*))
  (dolist (indicator indicators)
	  (when (null obj)
		(error "Indicator path bombed out at ~A: ~A of ~A"
		       indicator indicators obj))
	  (setq obj (lookup-part indicator obj)))
  obj)

(defun lookup-part (part-name obj)
  (cdr (assoc part-name (constraint-parts obj))))

(defun pretty-name (thing)
  (cond ((cell? thing) (cell-pretty-name thing))
	(t (format nil "~A"
		   (cons '>> (constraint-pretty-name thing))))))

(defun cell-pretty-name (cell) 
  (if (constraint? (cell-owner cell))
      (format nil "~A"
	      (cons '>> (cons (cell-name cell)
			      (constraint-pretty-name
				(cell-owner cell)))))
      (format nil "~A" (cell-name cell))))

(defun constraint-pretty-name (con)
  (cond ((constraint? (constraint-owner con))
	 (cons (constraint-name con)
	       (constraint-pretty-name (constraint-owner con))))
	(t (list (constraint-name con)))))

;;;; Equality system
;;; The special constraint == provides the glue for building 
;;;     networks of constraints.
;; Two parts can be declared identical only if they are
;;  of the same type.
;; Here identical means all their cells have the same values.

(defmacro == (first second)
  `(progn (process== ,first ,second)
	  (enforce-constraints *tcon*)))

(defun process== (first second)
  (cond ((and (cell? first) (cell? second))
	 (==cells first second))
	((and (constraint? first) (constraint? second)
	      (eq (constraint-prototype first)
		  (constraint-prototype second)))
	 (dolist (part-pair (constraint-parts first))
	   (process== (cdr part-pair)
		      (lookup-part (car part-pair) second))))))

(defun ==cells (first second)
  (let* ((tcon (cell-tcon first))
	 (==c (make-constraint :NAME '1<=>2
			       :TCON tcon
			       :PARTS (list (cons 'Cell1 first)
					    (cons 'Cell2 second))
			       :PROTOTYPE (get-prototype '== tcon))))
    (push (cons 'Cell1 ==C) (cell-roles first))
    (push (cons 'Cell2 ==C) (cell-roles second))
    ;Queue up equality rules on creation
    (push (cons 'RULE-1 ==C) (tcon-help-queue tcon))
    (push (cons 'RULE-2 ==C) (tcon-help-queue tcon))
    (push ==c (tcon-constraints tcon))
    ==c))

;;;; Unwiring stuff
;; Removes any == constraints between parts of thing1 and thing2

(defun un== (thing1 thing2)
  (cond ((and (cell? thing1) (cell? thing2))
	 (dolist (role (cell-roles thing1))
	  (let ((joint (rassoc (cdr role) (cell-roles thing2))))
	    (when joint
		  (format t "~% Removing ~A from ~A and ~A."
			  (cdr joint) thing1 thing2)
	     (setf (cell-roles thing1)
		   (delete role (cell-roles thing1) :COUNT 1))
	     (setf (cell-roles thing2)
		   (delete joint (cell-roles thing2) :COUNT 1))
	     (setf (tcon-constraints (cell-tcon thing1))
	      (delete (cdr joint) 
	       (tcon-constraints (cell-tcon thing1)) :COUNT 1))
	     (when (and (consp (cell-informant thing1))
			(eq (cdr (cell-informant thing1))
			    (cdr joint)))
		   (forget! thing1 (cell-informant thing1)))
	     (when (and (consp (cell-informant thing2))
			(eq (cdr (cell-informant thing2))
			    (cdr joint)))
		   (forget! thing1 (cell-informant thing2)))))))
	((and (constraint? thing1) (constraint? thing2)
	      (eq (constraint-prototype thing1)
		  (constraint-prototype thing2)))
	 (dolist (part-pair (constraint-parts thing1))
	   (un== (cdr part-pair)
		      (lookup-part (car part-pair) thing2))))))

;;;; Rule system
;; Conceptually, we can ask a cell what rules use it and what
;; rules can supply it with a value. For storage economy, this
;; is implemented by storing rules under the prototype.  A little
;; more work on retrieval, but alot of memory is saved this way.
;; (Yes, of course hash tables would speed things up....) 

(defun cell-suppliers (cell &aux prototype suppliers)
  ;; tells what rules could supply a value to this cell
  (dolist (rule-pointer (cell-roles cell))
    ;;These pointers are (<name> . <constraint>)
    ;;Add in the suppliers from each prototype.
    (setq prototype (constraint-prototype (cdr rule-pointer)))
    (dolist (rule (cdr (assoc 'SUPPLIERS
			(cdr (assoc (car rule-pointer)
			      (prototype-cells prototype))))))
      (push (cons rule (cdr rule-pointer)) suppliers)))
  suppliers)

(defun cell-users (cell &aux prototype users)
  ;; tells what rules could supply a value to this cell
  (dolist (rule-pointer (cell-roles cell))
    ;;These pointers are (<name> . <constraint>)
    ;; Add in users from prototypes. 
    (setq prototype (constraint-prototype (cdr rule-pointer)))
    (dolist (rule (cdr (assoc 'USERS
			(cdr (assoc (car rule-pointer)
			      (prototype-cells prototype))))))
      (push (cons rule (cdr rule-pointer)) users)))
  users)

(defun rule-sets (rule-pair)
  (lookup-part (cdr (assoc 'SETS
		     (cdr (assoc (car rule-pair)
			   (prototype-rules
			     (constraint-prototype
			       (cdr rule-pair)))))))
	       (cdr rule-pair)))

(defun rule-uses (rule-pair)
  (mapcar #'(lambda (cell-name)
	      (lookup-part cell-name (cdr rule-pair)))
	  (cdr (assoc 'USES
		(cdr (assoc (car rule-pair)
		      (prototype-rules
			(constraint-prototype
			  (cdr rule-pair)))))))))

;;;; Setting and propagating values

(defun known? (cell) (not (eq (cell-value cell) :UNKNOWN)))

(defun set! (cell value source &aux tcon)
  (setq tcon (cell-tcon cell))
  (if (tcon-debugging tcon)
      (format t "~% TCON: Computed ~A for ~A, via ~A."
	      value cell source))
  (cond ((known? cell)  ;if coincidence, do nothing.
	 (cond ((coincidence? (cell-value cell) value tcon)
		(if (tcon-debugging tcon)
		    (format t "~% TCON: ~A compatible with old value ~A for ~A." 
			    value (cell-value cell) (pretty-name cell))))
	       (t (signal-contradiction cell value source tcon))))
	(t (if (tcon-debugging tcon)
	       (format t "~% TCON: Setting ~A" cell))
	   (setf (cell-value cell) value)
	   (setf (cell-informant cell) source)
	   (spread-results cell source tcon))))

(defun spread-results (cell setter tcon)
 ;; Exclude the informant to prevent looping
  (dolist (user (cell-users cell))
    (unless (equal (car user) setter)
      (push user (tcon-help-queue tcon)))))

(defun help! (rule)
  (when (runnable? rule)
    (let ((result (run-rule rule))
	  (set-cell (rule-sets rule)))
      (cond ((eq result :DISMISS))
	    ((eq result :LOSE) ;got a contradiction
	     (signal-contradiction set-cell result rule (constraint-tcon (cdr rule))))
	    (t (set! set-cell result rule))))))

(defun runnable? (rule) (every #'known? (rule-uses rule)))

(defun run-rule (rule-pair)
 (let ((*tcon* (constraint-tcon (cdr rule-pair)))
       ($self (cdr rule-pair))
       ($informant rule-pair))
  (when (runnable? rule-pair)
   (apply (rule-body rule-pair)
	  (mapcar #'(lambda (cp) (cell-value cp))
		  (rule-uses rule-pair))))))

(defun rule-body (rule-pair)
 (cdr (assoc 'BODY
  (cdr (assoc (car rule-pair)
	      (prototype-rules
	       (constraint-prototype
		(cdr rule-pair))))))))


;;;; Forgetting values

(defun forget! (cell informant &aux tcon)
  (setq tcon (cell-tcon cell))
  (when (equal (cell-informant cell) informant)
    (do ((queue (list cell) (append new-cells (cdr queue)))
	 (new-cells nil nil))
	((null queue) cell)
      (if (tcon-debugging tcon)
	  (format t "~% TCON: Forgetting ~A.." (car queue)))
      (setf (cell-value (car queue)) :UNKNOWN)
      (setf (cell-informant (car queue)) nil)
      (push (car queue) (tcon-beg-queue tcon))
      (dolist (user (cell-users (car queue)))
	(when (equal (cell-informant (rule-sets user)) user)
	  (push (rule-sets user) new-cells))))))

(defun beg! (cell)
  (unless (known? cell)
    (dolist (rule (cell-suppliers cell))
      (if (runnable? rule)
	  (push rule (tcon-help-queue (cell-tcon cell)))))))

;;;; The rest of the dependency system

(defun coincidence? (val1 val2 tcon)
  (funcall (tcon-coincidence-handler tcon) val1 val2))

(defun default-coincidence-handler (val1 val2)
  (cond ((and (numberp val1) (numberp val2))
	 (nearly-zero? (- val1 val2)))
	(t (equal val1 val2))))

(defun nearly-zero? (m) (< (abs m) 1.0e-3))

(defun ground-justification? (thing) 
  (not (and (listp thing)
	    (typep (cdr thing) 'Constraint))))

(defun cell-ground (cell)
  (when (known? cell)
    (cond ((ground-justification? (cell-informant cell))
	   (unless (eq (cell-informant cell) 'god) (list cell)))
	  (t (rule-ground (cell-informant cell))))))

(defun rule-ground (rule-pair)
  (do ((queue (rule-uses rule-pair)
	      (nconc (cdr queue) new-cells))
       (marker (list t))
       (new-cells nil nil)
       (ground nil))
      ((null queue) ground)
    (unless (eq (getf (cell-plist (car queue)) 'marker) marker)
      (setf (getf (cell-plist (car queue)) 'marker) marker)
      (cond ((ground-justification? (cell-informant (car queue)))
	     (unless (eq (cell-informant (car queue)) 'GOD)
		     (push (car queue) ground)))
	    (t (setq new-cells
		     (rule-uses (cell-informant
				  (car queue)))))))))

;;;; Handling contradictions

(defun signal-contradiction (cell newval newsetter tcon)
  (funcall (tcon-contradiction-handler tcon)
	   cell newval newsetter tcon))

(defun default-contradiction-handler (cell newval
					   newsetter *tcon*)
  (let ((oldval (cell-value cell))
	(old-ground (cell-ground cell))
	(new-ground
	 (if (ground-justification? newsetter) nil
	   (rule-ground newsetter)))
	(common nil) (old-only nil)
	(new-only nil) (index 0))
    (cond ((and (eq newsetter 'user)
		(not (eq newval :LOSE)))
	   (format t "~% ~A is already known as ~A."
		   cell oldval)
	   (format t "~% The underlying assumptions are:")
	   (print-cells-and-values old-ground)
	   (format t "~% Retract one of these?")
	   (cond ((yes-or-no-p)
		  (ask-for-retraction old-ground))
		 (t (format t
		     "~% Okay, new value will be ignored."))))
	  ((eq newval :LOSE)
	   (if cell
	    (format t
	    "~%Contradiction discovered concerning ~A."
		    (cell-pretty-name cell))
	    (format t "~%Contradiction discovered by ~A."
		    newsetter))
	   (format t "~% The underlying assumptions are:")
	   (print-cells-and-values new-ground)
	   (ask-for-retraction new-ground))
	  (t (setq common (intersection old-ground
					new-ground))
	     (setq old-only (set-difference old-ground
					    common))
	     (setq new-only (set-difference new-ground
					    common))
	     (format t
	     "~%Conflicting values discovered for ~A."
		     (cell-pretty-name cell))
	     (format t "~%  [Old from ~A; New from ~A]"
		     (cell-informant cell) newsetter)
	     (cond (old-only
		    (format t
		     "~% The old value, ~A, depends on:"
			    oldval)
		    (print-cells-and-values old-only)
		    (setq index (length old-only)))
		   (t (format t "~% The old value is ~A."
			      oldval)))
	     (cond (new-only
		    (format t
		    "~% The new value, ~A, depends on:"
			    newval)
		    (print-cells-and-values new-only index)
		    (setq index (+ index
				   (length new-only))))
		   (t (format t "~% The new value is ~A."
			      newval)))
	     (when common
	       (format t "~% Both values depend on:")
	       (print-cells-and-values common index))
	     (ask-for-retraction
	       (append old-only new-only common))))))

;;;; More flash for contradiction handling

(defun print-cells-and-values (the-cells &optional (start 0))
  (do ((cells the-cells (cdr cells))
       (counter (1+ start) (1+ counter)))
      ((null cells))
    (format t "~% ~D. ~A = ~A."
	    counter (cell-pretty-name (car cells))
	    (cell-value (car cells)))))

(defun ask-for-retraction (assumptions)
  (let ((answer
	  (catch 'tcon-contradiction-handler
	    (break "~%  Please call TCON-ANSWER to retract."))))
    (cond ((not (integerp answer)) 
	   (format t "~%Answer must be an integer.")
	   (print-cells-and-values assumptions)
	   (ask-for-retraction assumptions))
	  ((or (< answer 1) (> answer (length assumptions)))
	   (format t "~%Answer must be between 1 and ~D."
		   (length assumptions))
	   (print-cells-and-values assumptions)
	   (ask-for-retraction assumptions))
	  (t (let ((assumption (nth (1- answer) assumptions)))
	       (forget! assumption
			(cell-informant assumption)))))))

(defun tcon-answer (answer)
  (throw 'Tcon-contradiction-handler answer))

;;;; User interface

(defmacro With-Network (tcon &rest forms)
  `(let ((*tcon* ,tcon)) ,@ forms))

(defun use-network (tcon) (setq *tcon* tcon))

(defun set-parameter (cell value)
  (cond ((cell? cell) (set! cell value 'USER)
	 (enforce-constraints (cell-tcon cell)))
	(t (format t "~% ~A isn't a cell." cell))))

(defun forget-parameter (cell) 
  (cond ((cell? cell) (forget! cell 'USER)
	 (enforce-constraints (cell-tcon cell)))
	(t (format t "~% ~A isn't a cell." cell))))

(defun change-parameter (cell newval)
  (cond ((cell? cell)  (forget! cell 'USER)
	 (set! cell newval 'USER)
	 (enforce-constraints (cell-tcon cell)))
	(t (format t "~% ~A isn't a cell." cell))))

(defun constant (cell value) (set! cell value 'God)
  (enforce-constraints (cell-tcon cell)))

(defun enforce-constraints (tcon)
  ;; This function should keep running the queues
  ;; until there is nothing left to do.
  (do ((rule nil)
       (begger nil)
       (rule-count 0)
       (beg-count 0))
      ((and (null (tcon-beg-queue tcon))
	    (null (tcon-help-queue tcon)))
       (values rule-count beg-count))
    (setq rule (pop (tcon-help-queue tcon)))
    (cond (rule (help! rule) (incf rule-count))
	  ((setq begger (pop (tcon-beg-queue tcon)))
	   (beg! begger) (incf beg-count)))))

(defmacro with-contradiction-handler (tcon handler
					   &body body)
  (let ((tconsv (gensym)) (old-handler (gensym)))
    `(let* ((,tconsv ,tcon)
	    (,old-handler
	     (tcon-contradiction-handler ,tconsv)))
     (unwind-protect
	 (progn (setf (tcon-contradiction-handler ,tconsv)
		      ,handler) ,@body)
       (setf (tcon-contradiction-handler ,tconsv)
	     ,old-handler)))))

;;;; Interrogatives

(defun what-is (cell &optional (indent ""))
  (cond ((known? cell)
	 (format t "~%~A ~A = ~A."
		 indent (cell-pretty-name cell) (cell-value cell)))
	(t (format t "~%~A ~A is unknown."
		   indent (cell-pretty-name cell)))))

(defun constraint-cells (constraint &aux cells)
  (dolist (part-entry (constraint-parts constraint)
		      cells)
   (if (cell? (cdr part-entry))
       (push (cdr part-entry) cells))))

(defun constraint-values (constraint &optional (recursive? nil) (indent "")
				     &aux nindent)
  (dolist (cell (constraint-cells constraint))
   (what-is cell indent))	  
  (when recursive? 
    (setq nindent (concatenate 'string " " indent))
    (dolist (part-entry (constraint-parts constraint))
      (unless (cell? (cdr part-entry))
       (format t "~%~A ~A:" indent (pretty-name (cdr part-entry)))
       (constraint-values (cdr part-entry) t nindent)))))

(defun show-network (tcon)
 (dolist (cell (tcon-cells tcon)) (what-is cell)))

;;;; Explanations

(defun why (cell)   ;;; Says why this cell is believed.
  (cond ((known? cell)
	 (format t "~% ~A = ~A, via ~A" (cell-pretty-name cell)
		 (cell-value cell) (cell-informant cell))
	 (unless (ground-justification? (cell-informant cell))
	   (format t "~%  and inputs:")
	   (dolist (ante (rule-uses (cell-informant cell)))
	     (format t "~%     ~A = ~A" 
		     (cell-pretty-name ante) (cell-value ante)))
	   (format t ".")))
	(t (format t "~% ~A is unknown."
		   (cell-pretty-name cell)))))

(defun premises (cell)
  (cond ((known? cell)
	 (cond ((ground-justification? (cell-informant cell))
		(why cell))
	       (t (format t "~% ~A = ~A, because:"
			  (cell-pretty-name cell)
			  (cell-value cell))
		  (dolist (premise (cell-ground cell))
		    (format t "~%      ~A = ~A, via ~A."
			    (cell-pretty-name premise)
			    (cell-value premise)
			    (cell-informant premise))))))
	(t (format t "~% ~A is unknown." (cell-pretty-name cell)))))

(defun needs (cell) ; What other values might yield a value for this cell.
  (cond ((known? cell)
	 (format t "~% ~A is already known." (cell-pretty-name cell)))
	(t (dolist (supplier (cell-suppliers cell))
	     (format t "~%To use ~A:" supplier)
	     (dolist (ucell (rule-uses supplier))
	       (unless (known? ucell)
		 (format t "~%   ~A"
			 (cell-pretty-name ucell)))))))
  cell)

;;; Explanations

(proclaim '(special *wfs-marker*))

(defun wfs (cell)
  (setq *wfs-marker* (cons cell nil))
    (cond ((known? cell) (wfs1 cell " "))
	  (t (format t "~% ~A not known." (cell-pretty-name cell)))))

(defun wfs1 (cell indent &aux antes)
  (format t "~%~A~A = ~A, via ~A." indent
	  (cell-pretty-name cell)
	  (cell-value cell)
	  (cell-informant cell))
  (unless (ground-justification? (cell-informant cell))
   ;; Must recurse
   (dolist (ante (rule-uses (cell-informant cell)))
    (format t "~% ~A~A" indent (cell-pretty-name ante))
    (unless (eq (getf (cell-plist ante) 'marker) *wfs-marker*)
	    (push ante antes)
	    (setf (getf (cell-plist ante) 'marker) *wfs-marker*)))
   (dolist (ante antes) (wfs1 ante (format nil " ~A" indent)))))

(defun show-suppliers (cell)
  (format t "~% For ~A:" (cell-pretty-name cell))
  (dolist (rule (cell-suppliers cell))
   (when (runnable? rule)
    (format t "~% ~A computes ~A."
	    rule (run-rule rule)))))
