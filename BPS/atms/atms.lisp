;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;; Assumption-based truth maintenance system, version 61 of 7/21/92.

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.
 
;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Definitions.

(defstruct (atms (:PRINT-FUNCTION print-atms))
  (title nil)
  (node-counter 0)              ; unique namer for nodes.
  (just-counter 0)              ; unique namer for justifications.
  (env-counter 0)               ; Unique id for environments.
  (nodes nil)                   ; List of all atms nodes.
  (justs nil)                   ; List of all justifications.
  (contradictions nil)          ; List of contradiction nodes.
  (assumptions nil)             ; List of all atms assumptions.
  (debugging nil)               ; Trace grungy details.
  (nogood-table nil)
  (contra-node nil)             ; A dummy contradiction node.
  (env-table nil)
  (empty-env nil)               ; Empty environment.
  (node-string nil)			       
  (enqueue-procedure nil))

(defun print-atms (atms stream ignore)
  (declare (ignore ignore))
  (format stream "#<ATMS: ~A>" (atms-title atms)))

(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)     					;; Unique name.
  (datum nil)                   ; Pointer to IE data structures.
  (label nil)                   ; minimal envs believed under
  (justs nil)                   ; providers of support
  (consequences nil)            ; provides support for.
  (contradictory? nil)          ; flag marking it as contradictory.
  (assumption? nil)             ; flag marking it as n assumption.
  (rules nil)                   ; run when label non-empty.
  (atms nil))
			   
(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (if (tms-node-assumption? node)
      (format stream "A-~D" (tms-node-index node))
      (format stream "#<NODE: ~A>" (node-string node))))

(defstruct (just (:PRINT-FUNCTION print-just))
	   (index 0)
	   (informant nil)
	   (consequence nil)
	   (antecedents nil))

(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "<~A ~D>" (just-informant just)
	  (just-index just)))

(defstruct (env (:PREDICATE env?)
		(:PRINT-FUNCTION print-env-structure))
	   (index 0)
	   (count 0)				; Number of assumptions.
	   (assumptions nil)
	   (nodes nil)
	   (nogood? nil)
	   (rules nil))				; Call this if becomes nogood.

(defun print-env-structure (env stream ignore)
  (declare (ignore ignore))
  (format stream "E-~D" (env-index env)))

(defun node-string (node)
  (funcall (atms-node-string (tms-node-atms node)) node))

(defmacro debugging (atms msg &optional node &rest args)
  `(when (atms-debugging ,atms)
     (format *trace-output*
	     ,msg (if ,node (node-string ,node)) ,@args)))

(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

(defun ordered-insert (item list test)
  (cond ((null list) (list item))
	((funcall test item (car list)) (cons item list))
	((eq item (car list)) list) 
	(t (cons (car list) (ordered-insert item (cdr list) test)))))

(defmacro ordered-push (item list test)
  `(setq ,list (ordered-insert ,item ,list ,test)))

(defun assumption-order (a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))

(defun env-order (e1 e2)
  (< (env-index e1) (env-index e2)))


;;; Basic inference engine interface.

(defun create-atms (title &key (node-string 'default-node-string)
		               (debugging NIL)
			       (enqueue-procedure NIL))
  (let ((atms (make-atms :TITLE title
			 :NODE-STRING node-string
			 :DEBUGGING debugging
			 :ENQUEUE-PROCEDURE enqueue-procedure)))
    (setf (atms-contra-node atms)
	  (tms-create-node atms "The contradiction"
			   :CONTRADICTORYP t))
    (setf (atms-empty-env atms) (create-env atms nil))
    atms))

(defun change-atms (atms &key node-string
		              enqueue-procedure debugging)
  (if node-string (setf (atms-node-string atms) node-string))
  (if debugging (setf (atms-debugging atms) debugging))
  (if enqueue-procedure
      (setf (atms-enqueue-procedure atms) enqueue-procedure)))

(defun true-node? (node)
  (eq (car (tms-node-label node))
      (atms-empty-env (tms-node-atms node))))

(defun in-node? (n &optional env)
  (if env
      (some #'(lambda (le) (subset-env? le env))
	    (tms-node-label n))
      (not (null (tms-node-label n)))))

(defun out-node? (n env) (not (in-node? n env)))

(defun node-consistent-with? (n env)
  (some #'(lambda (le) (not (env-nogood? (union-env le env))))
	(tms-node-label n)))				    

(defun tms-create-node (atms datum &key assumptionp contradictoryp
				   &aux node)
  (setq node (make-tms-node :INDEX (incf (atms-node-counter atms))
			    :DATUM datum
			    :ASSUMPTION? assumptionp
			    :CONTRADICTORY? contradictoryp
			    :ATMS atms))
  (push node (atms-nodes atms))
  (if contradictoryp (push node (atms-contradictions atms)))
  (when assumptionp
    (push node (atms-assumptions atms))
    (push (create-env atms (list node)) (tms-node-label node)))
  node)


(defun assume-node (node &aux atms)
  (unless (tms-node-assumption? node)
    (setq atms (tms-node-atms node))
    (debugging atms  "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t)
    (push node (atms-assumptions atms))
    (update (list (create-env atms (list node)))
	    node
	    'ASSUME-NODE)))

(defun make-contradiction
       (node &aux (atms (tms-node-atms node)) nogood)
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (atms-contradictions atms))
    (do nil (nil)
      (if (setq nogood (car (tms-node-label node)))
	  (new-nogood atms nogood 'MAKE-CONTRADICTION)
	  (return nil)))))
  
(defun justify-node (informant consequence antecedents &aux just atms)
  (setq atms (tms-node-atms consequence)
	just (make-just :INDEX (incf (atms-just-counter atms))
			:INFORMANT informant
			:CONSEQUENCE consequence
			:ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (atms-justs atms))
  (debugging atms
	     "~%Justifying ~A in terms of ~A on ~A"
	     consequence
	     informant
	     (mapcar #'node-string antecedents))
  (propagate just nil (list (atms-empty-env atms)))
  just)

(defun nogood-nodes (informant nodes)
  (justify-node informant
		(atms-contra-node (tms-node-atms (car nodes)))
		nodes))

;;; Label updating

(defun propagate (just antecedent envs &aux new-envs)
  (if (setq new-envs (weave antecedent envs (just-antecedents just)))
      (update new-envs (just-consequence just) just)))

(defun update (new-envs consequence just &aux atms enqueuef)
  (setq atms (tms-node-atms consequence))
  (when (tms-node-contradictory? consequence)
    (dolist (env new-envs) (new-nogood atms env just))
    (return-from update nil))
  (setq new-envs (update-label consequence new-envs))
  (unless new-envs (return-from update nil))
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (tms-node-rules consequence))
      (funcall enqueuef rule))
    (setf (tms-node-rules consequence) nil))
  (dolist (supported-just (tms-node-consequences consequence))
    (propagate supported-just consequence new-envs)
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (unless (member (car new-envs) (tms-node-label consequence))
      (rplaca new-envs nil)))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (unless new-envs (return-from update nil))))

(defun update-label (node new-envs &aux envs)
  (setq envs (tms-node-label node))
  (do ((new-envs new-envs (cdr new-envs)))
      ((null new-envs))
    (do ((nenvs envs (cdr nenvs)))
	((null nenvs) (push (car new-envs) envs))
      (cond ((null (car nenvs)))
	    ((null (car new-envs)))
	    ((case (compare-env (car new-envs) (car nenvs))
	       ((:EQ :S21) (rplaca new-envs nil))
	       (:S12 (setf (env-nodes (car nenvs))
			   (delete node (env-nodes (car nenvs))
				   :COUNT 1))
		     (rplaca nenvs nil)))))))
  (setq new-envs (delete nil new-envs :TEST #'eq))
  (dolist (new-env new-envs) (push node (env-nodes new-env)))
  (setf (tms-node-label node) (delete nil envs :TEST #'eq))
  new-envs)

(defun weave (antecedent envs antecedents &aux new-envs new-env)
  (setq envs (copy-list envs))
  (dolist (node antecedents)
    (unless (eq node antecedent)
      (setq new-envs nil)
      (dolist (env envs)
	(if env
	    (dolist (node-env (tms-node-label node))
	      (setq new-env (union-env env node-env))
	      (unless (env-nogood? new-env)
		(do ((nnew-envs new-envs (cdr nnew-envs)))
		    ((null nnew-envs) (push new-env new-envs))
		  (when (car nnew-envs)
		    (case (compare-env new-env (car nnew-envs))
		      ((:EQ :S21) (return nil))
		      (:S12 (rplaca nnew-envs nil)))))))))
      (setq envs (delete nil new-envs :TEST #'eq))
      (unless envs (return-from weave nil))))
  envs)

(defun in-antecedent? (nodes)
  (or (null nodes)
      (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))
  
(defun weave? (env nodes &aux new-env)
  (cond ((null nodes) t)			
	(t (dolist (e (tms-node-label (car nodes)))
	     (setq new-env (union-env e env))
	     (unless (env-nogood? new-env)
	       (if (weave? new-env (cdr nodes))
		   (return T)))))))

(defun supporting-antecedent? (nodes env)
  (dolist (node nodes t) (unless (in-node? node env) (return nil))))


(defun remove-node (node &aux atms)
  (if (tms-node-consequences node)
      (error "Can't remove node with consequences"))
  (setq atms (tms-node-atms node))
  (setf (atms-nodes atms)
	(delete node (atms-nodes atms) :test #'eq :count 1))
  (dolist (just (tms-node-justs node))
    (dolist (ant (just-antecedents just))
      (setf (tms-node-consequences ant)
	    (delete just (tms-node-consequences ant)
		    :test #'eq :count 1))))
  (dolist (env (tms-node-label node))
    (setf (env-nodes env)
	  (delete node (env-nodes env) :test #'eq :count 1))))

;;; Creating and extending environments.

(defun create-env (atms assumptions &aux e)
  (setq e (make-env :INDEX (incf (atms-env-counter atms))
		    :ASSUMPTIONS assumptions
		    :COUNT (length assumptions)))
  (setf (atms-env-table atms)
	(insert-in-table (atms-env-table atms) e))
  (set-env-contradictory atms e)
  e)

(defun union-env (e1 e2)
  (when (> (env-count e1)
	   (env-count e2))
    (psetq e1 e2 e2 e1))
  (dolist (assume (env-assumptions e1))
    (setq e2 (cons-env assume e2))
    (if (env-nogood? e2) (return nil)))
  e2)

(defun cons-env (assumption env &aux nassumes)
  (setq nassumes (ordered-insert assumption
				 (env-assumptions env)
				 #'assumption-order))
  (or (lookup-env nassumes)
      (create-env (tms-node-atms assumption) nassumes)))

(defun find-or-make-env (assumptions atms)
  (unless assumptions
    (return-from find-or-make-env (atms-empty-env atms)))
  ;; Presumes the list of assumptions is ordered properly
  (or (lookup-env assumptions)
      (create-env atms assumptions)))

;;; Env tables.

(defun insert-in-table (table env &aux count entry)
  (setq count (env-count env)
	entry (assoc count table :TEST #'=))
  (cond (entry (setf (cdr entry) (cons env (cdr entry))) table)
	(t (ordered-insert
	     (list count env) table
	     #'(lambda (entry1 entry2)
		 (< (car entry1) (car entry2)))))))

(defun lookup-env (assumes)
  (dolist (env (cdr (assoc (length assumes)
			   (atms-env-table (tms-node-atms (car assumes)))
			   :TEST #'=))
	       nil)
    (if (equal (env-assumptions env) assumes)
	(return env))))

(defun subset-env? (e1 e2)
  (cond ((eq e1 e2) t)
	((> (env-count e1)
	    (env-count e2)) nil)
	((subsetp (env-assumptions e1)
		  (env-assumptions e2)))))

(defun compare-env (e1 e2)
  (cond ((eq e1 e2) :EQ)
	((< (env-count e1) (env-count e2))
	 (if (subsetp (env-assumptions e1)
		      (env-assumptions e2))
	     :S12))
	((subsetp (env-assumptions e2) (env-assumptions e1))
	 :S21)))

;;; Processing nogoods

(defun new-nogood (atms cenv just &aux count)
  (debugging atms (format nil "~%  ~A new minimal nogood." cenv))
  (setf (env-nogood? cenv) just)
  (remove-env-from-labels cenv atms)
  (setf (atms-nogood-table atms)
	(insert-in-table (atms-nogood-table atms) cenv))
  (setq count (env-count cenv))
  (dolist (entry (atms-nogood-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(if (subset-env? cenv old)
	    (setf (cdr entry) (delete old (cdr entry) :COUNT 1))))))
  (dolist (entry (atms-env-table atms))
    (when (> (car entry) count)
      (dolist (old (cdr entry))
	(when (and (not (env-nogood? old))
		   (subset-env? cenv old))
	  (setf (env-nogood? old) cenv)
	  (remove-env-from-labels old atms))))))

(defun set-env-contradictory (atms env &aux count)
  (cond ((env-nogood? env) t)
	(t (setq count (env-count env))
	   (dolist (entry (atms-nogood-table atms))
	     (cond ((> (car entry) count)
		    (return nil))
		   (t (dolist (cenv (cdr entry))
			(when (subset-env? cenv env)
			  (setf (env-nogood? env)
				cenv)
			  (return t)))))))))

(defun remove-env-from-labels (env atms &aux enqueuef)
  (when (setq enqueuef (atms-enqueue-procedure atms))
    (dolist (rule (env-rules env))
      (funcall enqueuef rule))
    (setf (env-rules env) nil))
  (dolist (node (env-nodes env))
    (setf (tms-node-label node)
	  (delete env (tms-node-label node) :COUNT 1))))

;;; Interpretation construction

(proclaim '(special *solutions*))

(defun interpretations (atms choice-sets
			&optional defaults &aux solutions)
  (if (atms-debugging atms)
   (format *trace-output*
	   "~% Constructing interpretations depth-first..."))
  (let ((*solutions* nil)
	(choice-sets
	  (mapcar #'(lambda (alt-set)
		      (mapcan #'(lambda (alt)
				  (copy-list (tms-node-label alt)))
			      alt-set))
		  choice-sets)))
    (dolist (choice (car choice-sets))
      (get-depth-solutions1 choice (cdr choice-sets)))
    (setq *solutions* (delete nil *solutions* :TEST #'eq))
    (unless *solutions*
      (if choice-sets (return-from interpretations nil)
	              (setq *solutions* (list (atms-empty-env atms)))))
    (when defaults
      (setq solutions *solutions* *solutions* nil)
      (dolist (solution solutions)
	(extend-via-defaults solution defaults defaults)))
    (delete nil *solutions* :TEST #'eq)))

(defun get-depth-solutions1 (solution choice-sets
				      &aux new-solution)
  (cond ((null choice-sets)
	 (unless (do ((old-solutions *solutions* (cdr old-solutions)))
		     ((null old-solutions))
		   (when (car old-solutions)
		     (case (compare-env (car old-solutions) solution)
		       ((:EQ :S12) (return t))
		       (:S21 (rplaca old-solutions nil)))))
	   (push solution *solutions*)))
	((env-nogood? solution)) ;something died.
	(t (dolist (choice (car choice-sets))
	     (setq new-solution (union-env solution choice))
	     (unless (env-nogood? new-solution)
	       (get-depth-solutions1 new-solution
				     (cdr choice-sets)))))))


(defun extend-via-defaults (solution remaining original)
  (do ((new-solution)
       (defaults remaining (cdr defaults)))
      ((null defaults)
       (or (member solution *solutions* :TEST #'eq)
	   (dolist (default original)
	     (or (member default (env-assumptions solution)
			 :TEST #'eq)
		 (env-nogood? (cons-env default solution))
		 (return t)))
	   (push solution *solutions*)))
    (setq new-solution (cons-env (car defaults) solution))
    (unless (env-nogood? new-solution)
      (extend-via-defaults new-solution (cdr defaults) original))))

;;; Generating explanations
;;; This returns a list of justifications which form a DAG for the 
;;; derivation. This is quite complicated because this is really a 
;;; simple consequent JTMS.

(defun explain-node (node env) (explain-node-1 env node nil nil))

(defun explain-node-1 (env node queued-nodes explanation)
  (cond ((member node queued-nodes) nil)
	((and (tms-node-assumption? node)
	      (member node (env-assumptions env)))
	 (cons (cons 'ASSUME node) explanation))
	((dolist (just explanation)
	   (if (if (listp just)
		   (eq (cdr just) node) (eq (just-consequence just) node))
	       (return explanation))))
	(t (setq queued-nodes (cons node queued-nodes))
	   (dolist (just (tms-node-justs node))
	     (unless (dolist (a (just-antecedents just))
		       (unless (in-node? a env) (return t)))
	      (let ((new-explanation explanation))
		(dolist (a (just-antecedents just)
			   (return-from explain-node-1 (cons just new-explanation)))
		  (setq new-explanation
			(explain-node-1 env a queued-nodes new-explanation))
		  (unless new-explanation (return nil)))))))))

;;; Printing
(defun why-node (node &optional (stream t) (prefix ""))
  (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  (dolist (e (tms-node-label node))
    (env-string e stream))
  (format stream "}>"))

(defun why-nodes (atms &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))

(defun node-justifications (node &optional (stream t))
  (format t "~% For ~A:" (node-string node))
  (dolist (j (tms-node-justs node))
    (print-justification j stream)))

(defun print-justification (j &optional (stream t))
  (format stream "~%  ~A, " (just-informant j))
  (dolist (a (just-antecedents j))
    (why-node a stream "     ")))

(defun e (atms n)
  (dolist (bucket (atms-env-table atms))
    (dolist (env (cdr bucket))
    (if (= (env-index env) n) (return-from e env)))))

(defun print-env (e &optional (stream t))
  (format stream "~%~A:~A"
	  e (if (env-nogood? e)
		"* " " "))
  (env-string e stream))

(defun env-string (e &optional stream 
                     &aux assumptions strings printer)
  (setq assumptions (env-assumptions e))
  (when assumptions
    (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
  (dolist (a assumptions) (push (funcall printer a) strings))
  (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))

;;; Printing global data

(defun print-nogoods (atms &optional (stream t))
  (print-env-table (atms-nogood-table atms) stream))

(defun print-envs (atms &optional (stream t))
  (print-env-table (atms-env-table atms) stream))

(defun print-env-table (table stream)
  (dolist (bucket table)
    (dolist (env (cdr bucket))
      (print-env env stream))))

(defun print-atms-statistics (atms)
  (print-table "~% For env table:" (atms-env-table atms))
  (print-table "~% For nogood table:" (atms-nogood-table atms)))

(defun print-table (msg table)
  (format t msg)
  (dolist (entry table)
    (format t "~%   Length ~D, ~D" (car entry)
	    (length (cdr entry)))))
