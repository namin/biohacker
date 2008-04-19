;; -*- Mode: Lisp; -*-

;;;; JSAINT: A rational reconstruction of Slagel's SAINT program
;;; Last edited 1/29/92, by KDF

;;; Copyright (c) 1991 -- 1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, Xerox Corporation
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (Jsaint 
		   (:PRINT-FUNCTION (lambda (a st ignore)
				      (format st "<Agenda ~A>"
					      (jsaint-title a)))))
  (title "")           ;; Name for printing
  (jtre nil)           ;; Associated JTRE
  (agenda nil)         ;; List of queued subproblems
  (problem nil)        ;; When solved, we are done.
  (solution nil)       ;; Cached answer.
  (n-subproblems 0)    ;; Statistic
  (max-tasks 20)       ;; resource bound
  (debugging nil))     ;; Debugging flag

;; Start with the usual encapsulation

(proclaim '(special *jsaint*))

(defvar *jsaint* nil)

(defun create-jsaint (title problem &key (debugging nil)
			    (max-tasks nil))
  (let ((ag (make-jsaint
	      :TITLE title
	      :PROBLEM problem
	      :JTRE (create-jtre 
		      (concatenate 'string "JTRE of " title))
	      :DEBUGGING debugging
	      :MAX-TASKS (if (integerp max-tasks) max-tasks 20))))
    (in-jtre (jsaint-jtre ag))
    (change-jtms (jtre-jtms (jsaint-jtre ag))
		 :CONTRADICTION-HANDLER #'jsaint-contradiction-handler)
    (use-jsaint ag)))

(defmacro debugging-jsaint (js msg &rest args)
  `(when (jsaint-debugging ,js) (format t ,msg ,@ args)))

(defun change-jsaint (js &key (debugging :NADA) (problem :NADA)
		       (max-tasks :NADA))
  (unless (eq debugging :NADA) (setf (jsaint-debugging js) debugging))
  (unless (eq problem :NADA) (setf (jsaint-problem js) problem))
  (unless (eq max-tasks :NADA) (setf (jsaint-max-tasks js) max-tasks)))

(defun use-jsaint (js) (setq *jsaint* js))

(defmacro with-jsaint (js &rest forms) `(let ((*ag* ,js)) ,@ forms))

;;;; User entry point

(defvar *jsaint-rules*  ;; Fundamentals
  #+UNIX "/u/bps/code/jtms/jsrules.lisp"
  #+MCL "Macintosh HD:BPS:jtms:jsrules.fasl")
(defvar *jsaint-operators*  ;; Operators
  #+UNIX "/u/bps/code/jtms/jsops.lisp"
  #+MCL "Macintosh HD:BPS:jtms:jsops.fasl")

(defun solve-integral (integral
		       &key (title (symbol-name (gensym)))
		       (debugging nil)
		       (max-tasks 20))
  ;; Remove redudancies and canonicalize input. 
  (setq integral (eval (quotize (simplifying-form-of integral))))
  (use-jsaint (create-jsaint title integral
			     :DEBUGGING debugging
			     :MAX-TASKS max-tasks))
  (queue-problem (jsaint-problem *jsaint*) nil)
  (with-JTRE (jsaint-jtre *jsaint*) 
	     (load *jsaint-rules*)
	     (load *jsaint-operators*))
  (run-jsaint *jsaint*))

(defun explain-result (&optional (*jsaint* *jsaint*))
  (cond ((null (jsaint-solution *jsaint*))
	 (format t "~% Problem not solved yet."))
	((eq (jsaint-solution *jsaint*) :FAILED-PROBLEM)
	 (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
				    (jsaint-jtre *jsaint*)))
	 (format t "~% Failed to find a solution."))
	((eq (jsaint-solution *jsaint*) :FAILED-EMPTY)
	 (format t "~% Ran out of things to do.")
	 (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
				    (jsaint-jtre *jsaint*))))
	(t (format t "~% Solved the problem:")
	   (explore-network (get-tms-node
			 `(solution-of ,(jsaint-problem *jsaint*)
				       ,(jsaint-solution *jsaint*))
			 (jsaint-jtre *jsaint*))))))

;;;; Basic algorithm 

(defun run-jsaint (*jsaint*)
  (when (jsaint-solution *jsaint*)
    (return-from run-jsaint ;; Don't re-solve
      (values (jsaint-solution *jsaint*) *jsaint*)))
  (when (> (jsaint-n-subproblems *jsaint*)
	   (jsaint-max-tasks *jsaint*))
    (return-from run-jsaint ;; Respect resource limits
      (values :TIME-OUT *jsaint*)))
  (do ((done? nil)
       (solution (fetch-solution (jsaint-problem *jsaint*) *jsaint*)
		 (fetch-solution (jsaint-problem *jsaint*) *jsaint*))
       (failure-signal `(Failed (Integrate ,(jsaint-problem *jsaint*)))))
      (done? (values (jsaint-solution *jsaint*) *jsaint*))
    (cond (solution (setf (jsaint-solution *jsaint*) solution)
            (debugging-jsaint *jsaint*
             "~% ~A: Solved original problem." (jsaint-title *jsaint*))
	    (setq done? t))
	  ((in? failure-signal (jsaint-jtre *jsaint*))
	   (debugging-jsaint *jsaint*
	     "~% ~A: Failed on original problem."
	     (jsaint-title *jsaint*)) 
	   (setf (jsaint-solution *jsaint*) :FAILED-PROBLEM)
	   (setq done? t))
	  ((null (jsaint-agenda *jsaint*))
	   (debugging-jsaint *jsaint* "~% ~A: Agenda empty."
			     (jsaint-title *jsaint*))
	   (setf (jsaint-solution *jsaint*) :FAILED-EMPTY)
	   (setq done? t))
	  (t (process-subproblem (cdr (pop (jsaint-agenda *jsaint*))))))))

(defun process-subproblem (item &aux (jtre (jsaint-jtre *jsaint*))
			   (suggestions nil))

  (debugging-jsaint *jsaint* "~%  Trying to solve ~A." item)
  (open-subproblem item)
  (when (fetch-solution item *jsaint*)
	;; Bookkeeping is done by pdis rules
	(debugging-jsaint *jsaint* "~%    ..already solved.")
	(return-from process-subproblem T))
  (when (some #'(lambda (f) (in? f jtre)) ;; Already expanded
	      (fetch `(AND-SUBGOALS ,item ?subproblems) jtre))
	(debugging-jsaint *jsaint* "~%   ..already expanded.")
    (return-from process-subproblem T))
  (dolist (suggestion (fetch `(SUGGEST-FOR ,item ?operator) jtre))
    (when (in? suggestion jtre)
      (queue-problem `(try ,(third suggestion)) item)
      (push `(try ,(third suggestion)) suggestions)))
  ;; Presume extra subgoals don't come along.
  (assert! `(OR-SUBGOALS ,item ,suggestions) :OR-SUBGOALS jtre)
  (run-rules jtre))

(defun open-subproblem (item &aux (jtre (jsaint-jtre *jsaint*)))
  (assert! `(expanded ,item) :EXPAND-AGENDA-ITEM jtre)
  (assume! `(open ,item) :EXPAND-AGENDA-ITEM jtre)
  ;; Look for quick win, extra consequences.
  (run-rules jtre))

;;;; Queuing problems
;; Queue entries take the form (<difficulty> . <subproblem>)
;; Difficulty estimates are based on the form of the subproblem
;; alone, since there could be multiple parents for a subproblem.

(defun queue-problem (problem parent &aux entry)
  (setq entry (cons (estimate-difficulty problem) problem))
  (debugging-jsaint *jsaint* "~%   Queueing ~A, difficulty = ~D"
		    problem (car entry))
  (setf (jsaint-agenda *jsaint*)
	(merge 'list (list entry)
	       (jsaint-agenda *jsaint*)
	       #'(lambda (a b) (< (car a) (car b))))))

(defun estimate-difficulty (problem)
  (+ (max-depth problem) (count-symbols problem)))

(defun count-symbols (pr)
  (cond ((null pr) 0)
	((listp pr)
	 (reduce #'+ (mapcar #'count-symbols pr)
		 :INITIAL-VALUE 0))
	(t 1)))

(defun max-depth (pr)
  (cond ((not (listp pr)) 1)
	(t (1+ (reduce #'max (mapcar #'max-depth pr)
		       :INITIAL-VALUE 0)))))

;;;; Auxiliary routines

(defun fetch-solution (problem &optional (*jsaint* *jsaint*)
		       &aux (jtre (jsaint-jtre *jsaint*)))
  (dolist (solution (fetch `(SOLUTION-OF ,problem ?answer) jtre))
    (when (in? solution jtre)
      (return-from fetch-solution (third solution)))))

(defun jsaint-contradiction-handler (contradictions jtms)
  (ask-user-hander contradictions jtms)) ;; default

;;;; Defining operators

(defmacro defIntegration (name trigger &rest keyed-items
			  &aux subproblems result test)
  (setq subproblems (cadr (member :SUBPROBLEMS keyed-items)))
  (setq result (cadr (member :RESULT keyed-items)))
  (setq test (cadr (member :TEST keyed-items)))
  (unless result 
    (error "Integration operator must have result form"))
  `(rule ((:IN (expanded (Integrate ,trigger)) :VAR ?starter
	  ,@ (if test `(:TEST ,test) nil)))
	 (rlet ((?integral ,trigger)
		(?problem (Integrate ,trigger)))
	       (rlet ((?op-instance (,name ?integral)))
	     (rassert! (Operator-Instance ?op-instance)
		       :OP-INSTANCE-DEFINITION)
	     ;; If no subproblems, just create solution
     ,@ (cond ((null subproblems)
	       `((rlet ((?solution
			 (:EVAL (simplify ,(quotize result)))))
		  (rassert! (solution-of ?problem ?solution)
		   (,(keywordize name)
		     (Operator-Instance ?op-instance))))))
	      (t ;; Usual case
	       (let ((subs (calculate-subproblem-list subproblems))) 
		 `((rassert! (suggest-for ?problem ?op-instance)
		    (:INTOPEXPANDER ?starter))
   (rule ((:IN (expanded (try ?op-instance)) :VAR ?try))
	   (rlet ,subs
		 ,@ (mapcar #'(lambda (sub)
				`(queue-problem ,(car sub) ?problem))
			    subs)
		 (rassert! (AND-SUBGOALS (try ?op-instance)
					 ,(mapcar #'car subs))
			   (,(keywordize (format nil "~A-DEF" name))
			    ?try))
		 ;; Solution detector
		 ,(multiple-value-bind (triggers antes)
		      (calculate-solution-rule-parts subs subproblems)
		    `(rule (,@ triggers)
			   (rlet ((?solution
				    (:EVAL (simplify ,(quotize result)))))
				 (rassert! (solution-of ?problem ?solution)
					   (,(keywordize name)
					     ,@ antes)))))))))))))))

(defvar *test-operator*
	'(defIntegration Integral-of-Sum
	   (Integral (+ ?t1 ?t2) ?var)
	   :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
			 (?int2 (Integrate (Integral ?t2 ?var))))
	   :RESULT (+ ?int1 ?int2)))

;;;; Helpers for operator definition

(defun calculate-subproblem-list (subproblems &aux (counter -1))
  ;; Takes list of entries whose form is (?result-var ?form)
  ;; and returns a list of (?goal-var ?form)
  (mapcar #'(lambda (pair)
	      (incf counter)
	      (list (intern (format nil "?GOAL~D" counter)) 
		    (simplifying-form-of (cadr pair))))
	  subproblems))

(defun simplifying-form-of (alg-goal)
  ;; Run simplifier on subgoals, just in case.
  (cond ((null alg-goal) nil)
	((not (listp alg-goal)) alg-goal)
	((eq (car alg-goal) 'INTEGRAL) ;; Simplify as needed
	 `(INTEGRAL (:EVAL (SIMPLIFY ,(quotize (cadr alg-goal))))
	   ,(caddr alg-goal)))
	(t (cons (simplifying-form-of (car alg-goal))
		 (simplifying-form-of (cdr alg-goal))))))

(defun calculate-solution-rule-parts (sub-pairs res-pairs
				      &aux (counter -1)
				      (antes nil)
				      (triggers nil))
  (setq triggers
	(mapcar #'(lambda (subpair respair)
		    (incf counter)
		    (let ((rvar (intern (format nil "?RESULT~D" counter))))
		      (push rvar antes)
		      `(:in (solution-of ,(car subpair) ,(car respair))
			:VAR ,rvar)))
		sub-pairs res-pairs))
  (values triggers (nreverse antes)))

(defun keywordize (stuff)
  (cond ((null stuff) (error "Can't keywordize nothing."))
	((listp stuff) (keywordize (car stuff)))
	(t (intern (format nil "~A" stuff) 'keyword))))

;;;; Interrogatives

;;; SHOW-PROBLEM highlights the assertions relevant to
;;; the given problem.

(defun show-problem (pr &optional (*jsaint* *jsaint*)
			&aux stuff ands ors)
  (format t "~%~A:: (~D)" pr (estimate-difficulty pr))
  (with-JTRE (jsaint-jtre *jsaint*)
  (setq stuff (fetch `(parent-of ,pr ?x ?type)))
  (cond (stuff (format t "~% Parent(s): ")
  (dolist (p stuff)
	  (if (in? p)
	      (format t "~%   ~A, ~A."
		      (third p) (fourth p))
	    (format t "~%    BUG: Should be in: ~A" p))))
	(t (format t "~% No parents found.")))
  (if (fetch `(expanded ,pr)) (format t "~% Expanded,")
    (format t "~% Not expanded,"))
  (if (fetch `(open ,pr))
      (if (in? `(open ,pr)) (format t " open,")
	(format t " closed,"))
    (format t " not opened,"))
  (if (in? `(relevant ,pr)) (format t " relevant.")
    (format t " not relevant."))
  (cond ((setq stuff (fetch-solution pr))
	 (format t "~% Solved, solution = ~A" stuff))
	((and (setq stuff (car (fetch `(failed ,pr))))
	      (in? stuff)) (format t "~%  Failed."))
	((not (equal (car pr) 'try))
	 (format t "~% Neither solved nor failed.")))
  (setq ands (fetch `(and-subgoals ,pr ?ands)))
  (when ands (format t "~% And subgoals:")
	(dolist (subg (third (car ands)))
		(format t "~%   ~A" subg))
	(format t "."))
  (setq ors (fetch `(or-subgoals ,pr ?ors)))
  (when ors (format t "~% Or subgoals:")
	(dolist (subg (third (car ors)))
		(format t "~%   ~A" subg))
	(format t "."))))

;;;; Textual display of an AND/OR graph

(defun show-ao-graph (&optional (*jsaint* *jsaint*))
  (let* ((problems (get-problems))
	 (depth-table (update-ao-depth-table 
		       (jsaint-problem *jsaint*)
		       0 (list (cons (jsaint-problem *jsaint*) 0))
		       (list (jsaint-problem *jsaint*)))))
    (setq depth-table
	  (sort depth-table #'(lambda (x y) (< (cdr x) (cdr y)))))
    (dolist (pair depth-table)
	    (format t "~% ~D:" (cdr pair))
	    (show-problem (car pair)))))

(defun update-ao-depth-table (now depth depths path)
  (incf depth)
  (dolist (child (get-children now) depths)
   (unless (member child path :TEST 'equal) ;; Yes, can loop!
    (let ((entry (assoc child depths :TEST 'equal)))
      (unless entry 
	      (push (setq entry (cons child 0)) depths))
      (when (> depth (cdr entry))
	    (setf (cdr entry) depth)
	    (setq depths (update-ao-depth-table
			child depth depths (cons child path))))))))

(defun get-children (gp &optional (*jsaint* *jsaint*)
			&aux children)
  (dolist (maybe-kid (fetch `(parent-of ?x ,gp ?type)
			    (jsaint-jtre *jsaint*))
		     children)
	  (if (in? maybe-kid (jsaint-jtre *jsaint*))
	      (push (cadr maybe-kid) children))))

(defun get-problems (&optional (*jsaint* *jsaint*))
  (mapcar 'cadr (fetch '(expanded ?x) (jsaint-jtre *jsaint*))))

;;;; Debugging

(defun try-jsaint (problem &optional (title "JSAINT Test"))
  (solve-integral problem :DEBUGGING t :TITLE title))

(defun jfetch (pattern) (fetch pattern (jsaint-jtre *jsaint*)))

(defvar problem1 '(Integrate (Integral 1 x)))
(defvar problem2 '(Integrate (integral (+ x 5) x)))
(defvar problem3 '(Integrate (integral (* 46 (log x %e)) x)))
(setq problem4 '(Integrate
		 (integral (+ 0.63
			    (* 3.2 (sin (* 1.7 x)))
			    (* 4 (expt %e (* 2 x)))) x)))
