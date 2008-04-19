;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;; Logic-based truth maintenance system, version 43 of 7/5/93
;;;;  Modified: forbus on Thurs Apr 18 9:05:01 1996

;;; Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Definitions.

(defstruct (ltms (:PRINT-FUNCTION print-ltms))
  (title nil)
  (node-counter 0)              ; unique namer for nodes.
  (clause-counter 0)            ; unique namer for justifications.
  (nodes nil)                   ; hash table for nodes.
  (clauses nil)                 ; list of all clauses.
  (debugging nil)               ; debugging flag
  (checking-contradictions t)
  (node-string nil)			       
  (contradiction-handlers nil)
  (pending-contradictions nil)
  (enqueue-procedure nil)
  (complete nil)                ; Is this a complete LTMS?
  (violated-clauses nil)
  (queue nil)			; Queue of clauses to resolve.
  (conses nil)			; Source of conses to reuse in inner loop.
  (delay-sat nil)		; Don't resolve satisfied clauses.
  (cons-size 0))		; Size of temporary structure.

(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)                    ; unique namer for nodes
  (datum nil)		       ; positive inference engine datum.
  (label :UNKNOWN)             ; :UNKNOWN, :TRUE, or :FALSE.
  (support nil)                ; clause which supports it,
  (true-clauses nil)           ; clauses in which this node is true
  (false-clauses nil)          ; clauses in which this node is false
  (mark nil)                   ; marker for sweep algorithms
  (assumption? nil)
  (true-rules nil)             ; rules run when the node is true
  (false-rules nil)            ; rules run when the node is false
  (ltms nil)                   ; LTMS it is part of.
  (true-literal nil)	       ; True literal. 
  (false-literal nil))         ; False literal.

;; The last two fields have their names changed because there is
;; an obscure bug in ACLPC that causes it to barf if a field is
;; named TRUE or FALSE, even if there is a conc-name to be added.
;; The new names are more descriptive anyway.

(defstruct (clause (:PRINT-FUNCTION print-clause))
  (index 0)       ; Unique namer
  (informant nil)
  (literals nil)  ; a list of (<node> . <truth>)
  (pvs 0)         ; Number of terms which potentially violate it.
  (length 0)      ; Number of literals.
  (sats 0)	  ; Number of terms which satisfy it.
  (status nil))   ; :SUBSUMED | :QUEUED | :DIRTY | :NOT-INDEXED | nil

(defun print-ltms (ltms stream ignore)
   (declare (ignore ignore))
   (format stream "#<LTMS: ~A>" (ltms-title ltms)))

(defun print-tms-node (node stream ignore)
   (declare (ignore ignore))
   (format stream "#<NODE: ~A>" (node-string node)))

(defun print-clause (clause stream ignore) 
   (declare (ignore ignore))
   (format stream "#<Clause ~D>" (clause-index clause)))

(defun node-string (node)
  (funcall (ltms-node-string (tms-node-ltms node)) node))

(defmacro debugging-ltms (ltms msg &optional node &rest args)
  `(when (ltms-debugging ,ltms)
     (format *trace-output*
	     ,msg (if ,node (node-string ,node)) ,@args)))

(defun ltms-error (string &optional thing) (error string thing))

(defun default-node-string (n)
  (format nil "~A" (tms-node-datum n)))

(defmacro satisfied-clause? (clause) `(> (clause-sats ,clause) 0))

(defmacro violated-clause? (clause) `(= (clause-pvs ,clause) 0))

(defmacro walk-clauses (ltms f)
  `(if (ltms-complete ,ltms)
       (walk-trie ,f (ltms-clauses ,ltms))
       (mapc ,f (ltms-clauses ,ltms))))

;;; Basic inference-engine interface.

(defun create-ltms (title &key (node-string 'default-node-string)
                    (debugging NIL)
                    (checking-contradictions T)
                    (contradiction-handler 'ask-user-handler)
                    (enqueue-procedure NIL)
                    (cache-datums? T) 
                    (complete nil)
                    (delay-sat T)
                    &aux ltms)
   ;; The CACHE-DATUMS? flag is new in this version.  When used as
   ;; part of a larger system, the internal TMS cache tends to be redundant.
   ;; Creating an LTMS with this flag turned off avoids the storage overhead
   ;; of this redundancy, while still leaving a default mechanism in place
   ;; for experimentation and systems that choose to use it.
  (setq ltms
	(make-ltms :TITLE title
		   :NODES (if cache-datums? (make-hash-table :TEST #'equal))
		   :NODE-STRING node-string
		   :DEBUGGING debugging
		   :CHECKING-CONTRADICTIONS checking-contradictions
		   :ENQUEUE-PROCEDURE enqueue-procedure
		   :CONTRADICTION-HANDLERS (list contradiction-handler)
		   :DELAY-SAT delay-sat
		   :COMPLETE complete))
  ltms)

(defun change-ltms (ltms &key (contradiction-handler nil contra?)
		              node-string
			      enqueue-procedure
			      (debugging nil debugging?)
			      (checking-contradictions nil checking?)
			      (complete nil complete?)
			      (delay-sat nil delay-sat?))
  (if node-string (setf (ltms-node-string ltms) node-string))
  (if debugging? (setf (ltms-debugging ltms) debugging))
  (if checking? (setf (ltms-checking-contradictions ltms)
		      checking-contradictions))
  (if contra?
      (setf (ltms-contradiction-handlers ltms)
	    (list contradiction-handler)))
  (if enqueue-procedure
      (setf (ltms-enqueue-procedure ltms) enqueue-procedure))
  (if complete? (setf (ltms-complete ltms) complete))
  (if delay-sat? (setf (ltms-delay-sat ltms) delay-sat)))

(defun unknown-node? (node) (eq (tms-node-label node) :UNKNOWN))

(defun known-node? (node) (not (eq (tms-node-label node) :UNKNOWN)))

(defun true-node? (node) (eq (tms-node-label node) :TRUE))

(defun false-node? (node) (eq (tms-node-label node) :FALSE))


(defun tms-create-node (ltms datum &key assumptionp)
  (if (and (ltms-nodes ltms) (gethash datum (ltms-nodes ltms)))
      (ltms-error "Two nodes with same datum:" datum))
  (let ((node (make-tms-node :INDEX (incf (ltms-node-counter ltms))
			     :DATUM datum
			     :ASSUMPTION? assumptionp
			     :LTMS ltms)))
    (setf (tms-node-true-literal node) (cons node :TRUE))
    (setf (tms-node-false-literal node) (cons node :FALSE))
    (if (ltms-nodes ltms) ;; Insert if locally caching
       (setf (gethash datum (ltms-nodes ltms)) node))
    (when (and (ltms-complete ltms)
	       (> (ltms-node-counter ltms) (ltms-cons-size ltms)))
      (setf (ltms-conses ltms) nil)
      (incf (ltms-cons-size ltms) 50.)
      (dotimes (i (ltms-cons-size ltms))
	(push (cons nil nil) (ltms-conses ltms))))
    node))

(defun enable-assumption (node label)
  (cond ((not (tms-node-assumption? node))
	 (ltms-error "Can't enable the non-assumption ~A" node))
	((eq (tms-node-label node) label)
	 (setf (tms-node-support node) :ENABLED-ASSUMPTION))
	((eq (tms-node-label node) :UNKNOWN)
	 (top-set-truth node label :ENABLED-ASSUMPTION))
	(t (ltms-error "Can't set an already set node" node))))

(defun convert-to-assumption (node)
  (unless (tms-node-assumption? node)
    (debugging-ltms (tms-node-ltms node)
		    "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) T)))

(defun retract-assumption (node)
  (when (and (known-node? node)
	     (eq (tms-node-support node) :ENABLED-ASSUMPTION))
    (find-alternative-support (tms-node-ltms node)
			      (propagate-unknownness node))))
    

;;; Adding formulas to the LTMS.
(defun add-formula (ltms formula &optional informant)
  (setq informant (list :IMPLIED-BY formula informant))
  (dolist (clause (normalize ltms formula))
    (unless (eq :TRUE (setq clause (simplify-clause clause)))
	(add-clause-internal clause informant T)))
  (check-for-contradictions ltms))

(defun simplify-clause (literals)
  (setq literals (sort-clause literals))
  (do ((tail literals next)
       (next (cdr literals) (cdr next)))
      ((null next) literals)
    (cond ((not (eq (caar tail) (caar next))))
	  ((not (eq (cdar tail) (cdar next)))
	   (return-from simplify-clause :TRUE))
	  (t (rplacd tail (cdr next))))))

(defun sort-clause (literals)
  (sort (copy-list literals) ;; Avoids shared structure bugs.
     #'< :KEY #'(lambda (n) (tms-node-index (car n)))))

(defvar *ltms*)

(defun normalize (*ltms* exp) (normalize-1 exp nil))

(defun normalize-1 (exp negate)
  (case (and (listp exp) (car exp))
    (:IMPLIES (if negate
		  (nconc (normalize-1 (cadr exp) nil)
			 (normalize-1 (caddr exp) t))
		  (disjoin (normalize-1 (cadr exp) t)
			   (normalize-1 (caddr exp) nil))))
    (:IFF (normalize-iff exp negate))
    (:OR (if negate (normalize-conjunction exp t)
	     (normalize-disjunction exp nil)))
    (:AND (if negate (normalize-disjunction exp t)
	             (normalize-conjunction exp nil)))
    (:NOT (normalize-1 (cadr exp) (not negate)))
    (:TAXONOMY (normalize-tax exp negate))
    (t (if negate `((,(tms-node-false-literal (find-node *ltms* exp))))
	          `((,(tms-node-true-literal (find-node *ltms* exp))))))))

(defun normalize-tax (exp negate)
  (normalize-1 `(:AND (:OR ,@ (copy-list (cdr exp))) ;one must be true
                 ;; The list is copied above to prevent very nasty bugs, since
                 ;; the rest of normalize side effects structure continually for
                 ;; efficiency.
                 ,@ (do ((firsts (cdr exp) (cdr firsts))
		       (rests (cddr exp) (cdr rests))
		       (result nil))
		      ((null rests) result)
		    (dolist (other rests)
		      (push `(:NOT (:AND ,(car firsts) ,other))
			    result))))
	       negate))

(defun normalize-conjunction (exp negate)
  (mapcan #'(lambda (sub) (normalize-1 sub negate)) (cdr exp)))


(defun normalize-iff (exp negate)
  (nconc (normalize-1 `(:IMPLIES ,(cadr exp) ,(caddr exp)) negate)
	 (normalize-1 `(:IMPLIES ,(caddr exp) ,(cadr exp)) negate)))

(defun normalize-disjunction (exp negate)
  (unless (cdr exp)
    (return-from normalize-disjunction (list nil)))
  (do ((result (normalize-1 (cadr exp) negate))
       (rest (cddr exp) (cdr rest)))
      ((null rest) result)
    (setq result (disjoin (normalize-1 (car rest) negate) result))))

(defun disjoin (conj1 conj2)
  (unless (or conj1 conj2) (return-from disjoin nil))
  (mapcan #'(lambda (disj1)
	    (mapcar #'(lambda (disj2) (append disj1 disj2))
		    conj2))
	  conj1))

(defun find-node (ltms name)
  (cond ((typep name 'tms-node) name)
	((if (ltms-nodes ltms) (gethash name (ltms-nodes ltms))))
	((tms-create-node ltms name))))

(defmacro compile-formula (run-tms f &optional informant &aux ltms)
  (setq ltms (create-ltms f))
  (add-formula ltms (expand-formula f))
  (generate-code ltms run-tms (if informant `(:IMPLIED-BY ,f ,informant))))

(defun generate-code (ltms run-tms informant &aux result bound datum)
  (maphash #'(lambda (ignore symbol)
	       (when (or (tms-node-true-clauses symbol)
			 (tms-node-false-clauses symbol))
		 (setq datum (tms-node-datum symbol))
		 (when (listp datum)
		   (setf (tms-node-mark symbol) datum)
		   (setf (tms-node-datum symbol)
			 (make-symbol (format nil "~A" (cadr datum))))
		   (push symbol bound))))
	   (ltms-nodes ltms))
  (walk-clauses ltms
		#'(lambda (clause &aux ps ns)
		    (dolist (lit (clause-literals clause))
		      (if (eq (cdr lit) :TRUE)
			  (push (tms-node-datum (car lit)) ps)
			  (push (tms-node-datum (car lit)) ns)))
		    (push `(add-clause `(,,@ps) `(,,@ns) ,informant)
			  result)))
  `(let ,(mapcar #'(lambda (s)
		     `(,(tms-node-datum s) (find-node ,run-tms ,(tms-node-mark s))))
		 bound)
     ,@result))

(defun expand-formula (x)
  (setq x (macroexpand x))
  (cond ((not (listp x)) x)
	((case (macroexpand (car x))
	   (QUOTE (partial (cadr x)))
	   (LIST (mapcar #'expand-formula (cdr x)))
	   (LIST* (if (cddr x)
		      (cons (expand-formula (cadr x))
			    (expand-formula `(LIST* .,(cddr x))))
		      (expand-formula (cadr x))))
	   (CONS (cons (expand-formula (cadr x))
		       (mapcar #'expand-formula (caddr x))))))
	(t x)))

(defun partial (x)
  (cond ((null x) x)
	((keywordp x) x)
	((not (listp x)) `',x)
	(t (cons (partial (car x)) (partial (cdr x))))))


;;; Adding clauses
(defun add-clause (true-nodes false-nodes &optional informant)
  (add-clause-internal (nconc (mapcar #'tms-node-true-literal true-nodes)
			      (mapcar #'tms-node-false false-nodes))
		       informant
		       nil))

(defun add-clause-internal (literals informant internal &aux ltms)
  (setq ltms (tms-node-ltms
	       (or (caar literals)
		   (ltms-error "Total contradiction: Null clause" informant))))
  (if (ltms-complete ltms)
      (full-add-clause ltms literals informant)
      (push (bcp-add-clause ltms literals informant)
	    (ltms-clauses ltms)))
  (unless internal (check-for-contradictions ltms)))

(defun bcp-add-clause (ltms literals informant &optional (index T)
		                               &aux cl label)
  (setq cl (make-clause :INDEX (incf (ltms-clause-counter ltms))
			:LITERALS literals
			:INFORMANT informant
			:LENGTH (length literals)))
  (dolist (term literals)
    (if (eq :UNKNOWN (setq label (tms-node-label (car term))))
	(incf (clause-pvs cl)))
    (ecase (cdr term)
      (:TRUE
	(if index (insert-true-clause cl (car term)))
	(when (eq label :TRUE)
	  (incf (clause-sats cl)) (incf (clause-pvs cl))))
      (:FALSE
       (if index (insert-false-clause cl (car term)))
       (when (eq label :FALSE)
	 (incf (clause-sats cl)) (incf (clause-pvs cl))))))
  (if index (check-clauses ltms (list cl)))
  cl)

(defun insert-true-clause (cl node)
  (push cl (tms-node-true-clauses node)))

(defun insert-false-clause (cl node)
  (push cl (tms-node-false-clauses node)))

(defun add-nogood (culprit sign assumptions &aux trues falses)
  (dolist (a assumptions (add-clause trues falses 'NOGOOD))
    (ecase (if (eq a culprit) sign (tms-node-label a))
      (:TRUE (push a falses))
      (:FALSE (push a trues)))))

;;; Boolean Constraint Propagation.

(proclaim '(special *clauses-to-check*))

(defun check-clauses (ltms *clauses-to-check*)
  (debugging-ltms ltms "~% Beginning propagation...")
  (do nil ((null *clauses-to-check*))
    (check-clause ltms (pop *clauses-to-check*))))

(defun check-clause (ltms clause &aux unknown-pair)
  (cond ((violated-clause? clause)
	 (pushnew clause (ltms-violated-clauses ltms)))
	((= (clause-pvs clause) 1)
	 ;; Exactly one term of the clause remains that can
	 ;; satisfy the clause, so deduce that term
	 (setq unknown-pair (find-unknown-pair clause))
	 (when unknown-pair ;must check, because it might have other
	   (set-truth (car unknown-pair) ; support
		      (cdr unknown-pair) clause)))))

(defun find-unknown-pair (clause)
  (dolist (term-pair (clause-literals clause))
    (if (unknown-node? (car term-pair)) (return term-pair))))

(defun top-set-truth (node value reason &aux *clauses-to-check*)
  (set-truth node value reason)
  (check-clauses (tms-node-ltms node) *clauses-to-check*)
  (check-for-contradictions (tms-node-ltms node)))

(defun set-truth (node value reason &aux ltms enqueuef)
  (setq ltms (tms-node-ltms node)
	enqueuef (ltms-enqueue-procedure ltms))
  (debugging-ltms
    ltms "~%  Setting ~A to ~A, via ~A." node value reason)
  (setf (tms-node-support node) reason)
  (setf (tms-node-label node) value)
  (ecase value ;figure out which set of rules to queue up
    (:TRUE (when enqueuef
	     (dolist (rule (tms-node-true-rules node))
	       (funcall enqueuef rule))
	     (setf (tms-node-true-rules node) nil))
	   (dolist (clause (tms-node-true-clauses node))
	     (incf (clause-sats clause)))
	   (dolist (clause (tms-node-false-clauses node))
	     (if (< (decf (clause-pvs clause)) 2)
		 (push clause *clauses-to-check*))))
    (:FALSE (when enqueuef
	      (dolist (rule (tms-node-false-rules node))
		(funcall enqueuef rule)))
	    (setf (tms-node-false-rules node) nil)
	   (dolist (clause (tms-node-false-clauses node))
	     (incf (clause-sats clause)))
	    (dolist (clause (tms-node-true-clauses node))
	      (if (< (decf (clause-pvs clause)) 2)
		  (push clause *clauses-to-check*))))))

;;; Retracting an assumption.
(defun propagate-unknownness (in-node)
  (let (node old-value node2 unknown-queue ltms)
    (setq ltms (tms-node-ltms in-node))
    (do ((forget-queue (cons in-node nil) (nconc forget-queue new))
	 (new nil nil))
	((null forget-queue) unknown-queue)
      (setq forget-queue (prog1 (cdr forget-queue)
				(rplacd forget-queue unknown-queue)
				(setq unknown-queue forget-queue))
	    node (car unknown-queue))
      (debugging-ltms ltms "~% Retracting ~A." node)
      (setq old-value (tms-node-label node))
      (setf (tms-node-label node) :UNKNOWN)
      (setf (tms-node-support node) nil)
      (dolist (clause (ecase old-value
			(:TRUE (tms-node-false-clauses node))
			(:FALSE (tms-node-true-clauses node))))
	(when (= (incf (clause-pvs clause)) 2)
	  (when (setq node2 (clause-consequent clause))
	    (push node2 new))))
      (if (ltms-complete ltms)
	  (propagate-more-unknownness old-value node ltms)))))

(defun clause-consequent (clause)
  (dolist (term-pair (clause-literals clause))
    (when (eq (tms-node-label (car term-pair)) (cdr term-pair))
      (return (if (eq clause (tms-node-support (car term-pair)))
		  (car term-pair))))))

(defun find-alternative-support (ltms nodes)
  (dolist (node nodes)
    (when (unknown-node? node)
      (check-clauses ltms (tms-node-true-clauses node))
      (check-clauses ltms (tms-node-false-clauses node))))
  (if (eq T (ltms-complete ltms)) (ipia ltms)))

;;; Contradiction handling interface.
(defun check-for-contradictions (ltms &aux violated-clauses)
  (setq violated-clauses
	(delete-if-not #'(lambda (c) (violated-clause? c))
		       (ltms-violated-clauses ltms)))
  (setf (ltms-violated-clauses ltms) violated-clauses) ;; Cache them.
  (if violated-clauses (contradiction-handler ltms violated-clauses)))

(defun contradiction-handler (ltms violated-clauses)
  (cond ((not (ltms-checking-contradictions ltms))
         ;; Update cache of violated clauses
         (setf (ltms-pending-contradictions ltms)
               (delete-if-not #'(lambda (c) (violated-clause? c))
                  (ltms-pending-contradictions ltms)))
         (dolist (vc violated-clauses)
            (when (violated-clause? vc)
               (pushnew vc (ltms-pending-contradictions ltms)))))
	(t (dolist (handler (ltms-contradiction-handlers ltms))
	     (if (funcall handler violated-clauses ltms) (return T))))))

(defmacro without-contradiction-check (ltms &body body)
  (contradiction-check ltms nil body))

(defmacro with-contradiction-check (ltms &body body)
  (contradiction-check ltms t body))

(defun contradiction-check (ltms flag body)
  `(let* ((.ltms. ,ltms)
	  (.old-value. (ltms-checking-contradictions .ltms.)))
     (unwind-protect
	 (progn (setf (ltms-checking-contradictions .ltms.) ,flag)
		,@body)
       (setf (ltms-checking-contradictions .ltms.) .old-value.))))

(defmacro with-contradiction-handler (ltms handler &body body)
  `(let ((.ltms. ,ltms))
     (unwind-protect
	 (progn (push ,handler (ltms-contradiction-handlers .ltms.))
		,@ body)
       (pop (ltms-contradiction-handlers .ltms.)))))

(defmacro with-assumptions (assumption-values &body body)
  ;; Allows assumptions to be made safely, and retracted properly
  ;; even if non-local exits occur.
  `(unwind-protect (progn (dolist (av ,assumption-values)
			    (enable-assumption (car av) (cdr av)))
			 ,@ body)
     (dolist (av ,assumption-values) (retract-assumption (car av)))))

;;; Inquiring about well-founded support
(defun support-for-node (node &aux result support)
  (cond ((null (setq support (tms-node-support node))) nil)
	((eq support :ENABLED-ASSUMPTION) :ENABLED-ASSUMPTION)
	(t (dolist (pair (clause-literals support))
	     (unless (eq (car pair) node)
	       (push (car pair) result)))
	   (values result (clause-informant support)))))

(defun assumptions-of-node (node)
  (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node)) (list node))
	((known-node? node)
	 (assumptions-of-clause (tms-node-support node)))))

(defun assumptions-of-clause (in-clause &aux)
  (do ((clause-queue (list in-clause)
		     (nconc (cdr clause-queue) new-clauses))
       (mark (list nil))
       (node nil)
       (new-clauses nil nil)
       (assumptions nil))
      ((null clause-queue) assumptions)
    (dolist (term-pair (clause-literals (car clause-queue)))
      (setq node (car term-pair))
      (unless (eq (tms-node-mark node) mark)
	(unless (eq (tms-node-label node) (cdr term-pair))
	  (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node))
		 (push node assumptions))
		((null (tms-node-support node)) (ltms-error "Node is unknown" node))
		(t (push (tms-node-support node) new-clauses))))
	(setf (tms-node-mark node) mark)))))

;;; Simple user interface
(proclaim '(special *contra-assumptions*))

(defun ask-user-handler (contradictions ltms)
  (declare (ignore ltms))
  (dolist (contradiction contradictions)
    (if (violated-clause? contradiction)
	(handle-one-contradiction contradiction))))

(defun handle-one-contradiction (violated-clause)
   (let ((*contra-assumptions* (assumptions-of-clause violated-clause))
         (the-answer nil))
      (unless *contra-assumptions* (ltms-error "Global contradiction"
                                    violated-clause))
      (format t "~%Contradiction found:")
      (print-contra-list *contra-assumptions*)
      (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
      (setq the-answer
         (catch 'tms-contradiction-handler
            (cerror "Continue LTRE processing (after retracting an assumption)"
               "LTMS contradiction break")))
      (if the-answer
         (retract-assumption (nth (1- the-answer)
                                *contra-assumptions*)))))

(defun print-contra-list (nodes)
  (do ((counter 1 (1+ counter))
       (nn nodes (cdr nn)))
      ((null nn))
    (format t "~%~A ~A" counter
	    (node-string (car nn)))))

(defun tms-answer (num)
  (if (integerp num)
      (if (> num 0)
	  (if (not (> num (length *contra-assumptions*)))
	      (throw 'tms-contradiction-handler num)
	      (format t "~%Ignoring answer, too big."))
	  (format t "~%Ignoring answer, too small"))
      (format t "~%Ignoring answer, must be an integer.")))

(defun avoid-all (contradictions ignore &aux culprits culprit sign)
  (dolist (contradiction contradictions)
    (when (violated-clause? contradiction)
      (unless (setq culprits (assumptions-of-clause contradiction))
	(ltms-error "Total contradiction" contradiction))
      (setq culprit (car culprits)
	    sign (tms-node-label culprit))
      (retract-assumption culprit)
      (add-nogood culprit sign culprits)
      t)))

(defun clause-antecedents (clause &aux result)
  (dolist (pair (clause-literals clause) result)
    (unless (eq (tms-node-support (car pair)) clause)
      (push (car pair) result))))

(defun signed-node-string (node)
  (if (true-node? node) (node-string node)
      (format nil "~:[Unknown~;Not~][~A]"
	      (false-node? node) (node-string node))))

(defun node-consequences (node &aux conseq conseqs)
  (dolist (cl (ecase (tms-node-label node)
		(:TRUE (tms-node-false-clauses node))
		(:FALSE (tms-node-true-clauses node))))
    (unless (eq cl (tms-node-support node))
      (setq conseq (clause-consequent cl))
      (if conseq (push conseq conseqs))))
  conseqs)

(defun why-node (node)
  (cond ((unknown-node? node)
	 (format t "~%~A is unknown." (node-string node))
	 nil)
	((eq :ENABLED-ASSUMPTION (tms-node-support node))
	 (format t "~%~A is ~A <~A>"
		 (node-string node)
		 (tms-node-label node) (tms-node-support node))
	 nil)
	(t (format t "~%~A is ~A via ~A on"
		   (node-string node)
		   (tms-node-label node)
		   (or (clause-informant (tms-node-support node))
		       (tms-node-support node)))
	   (dolist (term-pair (clause-literals (tms-node-support node)))
	     (unless (equal (tms-node-label (car term-pair))
			    (cdr term-pair))
	       (format t "~%   ~A is ~A"
		       (node-string (car term-pair))
		       (tms-node-label (car term-pair)))))))
  node)

(defun why-nodes (ltms)
  (maphash #'(lambda (ignore n) (why-node n)) (ltms-nodes ltms)))

(defvar *line-count*)

(defun explain-node (node &aux *line-count*)
  (unless (eq (tms-node-label node) :UNKNOWN)
    (setq *line-count* 0)
    (maphash #'(lambda (ignore node) (setf (tms-node-mark node) nil))
	     (ltms-nodes (tms-node-ltms node)))
    (explain-1 node)))


(defun explain-1 (node &aux antecedents)
  (cond ((tms-node-mark node))
	((eq :ENABLED-ASSUMPTION (tms-node-support node))
	 (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~>~15<()~>   Assumption"
		 (incf *line-count*) (true-node? node) (node-string node))
	 (setf (tms-node-mark node) *line-count*))
	(t (setq antecedents
		 (mapcar #'explain-1 (clause-antecedents (tms-node-support node))))
	   (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~> ~15<~A~>  "
		   (incf *line-count*) (true-node? node)
		   (node-string node) antecedents)
	   (pretty-print-clause (tms-node-support node))
	   (setf (tms-node-mark node) *line-count*))))

(defun pretty-print-clauses (ltms)
  (walk-clauses ltms #'(lambda (l)
			 (format T "~% ")
			 (pretty-print-clause l))))

(defun pretty-print-clause (clause)
  (format T "(:OR")
  (dolist (literal (clause-literals clause))
    (format T " ~:[(:NOT ~A)~;~A~]"
	    (eq :TRUE (cdr literal)) (node-string (car literal))))
  (format T ")"))

(defun show-node-consequences (node)
  (let ((conseqs (node-consequences node)))
    (cond (conseqs 
	   (format t "~% Consequences of ~A:" (signed-node-string node))
	   (dolist (conseq conseqs)
		   (format t "~%  ~A" (signed-node-string conseq))))
	  (t (format t "~% ~A has no consequences." (node-string node))))))

(defun node-show-clauses (node)
  (format t "For ~A:" (node-string node))
  (dolist (cl (tms-node-true-clauses node))
    (format T "~%") (pretty-print-clause cl))
  (dolist (cl (tms-node-false-clauses node))
    (format T "~%") (pretty-print-clause cl)))


(defun explore-network (node)
  (unless (known-node? node)
	  (format t "~% Sorry, ~A not believed." (node-string node))
	  (return-from explore-network node))
  (do ((stack nil)
       (current node)
       (mode :ante)
       (options nil)
       (olen 0)
       (done? nil))
      (done? current)
      (cond ((eq mode :ante)
	     (why-node current)
	     (setq options (if (typep (tms-node-support current) 'clause)
			       (clause-antecedents (tms-node-support current))
			     nil)))
	    (t ;; Looking at consequences
	     (show-node-consequences current)
	     (setq options (node-consequences current))))
      (setq olen (length options))
      (do ((good? nil)
	   (choice 0))
	  (good? (case good?
		       (q (return-from explore-network current))
		       (c (setq mode :conseq))
		       (a (setq mode :ante))
		       (0 (if stack
			      (setq current (pop stack))
			      (return-from explore-network current)))
		       (t (push current stack)
			  (setq current (nth (1- good?) options)))))
	  (format t "~%>>>")
	  (setq choice (read))
	  (if (or (eq choice 'q)
		  (eq choice 'c)
		  (eq choice 'a)
		  (and (integerp choice)
		       (not (> choice olen))
		       (not (< choice 0))))
	      (setq good? choice)
	      (format t "~% Must be q, a, c or an integer from 0 to ~D."
		        olen)))))

