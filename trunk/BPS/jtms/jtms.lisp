;-*- Mode:  LISP; Syntax: Common-lisp; Package: USER -*-

;;; Justification-based Truth Maintenence System (JTMS)
;;; Version 176.
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (jtms (:PRINT-FUNCTION print-jtms))
  (title nil)					
  (node-counter 0)             ;; unique namer for nodes.
  (just-counter 0)             ;; unique namer for justifications.
  (nodes nil)                  ;; list of all tms nodes.
  (justs nil)                  ;; list of all justifications
  (debugging nil)              ;; debugging flag
  (contradictions nil)         ;; list of contradiction nodes.
  (assumptions nil)            ;; list of assumption nodes.
  (checking-contradictions T)  ;; For external systems
  (node-string nil)
  (contradiction-handler nil)
  (enqueue-procedure nil))

(defun print-jtms (jtms stream ignore)
  (declare (ignore ignore))
  (format stream "#<JTMS: ~A>" (jtms-title jtms)))

(defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
  (index 0)
  (datum nil)           ;; pointer to external problem solver
  (label :OUT)          ;; :IN means believed, :OUT means disbelieved
  (support nil)         ;; Current justification or premise marker
  (justs nil)           ;; Possible justifications
  (consequences nil)    ;; Justifications in which it is an antecedent
  (mark nil)            ;; Marker for sweep algorithms
  (contradictory? nil)  ;; Flag marking it as contradictory
  (assumption? nil)     ;; Flag marking it as an assumption.
  (in-rules nil)	;; Rules that should be triggered when node goes in
  (out-rules nil)	;; Rules that should be triggered when node goes out
  (jtms nil))           ;; The JTMS in which this node appears.

(defun print-tms-node (node stream ignore)
  (declare (ignore ignore))
  (format stream "#<Node: ~A>" (node-string node)))

(defstruct (just (:PRINT-FUNCTION print-just))
  (index 0)
  informant
  consequence
  antecedents)

(defun print-just (just stream ignore)
  (declare (ignore ignore))
  (format stream "#<Just ~D>" (just-index just)))

(defun tms-node-premise? (node &aux support)
  (and (setq support (tms-node-support node))
       (not (eq support :ENABLED-ASSUMPTION))
       (null (just-antecedents support))))

;;; Simple utilities:

(defun node-string (node)
  (funcall (jtms-node-string (tms-node-jtms node)) node))

(defmacro debugging-jtms (jtms msg &optional node &rest args)
  `(when (jtms-debugging ,jtms)
     (format *trace-output* ,msg (if ,node (node-string ,node)) ,@args)))

(defun tms-error (string node) (error string (node-string node)))

(defun default-node-string (n) (format nil "~A" (tms-node-datum n)))

(defun create-jtms (title &key (node-string 'default-node-string)
                               debugging
                               (checking-contradictions t)
                               (contradiction-handler 'ask-user-handler)
                               enqueue-procedure)
  (make-jtms :TITLE title
	     :NODE-STRING node-string
	     :DEBUGGING debugging
	     :CHECKING-CONTRADICTIONS checking-contradictions
	     :CONTRADICTION-HANDLER contradiction-handler
	     :ENQUEUE-PROCEDURE enqueue-procedure
	     ))
	     
(defun change-jtms (jtms &key contradiction-handler node-string
		              enqueue-procedure debugging
                              checking-contradictions)
  (if node-string (setf (jtms-node-string jtms) node-string))
  (if debugging (setf (jtms-debugging jtms) debugging))
  (if checking-contradictions
      (setf (jtms-checking-contradictions jtms)
	    checking-contradictions))
  (if contradiction-handler
      (setf (jtms-contradiction-handler jtms) contradiction-handler))
  (if enqueue-procedure
      (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))

;;; Basic inference-engine interface.

(defun in-node? (node) (eq (tms-node-label node) :IN))

(defun out-node? (node) (eq (tms-node-label node) :OUT))

(defun tms-create-node (jtms datum &key assumptionp contradictoryp)
  (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
			     :DATUM datum
			     :ASSUMPTION? assumptionp
			     :CONTRADICTORY? contradictoryp
			     :JTMS jtms)))
    (if assumptionp (push node (jtms-assumptions jtms)))
    (if contradictoryp (push node (jtms-contradictions jtms)))
    (push node (jtms-nodes jtms))
    node))

;;; Converts a regular node to an assumption and enables it.
(defun assume-node (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node)
    (debugging-jtms jtms "~%Converting ~A into an assumption" node)
    (setf (tms-node-assumption? node) t))
  (enable-assumption node))

(defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-contradictory? node)
    (setf (tms-node-contradictory? node) t)
    (push node (jtms-contradictions jtms))
    (check-for-contradictions jtms)))

(defun justify-node (informant consequence antecedents &aux just jtms)
  (setq jtms (tms-node-jtms consequence)
	just (make-just :INDEX (incf (jtms-just-counter jtms))
			:INFORMANT informant
			:CONSEQUENCE consequence
			:ANTECEDENTS antecedents))
  (push just (tms-node-justs consequence))
  (dolist (node antecedents) (push just (tms-node-consequences node)))
  (push just (jtms-justs jtms))
  (debugging-jtms jtms
		  "~%Justifying ~A by ~A using ~A."
		  consequence
		  informant
		  (mapcar #'node-string antecedents))
  (if (or antecedents (out-node? consequence))
      (if (check-justification just) (install-support consequence just))
      (setf (tms-node-support consequence) just))
  (check-for-contradictions jtms))

;;;; Support for adding justifications

(defun check-justification (just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

(defun justification-satisfied? (just) 
  (every #'in-node? (just-antecedents just)))

(defun install-support (conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

(defun propagate-inness (node &aux (jtms (tms-node-jtms node)) (q (list node)))
  (do () ((null (setq node (pop q))))
    (debugging-jtms jtms "~%   Propagating belief in ~A." node)
    (dolist (justification (tms-node-consequences node))
      (when (check-justification justification)
	(make-node-in (just-consequence justification) justification)
	(push (just-consequence justification) q)))))

(defun make-node-in (conseq reason &aux jtms enqueuef)
  (setq jtms (tms-node-jtms conseq)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     Making ~A in via ~A."
	     conseq
	     (if (symbolp reason)
		 reason
		 (cons (just-informant reason)
		       (mapcar (jtms-node-string jtms)
			       (just-antecedents reason)))))
  (setf (tms-node-label conseq) :IN)
  (setf (tms-node-support conseq) reason)
  (when enqueuef
    (dolist (in-rule (tms-node-in-rules conseq))
      (funcall enqueuef in-rule))
    (setf (tms-node-in-rules conseq) nil)))

;;; Assumption Manipulation
(defun retract-assumption (node &aux jtms)
  (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
    (setq jtms (tms-node-jtms node))
    (debugging-jtms jtms "~%  Retracting assumption ~A." node)
    (make-node-out node)
    (find-alternative-support jtms (cons node (propagate-outness node jtms)))))

(defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
  (unless (tms-node-assumption? node) 
    (tms-error "Can't enable the non-assumption ~A" node))
  (debugging-jtms jtms "~%  Enabling assumption ~A." node)
  (cond ((out-node? node) (make-node-in node :ENABLED-ASSUMPTION)
	                  (propagate-inness node))
	((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (null (just-antecedents (tms-node-support node)))))
	(t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
  (check-for-contradictions jtms))

(defun make-node-out (node &aux jtms enqueuef)
  (setq jtms (tms-node-jtms node)
	enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "~%     retracting belief in ~a." node)
  (setf (tms-node-support node) nil)
  (setf (tms-node-label node) :OUT)
  (if enqueuef (dolist (out-rule (tms-node-out-rules node)) 
		 (funcall enqueuef out-rule)))
  (setf (tms-node-out-rules node) nil))

(defun propagate-outness (node jtms &aux out-queue)
  (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
  (do ((js (tms-node-consequences node) (append (cdr js) new))
       (new nil nil)
       (conseq nil))
      ((null js) out-queue)
    ;;For each justification using the node, check to see if
    ;;it supports some other node.  If so, forget that node,
    ;;queue up the node to look for other support, and recurse
    (setq conseq (just-consequence (car js)))
    (when (eq (tms-node-support conseq) (car js)) 
      (make-node-out conseq)
      (push conseq out-queue)
      (setq new (tms-node-consequences conseq)))))

(defun find-alternative-support (jtms out-queue)
  (debugging-jtms jtms "~%   Looking for alternative supports.")
  (dolist (node out-queue)
    (unless (in-node? node)
      (dolist (just (tms-node-justs node))
	(when (check-justification just)
	  (install-support (just-consequence just)
				 just)
	  (return just))))))

;;; Contradiction handling interface
(defun check-for-contradictions (jtms &aux contradictions)
  (when (jtms-checking-contradictions jtms)
    (dolist (cnode (jtms-contradictions jtms))
      (if (in-node? cnode) (push cnode contradictions)))
    (if contradictions
	(funcall (jtms-contradiction-handler jtms) jtms contradictions))))

(defmacro without-contradiction-check (jtms &body body)
  (contradiction-check jtms nil body))

(defmacro with-contradiction-check (jtms &body body)
  (contradiction-check jtms t body))

(defun contradiction-check (jtms flag body)
  (let ((jtmsv (gensym)) (old-value (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-value (jtms-checking-contradictions ,jtmsv)))
       (unwind-protect
	   (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
	 (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))

(defmacro with-contradiction-handler (jtms handler &body body)
  (let ((jtmsv (gensym)) (old-handler (gensym)))
    `(let* ((,jtmsv ,jtms)
	    (,old-handler (jtms-contradiction-handler ,jtmsv)))
     (unwind-protect
	 (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
       (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

(defun default-assumptions (jtms)
  (with-contradiction-check jtms
    (with-contradiction-handler jtms #'(lambda (&rest ignore)
					 (declare (ignore ignore)) 
					 (throw 'CONTRADICTION t))
      (dolist (assumption (jtms-assumptions jtms))
	(cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION))
	      ((not (eq :DEFAULT (tms-node-assumption? assumption))))
	      ((catch 'CONTRADICTION (enable-assumption assumption))
	       (retract-assumption assumption)))))))

;;; Well-founded support inqueries
(defun supporting-justification-for-node (node) (tms-node-support node))

(defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
  (do ((nodes (list node) (append (cdr nodes) new))
       (new nil nil))
      ((null nodes) assumptions)
    (let ((node (car nodes)))
      (cond ((eq (tms-node-mark node) marker))
	    ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
	     (push node assumptions))
	    ((in-node? node)
	     (setq new (just-antecedents (tms-node-support node)))))
      (setf (tms-node-mark node) marker))))

(defun enabled-assumptions (jtms &aux result)
  (dolist (assumption (jtms-assumptions jtms) result)
    (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
	(push assumption result))))

;;; Inference engine stub to allow this JTMS to be used stand alone
(defun why-node (node &aux justification)
  (setq justification (tms-node-support node))
  (cond ((eq justification :ENABLED-ASSUMPTION)
	 (format t "~%~A is an enabled assumption"
		 (node-string node)))
	(justification
	 (format t "~%~A is IN via ~A on"
		 (node-string node)
		 (just-informant justification))
	 (dolist (anode (just-antecedents justification))
	   (format t "~%  ~A" (node-string anode))))
	(T (format t "~%~A is OUT." (node-string node))))
  node)

(defun why-nodes (jtms)
  (dolist (node (jtms-nodes jtms)) (why-node node)))

(proclaim '(special *contra-assumptions*))

(defun ask-user-handler (jtms contradictions)
  (handle-one-contradiction (car contradictions))
  (check-for-contradictions jtms))

(defun handle-one-contradiction (contra-node
				 &aux the-answer *contra-assumptions*)
  (setq *contra-assumptions* (assumptions-of-node contra-node))
  (unless *contra-assumptions*
    (tms-error "~%There is a flaw in the universe...~A" contra-node))
  (format t "~%Contradiction found: ~A" (node-string contra-node))
  (print-contra-list *contra-assumptions*)
  (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
  (setq the-answer
	(catch 'tms-contradiction-handler
	  (break "JTMS contradiction break")))
  (if (and (integerp the-answer)
	   (> the-answer 0)
	   (not (> the-answer (length *contra-assumptions*))))
      (retract-assumption (nth (1- the-answer)
			       *contra-assumptions*))))

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


(defun explore-network (node)
  (unless (in-node? node)
	  (format t "~% Sorry, ~A not believed." (node-string node))
	  (return-from explore-network node))
  (do ((stack nil)
       (current node)
       (options nil)
       (olen 0)
       (done? nil))
      (done? current)
      (why-node current)
      (setq options (if (typep (tms-node-support current) 'just)
			(just-antecedents (tms-node-support current))))
      (setq olen (length options))
      (do ((good? nil)
	   (choice 0))
	  (good? (case good?
		       (q (return-from explore-network current))
		       (0 (if stack
			      (setq current (pop stack))
			      (return-from explore-network current)))
		       (t (push current stack)
			  (setq current (nth (1- good?) options)))))
	  (format t "~%>>>")
	  (setq choice (read))
	  (cond ((or (eq choice 'q)
		     (and (integerp choice)
			  (not (> choice olen))
			  (not (< choice 0))))
		 (setq good? choice))
		(t (format t
		    "~% Must be q or an integer from 0 to ~D."
		    olen))))))
