;; -*- Mode: Lisp; -*-

;;;; Symbolic relaxation engine
;;; Last edited 2/4/93, by KDF

;; Copyright (c) 1988-1993 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;; This general-purpose program provides a backbone for
;; constraint systems that operate via symbolic relaxation.
;; In such systems, cells have a set of possible values, and
;; the relationships expressed via constraints act by pruning
;; possibilities from these cells.

;; A cell that has just a single value left is said to be
;; FIXED, and a cell that has no possible value is said to be
;; OVERCONSTRAINED.  An overconstrained cell implies the
;; constraints and initial premises are contradictory.

;; A specialized dependency system is used to keep track of
;; decisions, rather than a general-purpose TMS.  A "time-stamp"
;; system is used to allow reconstruction of the course of
;; events without constructing explicit justifications.

(in-package :COMMON-LISP-USER)

(defstruct (network (:PREDICATE network?)
		    (:PRINT-FUNCTION
		     (lambda (st str ignore) (declare (ignore ignore))
		       (format str "<Network ~A>" (network-title st)))))
  (title "")                   ;; Name for printing
  (cells nil)                 ;; Alist
  (constraints nil)           ;; Alist
  (name-test #'eql)           ;; For looking up names
  (equality-test #'equal)     ;; For comparing values
  (cell-queue nil)            ;; Cells to update
  (constraint-queue nil)      ;; Constraints to update
  (timestamp 0)               ;; Network clock 
  (event-list nil)            ;; List of constraint updates
  (debug? nil)                ;; debugging flag
  (status :NEW)               ;; One of :NEW, :QUIESCENT, :IN-PROGRESS, :OVERCONSTRAINED.
  (contradiction-reason nil)  ;; Cell currently overconstrained.
  (contradiction-hook nil)    ;; Procedure to call when overconstrained
  (plist nil))

(defmacro debug-waltzer (network &rest format-args)
  `(when (network-debug? ,network) (format t ,@ format-args)))

(defun create-network (title &key (debug? nil)
			    (contradiction-hook nil)
			    (name-test #'equal)
			    (equality-test #'equal))
  (make-network :TITLE title
		:DEBUG? debug?
		:CONTRADICTION-HOOK contradiction-hook
		:NAME-TEST name-test
		:EQUALITY-TEST equality-test))

;;;; Defining the parts of a network

(defstruct (cell (:PREDICATE cell?)
		 (:PRINT-FUNCTION
		  (lambda (st str ignore) (declare (ignore ignore))
		    (format str "<Cell ~A>" (cell-name st)))))
	   name	               ;; name for indexing
	   network             ;; what it is part of  
	   value               ;; Currently believed values
	   (constraints nil)   ;; Constraints it participates in
	   possible-values     ;; The set of values it can have
	   out-reasons         ;; list of exclusion reasons
	   (plist nil))        ;; Other properties

(defun build-cell (name network possible-values &aux cell)
  (setq cell (make-cell :NAME name
			:NETWORK network
			:POSSIBLE-VALUES possible-values))
  (push (cons name cell) (network-cells network))
  cell)

(defstruct (constraint (:PREDICATE constraint?)
		       (:PRINT-FUNCTION (lambda (st str ignore)
					  (declare (ignore ignore))
					  (format str "<Constraint ~A>"
						  (constraint-name st)))))
	   name                   ;; name for indexing
	   network                ;; what it is part of
	   (parts nil)            ;; cells it relates
	   (update-procedure nil) ;; Call on constraint to udpate it
	   (queued? nil)          ;; If non-NIL, already queued.
	   (plist nil))           ;; Other properties

(defun build-constraint (name network updater &aux con)
  (setq con (make-constraint :NAME name
			     :NETWORK network
			     :UPDATE-PROCEDURE updater))
  (push (cons name con) (network-constraints network))
  (queue-constraint con)
  con)

(defun add-constraint-cell (cell constraint)
  (push constraint (cell-constraints cell)))

;;; Initialization

(defun clear-network (net)
  (setf (network-constraint-queue net) nil)
  (setf (network-cell-queue net) nil)
  (setf (network-event-list net) nil)
  (setf (network-timestamp net) 0)
  (setf (getf (network-plist net) :STACK) nil)
  (dolist (cell-entry (network-cells net)) (clear-cell (cdr cell-entry)))
  (dolist (con-entry (network-constraints net))
    (setf (constraint-queued? (cdr con-entry)) nil)
    (queue-constraint (cdr con-entry)))
  (setf (network-status net) :NEW))

(defun clear-cell (cell)
  (setf (cell-value cell) (copy-list (cell-possible-values cell)))
  (setf (getf (cell-plist cell) :STACK) nil)
  (setf (cell-out-reasons cell)
	(mapcar #'(lambda (pval) (cons pval :IN))
		(cell-possible-values cell))))

(defun push-network (net)  ;; assumes the network is quiescent
  (unless (or (eq (network-status net) :NEW)
	      (eq (network-status net) :QUIESCENT))
	  (error "~% Cannot take snapshot of ~A in state ~A"
		 net (network-status net)))
  (debug-waltzer net " Pushing state of ~A at ~A.."
		 (network-title net) (network-timestamp net))
  (push (list (network-timestamp net)
	      (network-status net)
	      (network-event-list net))
	(getf (network-plist net) :STACK))
  (dolist (cell-entry (network-cells net)) (push-cell (cdr cell-entry))))

(defun push-cell (cell)
  (push (cons (copy-list (cell-value cell))
	      (copy-tree (cell-out-reasons cell)))
	(getf (cell-plist cell) :STACK)))

(defun pop-network (net &aux state)
  (unless (getf (network-plist net) :STACK)
	  (error "No state saved in ~A." net))
  (setf (network-constraint-queue net) nil)
  (setf (network-cell-queue net) nil)
  (setq state (pop (getf (network-plist net) :STACK)))
  (setf (network-timestamp net) (car state))
  (setf (network-status net) (cadr state))
  (setf (network-event-list net) (third state))
  (dolist (cell-entry (network-cells net)) (pop-cell (cdr cell-entry)))
  (dolist (con-entry (network-constraints net))
    (setf (constraint-queued? (cdr con-entry)) nil))
  (debug-waltzer net "~% ..Popped ~A back to ~A."
		 (network-title net) (network-timestamp net)))

(defun pop-cell (cell &aux state)
  (setq state (pop (getf (cell-plist cell) :STACK)))
  (setf (cell-value cell) (car state))
  (setf (cell-out-reasons cell) (cdr state)))

;;;; Updating cells
;;
;; There are two inferential operations on a cell.
;; (EXCLUDE <cell> <value> <&optional (informant :USER)>)
;; says that the informant has declared a particular value to no
;; longer be a consistent possibility for that cell.
;; (PICK <cell> <value> <&optional (informant :USER)>)
;; says that the informant has declared a particular possible
;; value to be the only consistent one.

(defun exclude (cell value &optional (informant :USER)
		     &aux net entry)
  (setq net (cell-network cell))
  (debug-waltzer net "~%   Excluding ~A from ~A via ~A" 
		 value (cell-name cell) informant)
  (setq entry (assoc value (cell-out-reasons cell)
		     :TEST (network-equality-test net)))
  (cond ((eq (cdr entry) :IN) ; must exclude it
	 (setf (cdr entry) (cons informant (network-timestamp net)))
	 (setf (cell-value cell) (delete value (cell-value cell)
					 :TEST (network-equality-test net)))
	 (when (null (cell-value cell))
	       (signal-contradiction cell 'Overconstrained))
	 (dolist (constraint (cell-constraints cell))
	   (queue-constraint constraint)))
	(t ;already excluded for some reason
	 (debug-waltzer net " -- already excluded."))))

(defun pick (cell value &optional (informant :USER)
		  &aux net entry)
  (setq net (cell-network cell))
  (debug-waltzer net "~%  Selecting ~A for ~A, via ~A."
		 value (cell-name cell) informant)
  (setq entry (assoc value (cell-out-reasons cell)
		     :TEST (network-equality-test net)))
  (cond ((eq (cdr entry) :IN) ;pick it by flushing the others
	 (dolist (other (cell-out-reasons cell))
	   (unless (funcall (network-equality-test net) value (car other))
	     (exclude cell (car other) informant))))
	(t (signal-contradiction cell 'dead-choice))))

(defun update (constraint)
  ;; These are data-driven, so not much to do here.
  (funcall (constraint-update-procedure constraint) constraint))

;;;; Running it

(defun fire-constraints (net)
  (when (eq (network-status net) :OVERCONSTRAINED)
	(return-from FIRE-CONSTRAINTS nil))
  (setf (network-status net) :IN-PROGRESS)
  (debug-waltzer net "~% Beginning propagation on ~A.." (network-title net))
  (do ((nconstraints 0)
       (ncells 0)
       (con nil))
      ((or (and (null (network-cell-queue net))
		(null (network-constraint-queue net)))
	   (eq (network-status net) :OVERCONSTRAINED))
       (debug-waltzer net "~% .. finished propagating through ~A." (network-title net))
       (unless (eq (network-status net) :OVERCONSTRAINED)
	       (setf (network-status net) :QUIESCENT))
       (values ncells nconstraints))
    (cond ((network-cell-queue net)
	   (incf ncells)
	   (eval (pop (network-cell-queue net))))
	  (t (setq con (pop (network-constraint-queue net)))
	     (incf nconstraints)
	     (incf (network-timestamp net))
	     (push con (network-event-list net))
	     (setf (constraint-queued? con) nil)
	     (update con)))))

(defun queue-cell (cell message value informant)
  (push `(,(case message
	     (:EXCLUDE 'exclude)
	     (:PICK 'pick)) ,cell ',value ',informant)
	(network-cell-queue (cell-network cell))))

(defun queue-constraint (constraint)
  (unless (constraint-queued? constraint)
    (setf (constraint-queued? constraint) t)
    (push constraint (network-constraint-queue
		      (constraint-network constraint)))))

(defun check-constraints (net)
  (dolist (con-entry (network-constraints net))
    (queue-constraint (cdr con-entry)))
  (fire-constraints net))

(defun signal-contradiction (cell message &aux net)
  (setq net (cell-network cell))
  (debug-waltzer net "~%Contradiction concerning  cell(s) ~A: ~A"
		 cell message)
  (setf (network-status net) :OVERCONSTRAINED)
  (setf (network-contradiction-reason net) cell)
  (when (network-contradiction-hook net)
	(funcall (network-contradiction-hook net) net)))

;;; Interrogatives

;;; Network properties first, then cellular queries

(defun lookup-cell (name net)
  (cdr (assoc name (network-cells net)
	      :TEST (network-name-test net))))

(defun lookup-constraint (name net)
  (cdr (assoc name (network-constraints net)
	      :TEST (network-name-test net))))

(defun what-are (net)
  (format t "~% ~A is ~A" net (network-status net))
  (dolist (cell-entry (network-cells net)) (what-is (cdr cell-entry))))

(defun determined? (net)
  (and (eq (network-status net) :QUIESCENT)
       (every #'(lambda (cell-entry) (known? (cdr cell-entry)))
	      (network-cells net))))

(defun to-plunk (net &aux result)
  (dolist (cell-entry (network-cells net) result)
	  (when (cdr (cell-value (cdr cell-entry)))
		(push (cdr cell-entry) result))))
	
;;;; Interrogating cells

(defun what-is (cell)
  (cond ((null (cell-value cell))
	 (format t "~%  ~A is overconstrained." (cell-name cell)))
	((cdr (cell-value cell))
	 (format t "~%  ~A is one of ~A." (cell-name cell)
		 (cell-value cell)))
	(t (format t "~%  ~A = ~A." (cell-name cell)
		   (car (cell-value cell))))))

(defun known? (cell)
  (and (cell-value cell) (null (cdr (cell-value cell)))))

(defun value (cell)
  (unless (known? cell)
	  (error "~%~A not known." (cell-name cell)))
  (car (cell-value cell)))

;;; Network search procedure.

(defun search-network (net consistent-proc contra-proc)
  (labels
   ((search-thru-plunkable-cells (cells indent)
     (cond ((eq (network-status net) :OVERCONSTRAINED)
	    (funcall contra-proc net))
	   ((determined? net) (funcall consistent-proc net))
	   ((null cells) ;; Should be either determined or contradictory, bug!
	    (error "Inconsistent network status -- ~A" net))
	   (t ;; Now we explore possibilities for the next cell 
	    (dolist (val (copy-list (cell-value (car cells))))
	      (format t "~%~A Trying ~A for ~A" indent val (cell-name (car cells)))
	      (push-network net)
	      (pick (car cells) val :SEARCH)
	      (fire-constraints net)
	      (search-thru-plunkable-cells (cdr cells) (concatenate 'string indent " "))
	      (pop-network net))))))
  (when (determined? net)
	(funcall consistent-proc net)
	(return-from search-network net))
  (search-thru-plunkable-cells (to-plunk net) "")))

(defun say-solution (net)
  (unless (determined? net)
    (error "Say-solution called with unsolved network ~A." (network-title net)))
  (format t "~% A solution for ~A:" (network-title net))
  (what-are net)
  (break "Consistent solution"))

(defun say-contradiction (net)
  (unless (eq (network-status net) :OVERCONSTRAINED)
    (error "Say-contradiction called on okay network ~A." (network-title net)))
  (format t "~% ~A overconstrained, due to ~A."
	  (network-title net)
	  (cell-name (network-contradiction-reason net))))

(defun show-search (net)
  (search-network net (function say-solution) (function say-contradiction)))
