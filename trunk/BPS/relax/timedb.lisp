;; -*- Mode: Lisp; -*-

;;;; Allen's temporal logic implemented in WALTZER
;;; Last edited 1/29/93, by KDF

;; Copyright (c) 1988 -- 1992 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (timedb (:PREDICATE temporal-database?)
		   (:PRINT-FUNCTION
		    (lambda (st str ignore) (declare (ignore ignore))
		      (format str "<Temporal DB ~A>" (timedb-name st)))))
  (title "")                ;; String for printing
  (debugging nil)          ;; Debugging flag
  (interval-id 0)          ;; Unique ID
  (intervals nil)          ;; List of intervals
  (relations nil) ;; Temporal relationships
  (transitivity-table nil) ;; Transitivity table
  network)                 ;; A constraint network

(defstruct (interval (:PREDICATE interval?)
		     (:PRINT-FUNCTION
		      (lambda (st str ignore) (declare (ignore ignore))
			(format str "I-~A" (interval-name st)))))
  name
  index      ; for ordering them 
  timedb     ; backpointer
  relations) ; ((<interval> . <relation-cell>))

(defun interval-order (x y)
  (< (interval-index x) (interval-index y)))

(defvar *timedb* nil)  ;; Current temporal database
(defvar *trels-file* "/u/bps/code/relax/allen.lisp")

(defun in-timedb (new-one) (setq *timedb* new-one))

(defun with-timedb (timedb &rest forms)
  `(let ((*timedb* ,timedb)) ,@ forms))
     
(defun create-timedb (title &optional
			   (t-file *trels-file*)
			   (debugging? nil))
  (setq *timedb*
	(make-timedb :TITLE title
		     :NETWORK
		     (create-network
		      (format nil "of ~A" name)))
	:DEBUGGING debugging?)
  (load t-file)
  *timedb*)

;;; Indexing relationships

(defun lookup-trel (i1 i2 &optional (virtual? nil) 
		       (*timedb* *timedb*))
  (let ((old (assoc i2 (interval-relations i1))))
    (if old (cdr old)
	(if virtual? (make-trel i1 i2) nil))))

(defun make-trel (i1 i2)
  (cond ((interval-order i2 i1) (make-trel i2 i1))
	(t (let ((tr (build-cell
		      (cons i1 i2) (timedb-network *timedb*)
		      (timedb-relations *timedb*))))
	     (push (cons i2 tr) (interval-relations i1))
	     (push (cons i1 tr) (interval-relations i2))
	     (clear-cell tr)
	     (find-transitive-relations tr)
	     tr))))

;;; Using transitivity
;; Simple version: installs all transitive relationships.

(defun find-transitive-relations (trel)
  ;; Any relationships in the interval's index
  ;; that have shared elements are fair game.
  (let ((i1 (car (cell-name trel)))
	(i2 (cdr (cell-name trel))))
    (dolist (tpair1 (interval-relations i1))
      (unless (eq (car tpair1) i2)
	(let ((tpair2 (assoc (car tpair1)
			     (interval-relations i2))))
	  (when tpair2
	    ;; Now must find which is which.
	    (let ((i3 (car tpair1)))
	      (cond ((interval-order i1 i3)
		     (cond ((interval-order i3 i2)
			    (build-transitive-constraint
			      (cdr tpair1) (cdr tpair2) trel))
			   (t (build-transitive-constraint
				trel (cdr tpair2)(cdr tpair1)))))
		    (t (build-transitive-constraint
			 (cdr tpair1) trel (cdr tpair2)))))))))))

(defun build-transitive-constraint (tr1 tr2 tr3)
  (let ((con (build-constraint 
	      (list tr1 tr2 tr3) 
	      (timedb-network *timedb*)
	      #'update-trel-transitivity)))
    (add-constraint-cell tr1 con)
    (add-constraint-cell tr2 con)
    (add-constraint-cell tr3 con)
    (setf (constraint-parts con) (list tr1 tr2 tr3))))

;;;; What the constraint does

(defun update-trel-transitivity (con &aux cells diffs possibles)
  (setq cells (constraint-parts con))
  (setq possibles
	(update-possible-trel-values (cell-value (first cells))
				   (cell-value (second cells))))
  (when possibles
    (setq diffs (set-difference (cell-value (third cells))
				possibles))
    (when diffs
      (dolist (d diffs)
	(queue-cell (third cells) :EXCLUDE d con)))))

(defun update-possible-trel-values (vals1 vals2 &aux possibles)
  (setq possibles nil)
  (dolist (v1 vals1)
    (dolist (v2 vals2)
      (setq possibles (union possibles
			     (lookup-transitive-trels v1 v2)))))
  possibles)

(defun lookup-transitive-trels (r1 r2)
  (cdr (assoc r2
        (cdr (assoc r1 
	      (timedb-transitivity-table *timedb*))))))

;;;; User interface

(defmacro interval (i &optional (*timedb* *timedb*))
  `(progn (let ((int (make-interval
		      :NAME ',i
		      :RELATIONS nil
		      :TIMEDB *timedb*
		      :INDEX (incf (timedb-interval-id *timedb*)))))
	    (push (cons ',i int) (timedb-intervals *timedb*)))))

(defun lookup-interval (iname &optional (*timedb* *timedb*))
  (cdr (assoc iname (timedb-intervals *timedb*) :TEST #'equal)))

(defmacro tassert (int1 int2 &optional (rels :NOT-GIVEN)
			(*timedb* *timedb*))
  (when (eq rels :NOT-GIVEN)
	(setq rels (timedb-relations *timedb*)))
  `(temporal-relations (lookup-interval ',int1)
		       (lookup-interval ',int2) ',rels *timedb*))

(defun temporal-relations (a b &optional (possibles nil)
			     (*timedb* *timedb*) &aux trel)
  (setq trel (lookup-trel a b t))
  (when possibles
   (dolist (rel possibles)
    (unless (member rel (timedb-relations *timedb*))
     (error "~A not legitmate temporal relation [~A, ~A, ~A]"
	    rel a b possibles)))
   (dolist (rel (timedb-relations *timedb*))
    (unless (member rel possibles)
	    (queue-cell trel :EXCLUDE rel 'USER))))
  (fire-constraints (timedb-network *timedb*)))

(defun what-time (i1 i2 &aux rel)
  (setq rel (lookup-trel i1 i2))
  (if rel
      (format t "~%~A {~A} ~A" (interval-name i1)
	      (make-relations-string (cell-value rel))
	      (interval-name i2))
    (format t "~%No known relationship between ~A and ~A."
	    (interval-name i1)
	    (interval-name i2))))

(defun what-times (&optional (*timedb* *timedb*))
  (dolist (c (reverse (network-cells
		       (timedb-network *timedb*))))
    (what-time (car (cell-name (cdr c)))
	       (cdr (cell-name (cdr c))))))

(defun make-relations-string (rels)
  (format nil "~{~<~%  ~1:; ~S~>~^,~}" rels))

;;;; Defining a temporal logic, Allen-style

(defmacro defTemporalRelation (sym)
  `(push ',sym (timedb-relations *timedb*)))

(defmacro t-transitivity (r1 r2 &rest options)
  (dolist (op (cons r1 (cons r2 options)))
   (unless (member op (timedb-relations *timedb*))
    (error "~A not a possible temporal relationship [~A, ~A]"
	   op r1 r2)))
 `(let ((r1-entry (assoc ',r1
			 (timedb-transitivity-table *timedb*))))
    (unless r1-entry
	    (setq r1-entry (list ',r1))
	    (push r1-entry (timedb-transitivity-table *timedb*)))
    (push (cons ',r2 ',(copy-list options)) (cdr r1-entry))))
