;; -*- Mode: Lisp; -*-

;;;; Inequality reasoning module for TGIZMO
;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *tgizmo*))

(defun num-order (n1 n2) (string< (format nil "~A" n1) (format nil "~A" n2)))

(defun individual-of (q-or-n) ;; Presumes single owner
    (cond ((not (listp q-or-n)) nil)
	  ((or (eq (car q-or-n) 'A)
	       (eq (car q-or-n) 'D)) (individual-of (cadr q-or-n)))
	  (t (cadr q-or-n))))

(defun quantity-of (q-or-n)
  (cond ((null q-or-n) (error "Bad format for quantity: ~A" q-or-n))
	((not (listp q-or-n)) nil)
	((or (eq (car q-or-n) 'A) (eq (car q-or-n) 'D)) (cadr q-or-n))
	((or (null (cdr q-or-n)) (cddr q-or-n))
	 (error "Bad format for quantity: ~A" q-or-n))
	(t q-or-n)))

(defun install-comparison-constraints-if-needed (n1 n2 &aux cycles)
  (when (equal n1 n2) (error "Can't do self-comparisons: ~A." n1))
  (if (num-order n2 n1) (psetq n1 n2 n2 n1))
  (unless (member (cons n1 n2) (tgizmo-comparisons *tgizmo*) :TEST #'equal)
	  (install-comparison-constraints n1 n2)
	  ;; Preprocess connectivity for transitivity inferences
	  (setq cycles (find-comparison-cycles-for n1 n2))
	  (when cycles
		(debugging-tgizmo :COMP
				  "~%  ..Found new comparison cycles:~%~A" cycles)
		(setf (tgizmo-update-ineqs? *tgizmo*) t)
		(setf (tgizmo-comp-cycles *tgizmo*)
		      (nconc cycles
			     (tgizmo-comp-cycles *tgizmo*))))
	  ;; Finaly, record the comparison as potentially interesting.
	  (push (cons n1 n2) (tgizmo-comparisons *tgizmo*))))

;;;; Internal consistency and updating for comparisons

(defun install-comparison-constraints (n1 n2)
  ;; When one of them is not a quantity, perhaps because
  ;;  the individual in question does not exist,
  ;; the comparison is irrelevant
  (let ((q1 (quantity-of n1))
	(q2 (quantity-of n2))
	(node nil))
    (unless (or q1 q2) (error "Can't compare constants: ~A, ~A" q1 q2))
    (assert! `(:IFF ,(if q1
			 (if q2 `(:OR (:NOT (Quantity ,q1))
				      (:NOT (Quantity ,q2)))
			   `(:NOT (Quantity ,q1)))
		       `(:NOT (Quantity ,q2)))
		    (:AND (:NOT (,n1 <= ,n2))
			  (:NOT (,n2 <= ,n1))))
	     :EXISTENCE-CONSTRAINT)
    ;; Ensure transitivity checked whenever new information
    ;; arrives.
    (setq node (get-tms-node `(,n1 <= ,n2)))
    (push `(update-ineqs-as-needed ,node :TRUE)
	  (tms-node-true-rules node))
    (push `(update-ineqs-as-needed ,node :FALSE)
	  (tms-node-false-rules node))
    (setq node (get-tms-node `(,n2 <= ,n1)))
    (push `(update-ineqs-as-needed ,node :TRUE)
	  (tms-node-true-rules node))
    (push `(update-ineqs-as-needed ,node :FALSE)
	  (tms-node-false-rules node)) 
    node))

(defun update-ineqs-as-needed (node label)
  (setf (tgizmo-update-ineqs? *tgizmo*) t)
  (case label
	(:TRUE (push `(update-ineqs-as-needed ,node ,label)
		     (tms-node-true-rules node)))
	(:FALSE (push `(update-ineqs-as-needed ,node ,label)
		      (tms-node-false-rules node)))
	(t (error "Inappropriate label ~A in update-ineqs-as-needed: ~A."
		  label node))))

;;;; Test predicates

(defun greater-than? (n1 n2)
  (and (true? `(,n2 <= ,n1)) (false? `(,n1 <= ,n2))))

(defun less-than? (n1 n2)
  (and (true? `(,n1 <= ,n2)) (false? `(,n2 <= ,n1))))

(defun equal-to? (n1 n2)
  (and (true? `(,n2 <= ,n1)) (true? `(,n1 <= ,n2)))) 

(defun rel-value (n1 n2)
  (let ((le `(,n1 <= ,n2))
	(ge `(,n2 <= ,n1)))
    (cond ((true? le)
	   (cond ((true? ge) :=)
		 ((false? ge) :<)
		 (t :<=)))
	  ((false? le)
	   (cond ((true? ge) :>)
		 ((false? ge) :BT)
		 (t :>=)))
	  ((true? ge) :>=)
	  (t :??))))

(defun rel-value-clause (n1 n2 rel)
  (case rel
	(:<= `(,n1 <= ,n2))
	(:>= `(,n2 <= ,n1))
	(:= `(:AND (,n1 <= ,n2) (,n2 <= ,n1)))
	(:< `(:AND (,n1 <= ,n2) (:NOT (,n2 <= ,n1))))
	(:> `(:AND (:NOT (,n1 <= ,n2)) (,n2 <= ,n1)))))

(defun comparison? (n1 n2 &optional (*tgizmo* *tgizmo*))
  (install-comparison-constraints-if-needed n1 n2)
  (use-transitivity)
  (rel-value n1 n2))

;;;; Helpers for asserting inequalities

(defun greater-than! (n1 n2 &optional (reason :USER))
  (assume! `(:NOT (,n1 <= ,n2)) reason)
  (assume! `(,n2 <= ,n1) reason))

(defun less-than! (n1 n2 &optional (reason :USER))
  (assume! `(,n1 <= ,n2) reason)
  (assume! `(:NOT (,n2 <= ,n1)) reason))

(defun equal-to! (n1 n2 &optional (reason :USER))
  (assume! `(,n1 <= ,n2) reason)
  (assume! `(,n2 <= ,n1) reason))

;;; The next three procedures are used in code which 
;;; produces justifications.

(defun lt-forms (n1 n2)
  `((,n1 <= ,n2) (:NOT (,n2 <= ,n1))))

(defun eq-forms (n1 n2)
  `((,n1 <= ,n2) (,n2 <= ,n1)))

(defun gt-forms (n1 n2)
  `((:NOT (,n1 <= ,n2)) (,n2 <= ,n1))) 

(defun lte-forms (n1 n2) `((,n1 <= ,n2)))

(defun gte-forms (n1 n2) `((,n2 <= ,n1)))

;;;; Transitivity computations

(defun use-transitivity (&optional (*tgizmo* *tgizmo*))
  (do () ((not (tgizmo-update-ineqs? *tgizmo*)))
      (setf (tgizmo-update-ineqs? *tgizmo*) nil)
      (dolist (cycle (tgizmo-comp-cycles *tgizmo*))
	      (check-comp-cycle cycle))
      (tg-run-rules)))

(defun find-comparison-cycles-for (n1 n2 &aux start)
  ;; Queue entries need to be (<n3> <n1>)
  (do ((queue (mapcar #'(lambda (n) (list n n1))
		      (find-comparison-set n1 n2))
	      (nconc (cdr queue) new-entries))
       (new-entries nil nil)
       (candidates nil)
       (cycles nil))
      ((null queue) (mapcar #'make-comp-cycle cycles))
      (cond ((equal (caar queue) n2) ;; Got one
	     (push  (car queue) cycles))
	    (t (dolist (new-num
			(find-comparison-set (caar queue)
					     (cadar queue)))
		       (unless (member new-num
				       (car queue) :TEST #'equal)
			       (push (cons new-num (car queue))
				     new-entries)))))))

(defun make-comp-cycle (number-list)
  (do ((nums number-list (cdr nums))
       (cycle nil))
      ((null (cdr nums))
       (nreverse (cons (cons (car nums) (car number-list))
		       cycle)))
      (push (cons (car nums) (cadr nums)) cycle)))

(defun find-comparison-set (n1 &optional (skip nil)
			       &aux others)
  (dolist (other (tg-fetch `(,n1 <= ?other)) others)
	  (unless (and skip (equal (third other) skip))
		  (push (third other) others))))


(defun check-comp-cycle (cycle &aux lts gts eqs leqs geqs unks)
  ;; Squeezes out any inferences that can be made via transitivity
  ;; Recall cycle is a list of pairs ((n1 . n2) ... (ni . n1)), 
  ;; where comparisons link each of the numbers.
  ;; What to do with a cycle can be defined by analogy with
  ;; BCP on clauses.  That is, some labellings are inconsistent,
  ;; while some partial labellings can lead to others being labelled.  
  (dolist (pair cycle)
	  (case (rel-value (car pair) (cdr pair))
		(:= (push pair eqs))
		(:< (push pair lts))
		(:> (push pair gts))
		(:<= (push pair leqs))
		(:>= (push pair geqs))
		(:?? (push pair unks))))
  (cond ((cdr unks)) ;; Too many unknowns
	((null unks) ;; Look for violations, hardening
	 (cond ((not (or leqs geqs)) ;; All hard
		(cond ((or (and (null gts) (null lts)) 
			   (and gts lts))) ;; Okay, all = or mixed
		      (t (contradiction ;; Net > or net <, impossible
			  (find-cycle-support cycle)
			  (tgizmo-ltre *tgizmo*)))))
	       ((null leqs) ;; must be geqs
		(cond (lts) ;; Could be either, no constraint
		      (gts ;; Contradiction, net effect must be >
		       (contradiction (find-cycle-support cycle)
				      (tgizmo-ltre *tgizmo*)))
		      (t ;; geq's -> eq's
		       (assert! `(:IMPLIES
				  (:AND ,@ (find-cycle-support cycle))
				  (:AND ,@ (mapcar #'(lambda (pair)
						       (rel-value-clause (car pair) (cdr pair)
									 :=))
						   geqs)))
				:HARDENING->=))))
	       (t ;; Must be leqs
		(cond (gts) ;; Could be either, no constraint
		      (lts ;; Contradiction, net effect must be <
		       (contradiction (find-cycle-support cycle)
				      (tgizmo-ltre *tgizmo*)))
		      (t ;; leq's -> eq's
		       (assert! `(:IMPLIES
				  (:AND ,@ (find-cycle-support cycle))
				  (:AND ,@ (mapcar #'(lambda (pair)
						       (rel-value-clause (car pair) (cdr pair)
									 :=))
						   leqs)))
				:HARDENING-<=))))))
	(t ;; Can last one be concluded?
	 (cond ((and lts gts)) ;; no constraint
	       (lts (if (null geqs) ;; Last must be gt.
			(assert! `(:IMPLIES
				   (:AND ,@ (find-cycle-support cycle))
				   ,(rel-value-clause (caar unks) (cdar unks) :>))
				  :>-VIA-TRANS)))
	       (gts (if (null leqs) ;; Last must be lt.
			(assert! `(:IMPLIES
				   (:AND ,@ (find-cycle-support cycle))
				   ,(rel-value-clause (caar unks) (cdar unks) :<))
				 :<-VIA-TRANS)))
	       (eqs (assert! `(:IMPLIES ;; Last must be =.
			       (:AND ,@ (find-cycle-support cycle))
			       ,(rel-value-clause (caar unks) (cdar unks) :=))
			     :=-VIA-TRANS))
	       ((null geqs) ;; All leq's plus unknown.  Must be geq.
		(assert! `(:IMPLIES (:AND ,@ (find-cycle-support cycle))
				    ,(rel-value-clause (caar unks) (cdar unks) :>=))
			 :GEQ-VIA-TRANS))
	       ((null leqs) ;; All geq's plus unknown.  Must be leq.
		(assert! `(:IMPLIES (:AND ,@ (find-cycle-support cycle))
				    ,(rel-value-clause (caar unks) (cdar unks) :<=))
			 :LEQ-VIA-TRANS))
	       (t (error "Bad length cycle: ~A" cycle))))))

;;; Support code

(defun find-cycle-support (cycle &aux support form)
  (dolist (pair cycle support)
   (when (setq form (signed-form `(,(car pair) <= ,(cdr pair))))
	 (push form support))
   (when (setq form (signed-form `(,(cdr pair) <= ,(car pair))))
	 (push form support))))

(defun signed-form (form)
  (case (label-of form)
	(:TRUE form)
	(:FALSE (list :NOT form))
	(t nil)))

(defun show-comp-cycles (&optional (*tgizmo* *tgizmo*) &aux (counter 0))
  (dolist (cycle (tgizmo-comp-cycles *tgizmo*))
	  (format t "~%~D: " counter)
	  (dolist (pair cycle) (format t "~A " (number-string (car pair))))
	  (incf counter))
  counter)

(defun show-ineqs (&optional (*tgizmo* *tgizmo*))
  (dolist (comp (tgizmo-comparisons *tgizmo*))
	  (format t "~%  ~A ~A ~A" (number-string (car comp))
		  (rel-value (car comp) (cdr comp))
		  (number-string (cdr comp)))))

;;; Test cases

(setq *ineq-test1* '((> (A (T i1)) (A (T i2)))
		       (= (A (T i2)) (A (T i3)))
		       (> (A (T i3)) (A (T i4)))
		       (< (A (T i4)) (A (T i1)))
		       (> (A (T i3)) (A (T i1))))) ;; Should be 3 cycles

(setq *ineq-test2* '((> (A (T fred)) (A (T george)))
		     (> (A (T george)) (A (T mable)))
		     (= (A (T mable)) (A (T anabelle)))
		     (> (A (T anabelle)) (A (T pat)))))
