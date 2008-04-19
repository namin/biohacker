;; -*- Mode: Lisp; -*-

;;;; Database for LTRE
;;;; Last Edited: 4/27/94, by KDF

;;; Copyright 1986 - 1995 Kenneth D. Forbus, 
;;; Northwestern University, and Johan de Kleer, Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (dbclass (:PRINT-FUNCTION dbclass-print-procedure))
  name    ; Corresponding symbol
  ltre    ; LTRE it is part of.
  facts   ; Associated facts
  rules)  ; Associated rules

(defun dbclass-print-procedure (r st ignore)
  (declare (ignore ignore))
  (format st "<Dbclass ~A>" (dbclass-name r)))

(defstruct (datum (:PRINT-FUNCTION datum-print-procedure))
  counter              ; Unique ID for easy lookup
  ltre                 ; The LTRE it is part of
  lisp-form            ; Expression for pattern-matching
  (tms-node nil)       ; Pointer into TMS
  dbclass                ; Dbclass of the corresponding pattern
  (assumption? nil)    ; if non-nil, indicates informant
  (plist nil))          ; local property list

(defun datum-print-procedure (d st ignore)
  (declare (ignore ignore)) 
  (format st "<Datum ~D>" (datum-counter d)))

;; Previous version used ALPHALESSP, a refuge from MACLISP that
;; compared the printed representations of any two lisp objects (as
;; defined by FORMAT.  Given that we only use it for database forms,
;; exploiting the datum counter to provide a cheap ordering constraint
;; between assertions makes more sense.

(defun form< (x y) (< (datum-counter (referent x t))
                      (datum-counter (referent y t))))


;;;; Adding data

(defvar *connective-list* '(:IMPLIES :AND :OR :IFF :NOT :TAXONOMY))

(defun simple-proposition? (x)
  (or (not (listp x))
      (not (member (car x) *connective-list*))))

(defun negated-proposition? (form)
  (and (listp form) (eq (car form) :NOT)
       (simple-proposition? (cadr form))))

(defun assert! (fact just &optional (*LTRE* *LTRE*))
  (debugging-ltre "~%    Asserting ~A via ~A." fact just)
  ;; For assertions, simply install the clause.
  (add-formula (ltre-ltms *ltre*) (build-tms-formula fact *LTRE*) just))

(defun assume! (fact reason &optional (*LTRE* *LTRE*) &aux datum node)
  (setq datum (referent (if (negated-proposition? fact) (cadr fact)
			  fact) t)
	node (datum-tms-node datum))
  (debugging-ltre "~%    Assuming ~A via ~A." fact reason)
  (unless (or (negated-proposition? fact) (simple-proposition? fact))
	  ;; Install clauses corresponding to the assumption, with proper
	  ;; logical scoping
	  (add-formula (ltre-ltms *ltre*)
             `(:IMPLIES ,node ,(build-tms-formula fact *LTRE*)) reason))
  (cond ((not (datum-assumption? datum))
	 (setf (datum-assumption? datum) reason)
	 (convert-to-assumption node)
	 (enable-assumption node (if (negated-proposition? fact) :FALSE :TRUE)))
	((eq reason (datum-assumption? datum)))
	(t (error "Fact ~A assumed because of ~A assumed again because of ~A"
		  (show-datum datum) (datum-assumption? datum) reason)))
  datum)

(defun already-assumed? (fact) (datum-assumption? (referent fact t)))

(defun quiet-assert! (fact *LTRE* &optional (just 'user))
  (without-contradiction-check (ltre-ltms *LTRE*)
		  (assert! fact *LTRE* just)))

(defmacro rassert! (fact &optional (just 'user))
  `(assert! ,(quotize fact) ,(quotize just)))

(defun build-tms-formula (formula *LTRE*)
  (cond ((and (listp formula)
	      (member (car formula) *connective-list*))
	 (cons (car formula)
	       (mapcar #'(lambda (form)
			   (build-tms-formula form *LTRE*)) (cdr formula))))
	(t (datum-tms-node (referent formula t)))))

;;; Retraction and "bulk assumptions"

(defun retract! (fact just &optional (*LTRE* *LTRE*) (quiet? t)
		      &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond ((not (tms-node-assumption? node))
	 (unless quiet?
	   (format t "~%~A isn't an assumption." (show-datum datum))))
	((not (known-node? node))
	 (unless quiet?
	   (format T "~%The assumption ~A is not currently in." fact)))
	((eq just (datum-assumption? datum))
	 (debugging-ltre "~%    Retracting ~A via ~A." fact just)
	 (setf (datum-assumption? datum) nil)
	 (retract-assumption node))
	((not quiet?)
	 (format t "~%~A not source of assumption for ~A"
		 just fact)))
  node)

(defmacro rretract! (fact &optional (just 'USER))
  `(retract! ,(quotize fact) ,(quotize just)))

(defun contradiction (losers *LTRE* &aux datum trues falses)
  ;; Here we re-cycle this procedure to indicate that a
  ;; conjunction of facts are contradictory.
  (dolist (fact losers
		(add-clause trues falses :DECLARED-CONTRADICTION))
    (setq datum (referent fact t))
    (if (negated-proposition? fact)
	(push (datum-tms-node datum) falses)
      (push (datum-tms-node datum) trues))))

(defmacro assuming (facts-to-assume *LTRE* &body body)
  ;; A useful abstraction for user code.  
  `(with-assumptions
    (mapcar #'(lambda (fact &aux node datum)
		(setq datum (referent fact t)
		      node (datum-tms-node datum))
		(convert-to-assumption node)
		(cons node (if (negated-proposition? fact) :FALSE :TRUE)))
	    ,facts-to-assume)
    ,@ body))

;;;; Database system

(defun get-dbclass (fact &optional (*LTRE* *LTRE*) &aux dbclass)
  (cond ((null fact) (error "~% NIL can't be a dbclass."))
	((listp fact) 
	 (cond ((negated-proposition? fact) (get-dbclass (cadr fact) *LTRE*))
	       (t (get-dbclass (car fact) *LTRE*))))
	((variable? fact)
	 (cond ((boundp fact) (get-dbclass (symbol-value fact) *LTRE*))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((gethash fact (ltre-dbclass-table *LTRE*)))
	       (t (setq dbclass
			(make-dbclass :NAME fact :FACTS nil
				    :RULES nil :LTRE *LTRE*))
		  (setf (gethash fact (ltre-dbclass-table *LTRE*)) dbclass)
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

(defun referent (fact &optional (virtual? nil) (*LTRE* *LTRE*))
  (if virtual? (insert fact) (referent1 fact)))

(defun referent1 (fact &aux form) ;; Could use seperate hash table
  (setq form (if (negated-proposition? fact) (cadr fact) fact))
  (dolist (candidate (dbclass-facts (get-dbclass fact *LTRE*)))
	  (when (equal (datum-lisp-form candidate) form)
		(return candidate))))

(defun insert (fact &aux datum form)
  (setq datum (referent1 fact))
  (cond (datum (values datum t))
	(t (setq form (if (negated-proposition? fact) (cadr fact) fact))
	   (setq datum (make-datum :COUNTER (incf (ltre-datum-counter *LTRE*))
				   :LTRE *LTRE*
				   :LISP-FORM form
				   :DBCLASS (get-dbclass form *LTRE*)))
	   (setf (datum-tms-node datum) (tms-create-node (ltre-ltms *LTRE*) datum))
	   (push datum (dbclass-facts (datum-dbclass datum)))
	   (try-rules datum) 
	   (values datum nil))))

(defun fetch (pattern &optional (*LTRE* *LTRE*) &aux bindings unifiers form)
  (setq form (if (negated-proposition? pattern) (cadr pattern) pattern))
  (dolist (candidate (get-candidates form *LTRE*) unifiers)
    (setq bindings (unify form (datum-lisp-form candidate)))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

(defun get-candidates (pattern *LTRE*)
  (dbclass-facts (get-dbclass pattern *LTRE*)))

(defun map-dbclass (proc &optional (*LTRE* *LTRE*))
  (maphash #'(lambda (name dbclass) (declare (ignore name))
	       (funcall proc dbclass))
	   (ltre-dbclass-table *LTRE*)))

;;;; Interface and display of data

(defun true? (fact &optional (*LTRE* *LTRE*) &aux r)
  (when (setq r (referent fact nil))
    (if (negated-proposition? fact)
	(false-node? (datum-tms-node r))
	(true-node? (datum-tms-node r)))))

(defun false? (fact &optional (*LTRE* *LTRE*) &aux r)
  (when (setq r (referent fact nil))
    (if (negated-proposition? fact)
	(true-node? (datum-tms-node r))
	(false-node? (datum-tms-node r)))))

(defun known? (fact &optional (*LTRE* *LTRE*) &aux r)
  (when (setq r (referent fact nil))
    (known-node? (datum-tms-node r))))

(defun unknown? (fact &optional (*LTRE* *LTRE*) &aux r)
  (if (setq r (referent fact nil))
      (unknown-node? (datum-tms-node r))
      t))

(defun label-of (fact &optional (*ltre* *ltre*) &aux r)
  (when (setq r (referent fact nil))
	(tms-node-label (datum-tms-node r))))

(defun why? (fact &optional (*LTRE* *LTRE*) &aux r)
  (cond ((setq r (referent fact nil))
	 (why-node (datum-tms-node r))
	 (if (and (known-node? (datum-tms-node r))
		  (datum-assumption? r)) 
	     (format t " (~A)" (datum-assumption? r))))
	(t (format t "~%~A not in database." fact))))

(defun get-tms-node (fact &optional (*LTRE* *LTRE*))
  (datum-tms-node (referent fact t)))

(defun view-node (d) (datum-lisp-form (tms-node-datum d)))

(defun signed-view-node (d) ;; For creating terms in clauses
  (if (false-node? d) (list :NOT (view-node d))
    (if (true-node? d) (view-node d)
      (error "SIGNED-VIEW-NODE requires knowing label: ~A" d))))

(defun show-datum (datum) (format nil "~A" (datum-lisp-form datum)))
(defun make-node-string (node) (show-datum (tms-node-datum node))) ;; For LTMS 

(defun assumptions-of (fact &optional (*LTRE* *LTRE*))
  (mapcar #'view-node 
	  (assumptions-of-node
	   (datum-tms-node (referent fact t)))))

(defun consequences (fact)
  (unless (known? fact) (return-from consequences nil))
  (show-node-consequences (get-tms-node fact)))

(defun explore (fact) (explore-network (get-tms-node fact)))

;;;; Global interrogatives

(defun show-data (&optional (*LTRE* *LTRE*) (stream *standard-output*)
			    &aux counter)
  (setq counter 0)
  (format stream 
	  "~%~D facts total." (ltre-datum-counter *LTRE*))
  (maphash
   #'(lambda (key dbclass)
       (declare (ignore key))
       (dolist (datum (dbclass-facts dbclass))
	 (incf counter)
	 (format stream "~%~A: ~A" (show-datum datum)
		 (cond ((true-node? (datum-tms-node datum)) "TRUE")
		       ((false-node? (datum-tms-node datum)) "FALSE")
		       (t "UNKNOWN")))))
   (ltre-dbclass-table *LTRE*))
  counter)

(defun get-datum (num &optional (*LTRE* *LTRE*))
  (maphash #'(lambda (key dbclass) 
	       (declare (ignore key))
	       (dolist (datum (dbclass-facts dbclass))
		       (when (= (datum-counter datum) num)
			     (return-from GET-DATUM datum))))
	   (ltre-dbclass-table *LTRE*)))

(defun get-clause (num &optional (*LTRE* *LTRE*))
  (dolist (clause (ltms-clauses (ltre-ltms *LTRE*)))
    (when (= (clause-index clause) num) (return-from GET-clause clause))))

(defun fetch-global (pattern &optional (status nil) (*ltre* *ltre*)
			     &aux results)
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
	       (if (and (case status
			      (:TRUE (true-node? (datum-tms-node datum)))
			      (:FALSE (false-node? (datum-tms-node datum)))
			      (:KNOWN (known-node? (datum-tms-node datum)))
			      (:UNKNOWN (not (known-node? (datum-tms-node datum))))
			      (t t))
			(not (eq (unify (datum-lisp-form datum) pattern) :FAIL)))
		   (push (datum-lisp-form datum) results)))))
  results)