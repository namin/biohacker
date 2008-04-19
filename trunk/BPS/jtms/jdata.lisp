;; -*- Mode: Lisp; -*-

;;;; Database for Tiny Rule Engine using JTMS
;;;; Last Edited 7/1/92, by KDF

;; Copyright (c) 1989, 1990, 1991 Kenneth D. Forbus, Northwestern University,
;; and Johan de Kleer and the Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;;; Database structure and contents

(defstruct (dbclass (:PRINT-FUNCTION jtre-dbclass-printer))
  name    ; Corresponding symbol
  jtre    ; JTRE it is part of.
  facts   ; Associated facts
  rules)  ; Associated rules

 (defun jtre-dbclass-printer (r st ignore)
   (declare (ignore ignore))
   (format st "<Dbclass ~A>" (dbclass-name r)))

(defstruct (datum (:PRINT-FUNCTION jtre-datum-printer))
  id                   ; Unique ID for easy lookup
  lisp-form            ; Expression for pattern-matching
  (tms-node nil)       ; Pointer into TMS
  dbclass              ; Dbclass of the corresponding pattern
  (assumption? nil)    ; if non-nil, indicates informant
  (plist nil))         ; local property list

(defun jtre-datum-printer (d st ignore)
  (declare (ignore ignore))
  (format st "<Datum ~D>" (datum-id d)))

;;;; Making statements

(defun assert! (fact just &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (unless (listp just) (setq just (list just)))
  (debugging-jtre "~%    Asserting ~A via ~A." fact just)
  (justify-node (car just) node
		(mapcar #'(lambda (f) (datum-tms-node (referent f t)))
			(cdr just)))
  datum)

(defmacro rassert! (fact just)
  `(assert! ,(quotize fact) ,(quotize just)))

(defun quiet-assert! (fact just &optional (*JTRE* *JTRE*))
  (without-contradiction-check (jtre-jtms *JTRE*) (assert! fact just)))

(defun assume! (fact reason &optional (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond	((not (datum-assumption? datum))
	 (setf (datum-assumption? datum) reason)
	 (debugging-jtre "~%    Assuming ~A via ~A." fact reason)
	 (assume-node node)
	 (enable-assumption node))
	((eq reason (datum-assumption? datum)))
	(t (error
	    "Fact ~A assumed because of ~A assumed again because of ~A"
	    (show-datum datum) (datum-assumption? datum) reason)))
  datum)

(defun already-assumed? (fact) (datum-assumption? (referent fact t)))

;;;; Retraction

(defun retract! (fact &optional (just 'user) (quiet? nil)
		      (*JTRE* *JTRE*) &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond ((not (tms-node-assumption? node))
	 (unless quiet?
	   (format t "~%~A isn't an assumption."
		   (show-datum datum))))
	((not (in-node? node))
	 (unless quiet?
	   (format T
	     "~%The assumption ~A is not currently in."
	     fact)))
	((eq just (datum-assumption? datum))
	 (debugging-jtre "~%    Retracting ~A via ~A."
			 fact just)
	 (setf (datum-assumption? datum) nil)
	 (retract-assumption node))
	((not quiet?)
	 (format t "~%~A not source of assumption for ~A"
		 just fact)))
  node)

(defmacro rretract! (fact &optional (just 'USER))
  `(retract! ,(quotize fact) ,(quotize just)))

(defun contradiction (fact &optional (*JTRE* *JTRE*))
  (make-contradiction (datum-tms-node (referent fact t))))

;;;; Interface and display of data

(defun in? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(in-node? (datum-tms-node r))))

(defun out? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(out-node? (datum-tms-node r))))
    
(defun why? (fact &optional (*JTRE* *JTRE*) &aux r)
  (when (setq r (referent fact))
	(why-node (datum-tms-node r))))

(defun assumptions-of (fact &optional (*JTRE* *JTRE*))
  (mapcar #'view-node 
	  (assumptions-of-node
	   (datum-tms-node (referent fact *jtre* t)))))

(defun fetch (pattern &optional (*JTRE* *JTRE*) &aux bindings unifiers)
  (dolist (candidate (get-candidates pattern) unifiers)
    (setq bindings (unify pattern (datum-lisp-form candidate)))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

;;;; More display-intensive procedures

(defun wfs (fact &optional (*JTRE* *JTRE*))
  ;; Displays well-founded support for a fact
  (cond ((out? fact) (format t "~% ~A is OUT." fact))
	(t (do ((queue (list (get-tms-node fact))
		       (nconc (cdr queue) new-antes))
		(so-far (list (get-tms-node fact)))
		(new-antes nil nil))
	       ((null queue) (format t "~%--------") fact)
	     (why-node (car queue))
	     (unless (or (out-node? (car queue))
			 (tms-node-assumption? (car queue)))
	       ;; Go down the support
	       (dolist (ante (just-antecedents
			      (tms-node-support (car queue))))
		 (unless (member ante so-far)
		   (push ante so-far)
		   (push ante new-antes))))))))

(defun say-datum-belief (pr &optional (*jtre* *jtre*)
			    (indent ""))
  (format t "~%~A~A: ~A" indent pr
	  (if (in-node? (get-tms-node pr *jtre*))
	      "IN" "OUT")))

(defun show-justifications (fact &optional (*jtre* *jtre*))
  (format t "~% ~A::" fact)
  (let* ((node (get-tms-node fact *jtre*))
	 (justs (tms-node-justs node)))
    (unless justs
	    (format t " No justifications.")
	    (return-from show-justifications node))
    (dolist (j justs)
	    (format t "~% ~A" (just-informant j))
	    (cond ((just-antecedents j) 
		   (format t ", on:")
		   (dolist (ante (just-antecedents j))
			   (say-datum-belief
			    (view-node ante) *jtre* "  "))
		   (format t "."))
		  (t (format t "."))))))

(defun show-data (&optional (*JTRE* *JTRE*)
			    (stream *standard-output*))
  (format stream 
	  "~%~D facts total." (jtre-datum-counter *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
	       (format stream "~%~A: ~A" (show-datum datum)
		       (if (in-node? (datum-tms-node datum))
			   "IN" "OUT"))))))

;;;; Database system

(defun get-dbclass (fact &optional (*JTRE* *JTRE*)
			 &aux dbclass)
  (cond ((null fact) (error "~% NIL can't be a dbclass."))
	((listp fact) (get-dbclass (car fact) *JTRE*))
	((variable? fact)
	 (cond ((boundp fact)
		(get-dbclass (symbol-value fact) *JTRE*))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((setq dbclass
		      (gethash fact
			       (jtre-dbclass-table *JTRE*)))
		dbclass)
	       (t (setq dbclass
			(make-dbclass :NAME fact :FACTS nil
				    :RULES nil :JTRE *JTRE*))
		  (setf (gethash fact
			 (jtre-dbclass-table *JTRE*))
			dbclass)
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

(defun referent (fact &optional (virtual? nil)
		      (*JTRE* *JTRE*))
  (if virtual? (insert fact) (referent1 fact)))

(defun referent1 (fact)
  (dolist (candidate (dbclass-facts (get-dbclass fact)))
	  (when (equal (datum-lisp-form candidate) fact)
		(return candidate))))

(defun insert (fact &aux datum)
  (setq datum (referent1 fact))
  (cond (datum (values datum t))
	(t (setq datum
		 (make-datum
		  :ID (incf (jtre-datum-counter *JTRE*))
		  :LISP-FORM fact
		  :DBCLASS (get-dbclass fact)))
	   (setf (datum-tms-node datum)
		 (tms-create-node (jtre-jtms *JTRE*) datum))
	   (push datum (dbclass-facts (datum-dbclass datum)))
	   (try-rules datum)
	   (values datum nil))))

(defun get-candidates (pattern)
  (dbclass-facts (get-dbclass pattern)))

(defun map-dbclass (proc &optional (*JTRE* *JTRE*))
  (maphash #'(lambda (name dbclass) (declare (ignore name))
	       (funcall proc dbclass))
	   (jtre-dbclass-table *JTRE*)))

(defun get-tms-node (fact &optional (*JTRE* *JTRE*))
  (datum-tms-node (referent fact t)))

(defun view-node (node)
  (datum-lisp-form (tms-node-datum node)))

;;;; More query routines

(defun show-datum (datum)
  (format nil "~A" (datum-lisp-form datum)))

(defun get-datum (num &optional (*JTRE* *JTRE*))
  (map-dbclass
   #'(lambda (dbclass)
       (dolist (datum (dbclass-facts dbclass))
	       (when (= (datum-id datum) num)
		     (return-from GET-DATUM datum))))))

(defun get-just (num &optional (*JTRE* *JTRE*))
  (dolist (just (jtms-justs (jtre-jtms *JTRE*)))
    (when (= (just-index just) num)
	  (return-from GET-just just))))
