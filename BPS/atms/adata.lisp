;;; -*- Mode: Lisp; -*-

;;; ATRE database
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;;  University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun assert! (fact just &optional (*atre* *atre*)
		     &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (unless (listp just) (setq just (list just)))
  (debugging-atre "~%    Asserting ~A via ~A." fact just)
  (justify-node (car just) node
		(mapcar #'(lambda (f)
			    (datum-tms-node (referent f t)))
			(cdr just)))
  datum)

(defun assume! (fact reason &optional (*atre* *atre*)
		     &aux datum node)
  (setq datum (referent fact t)
	node (datum-tms-node datum))
  (cond ((not (datum-assumption? datum))
	 (setf (datum-assumption? datum) reason)
	 (debugging-atre
	  "~%    Assuming ~A via ~A." fact reason)
	 (assume-node node))
	((eq reason (datum-assumption? datum)))
	(t (error
  "Fact ~A assumed because of ~A assumed again because of ~A"
  (show-datum datum) (datum-assumption? datum) reason)))
  datum)

(defun already-assumed? (fact)
  (tms-node-assumption? (get-tms-node fact)))

(defun assume-if-needed (fact reason &optional (*atre* *atre*))
  (unless (already-assumed? fact) (assume! fact reason)))

(defmacro rassert! (fact just)
  `(assert! ,(quotize fact) ,(quotize just)))

(defun contradiction (fact &optional (*atre* *atre*))
  (make-contradiction (datum-tms-node (referent fact t))))

(defmacro rnogood! (informant &rest facts)
  `(assert! 'false ,(quotize (cons informant facts))))

;;;; Database system

(defun get-dbclass (fact &aux dbclass)
  (cond ((null fact) (error "~% NIL can't be a dbclass."))
	((listp fact) (get-dbclass (car fact)))
	((variable? fact)
	 (cond ((boundp fact) (get-dbclass (symbol-value fact)))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((setq dbclass
		      (gethash fact
			       (atre-dbclass-table *atre*)))
		dbclass)
	       (t (setq dbclass
			(make-dbclass :NAME fact :FACTS nil
				    :RULES nil :ATRE *atre*))
		  (setf (gethash fact
				 (atre-dbclass-table *atre*))
			dbclass)
		  (push dbclass (atre-dbclasses *atre*))
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

(defun referent (fact &optional (virtual? nil))
  (if virtual? (insert fact) (referent1 fact)))

(defun referent1 (fact) ;; Could use seperate hash table
  (dolist (candidate (dbclass-facts (get-dbclass fact)))
	  (when (equal (datum-lisp-form candidate) fact)
		(return-from referent1 candidate))))

(defun insert (fact &aux datum)
  (setq datum (referent1 fact))
  (cond (datum (values datum t))
	(t (setq datum (make-datum
			:COUNTER
			(incf (atre-datum-counter *atre*))
			:ATRE *atre*
			:LISP-FORM fact
			:DBCLASS (get-dbclass fact)))
	   (setf (datum-tms-node datum)
		 (tms-create-node (atre-atms *atre*) datum))
	   (push datum (dbclass-facts (datum-dbclass datum)))
	   (try-rules datum)
	   (values datum nil))))

(defun fetch (pattern &optional (*atre* *atre*)
		      &aux bindings unifiers)
  (dolist (candidate (get-candidates pattern) unifiers)
    (setq bindings (unify pattern (datum-lisp-form candidate)))
    (unless (eq bindings :FAIL)
      (push (sublis bindings pattern) unifiers))))

(defun get-candidates (pattern)
  (dbclass-facts (get-dbclass pattern)))

;;;; Interface and display of data

(defun true? (fact &optional (*atre* *atre*) &aux r)
  (when (setq r (referent fact nil))
	(true-node? (datum-tms-node r))))

(defun in? (fact env &optional (*atre* *atre*) &aux r)
  (when (setq r (referent fact nil))
	(in-node? (datum-tms-node r) env)))

(defun out? (fact env &optional (*atre* *atre*) &aux r)
  (when (setq r (referent fact nil))
	(out-node? (datum-tms-node r) env)))

(defun consistent-with? (fact env &optional (*atre* *atre*)
			      &aux r)
  (when (setq r (referent fact nil))
	(node-consistent-with? (datum-tms-node r) env)))

(defun why? (fact &optional (*atre* *atre*)
		  (stream *standard-output*)
		  &aux r)
  (when (setq r (referent fact nil))
	(why-node (datum-tms-node r) stream)))

(defun environment-of (facts &optional (*atre* *atre*)
			     &aux node env)
  (setq env (atms-empty-env (atre-atms *atre*)))
  (dolist (fact facts)
	  (setq node (get-tms-node fact *atre*))
	  (unless (tms-node-assumption? node)
  (error "Non-assumption in ENVIRONMENT-OF: ~A." fact))
	  (setq env (cons-env node env))
	  (when (env-nogood? env)
		(return-from ENVIRONMENT-OF
			     (values nil env))))
  env)

(defun environment-cons (fact env)
  (cons-env (get-tms-node fact) env))    

(defun view-env (env)
  (mapcar #'view-node (env-assumptions env)))

(defun justifications (fact &optional (*atre* *atre*)
			    (stream *standard-output*))
  (node-justifications (get-tms-node fact *atre*) stream))
    
;;; More interrogatives

(defun the-e (num &optional (*atre* *atre*))
  (e (atre-atms *atre*) num))

(defun get-tms-node (fact &optional (*atre* *atre*))
  (datum-tms-node (referent fact t)))

(defun view-node (node)
  (datum-lisp-form (tms-node-datum node)))

(defun stringify-node (node)
  (format nil "~A" (view-node node)))

(defun assumptions-of (fact)
  (tms-node-label (datum-tms-node (referent fact t))))

(defun get-datum (num &optional (*atre* *atre*))
  (maphash #'(lambda (key dbclass)
	       (declare (ignore key))
	       (dolist (datum (dbclass-facts dbclass))
		 (when (= (datum-counter datum) num)
		   (return-from GET-DATUM datum))))
	   (atre-dbclass-table *atre*)))

(defun get-just (num &optional (*atre* *atre*))
  (dolist (just (atms-justs (atre-atms *atre*)))
    (when (= (just-index just) num)
      (return-from GET-just just))))

;;; Extra printing routines

(defun show-datum (datum) (format nil "~A"
				  (datum-lisp-form datum)))
(defun show-data (&optional (*atre* *atre*)
			    (stream *standard-output*)
		       &aux counter)
  (setq counter 0)
  (format stream 
	  "~%~D facts total." (atre-datum-counter *atre*))
  (dolist (dbclass (atre-dbclasses *atre*) counter)
    (dolist (datum (dbclass-facts dbclass))
      (incf counter)
      (format stream "~%~A: ~A" (show-datum datum)
	      (assumptions-of (datum-lisp-form datum)))))
  counter)

(defun show-context (env &optional (*atre* *atre*)
			 (stream *standard-output*)
		       &aux counter)
  (setq counter 0)
  (dolist (dbclass (atre-dbclasses *atre*))
    (dolist (datum (dbclass-facts dbclass))
      (when (in-node? (datum-tms-node datum) env)
	(incf counter)
	(format stream "~%~A" (show-datum datum)))))
  (format stream  "~%~D facts total." counter)
  counter)

(defun show-dbclasses (&optional (*atre* *atre*)
				 (stream *standard-output*)
				&aux counter)
  ;; Handy for finding buggy assertions
  (setq counter 0)
  (dolist (dbclass (atre-dbclasses *atre*) counter)
    (incf counter)
    (format stream "~% ~A: ~D facts, ~D rules"
	    (dbclass-name dbclass)
	    (length (dbclass-facts dbclass))
	    (length (dbclass-rules dbclass)))))


