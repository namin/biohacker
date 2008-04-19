;; -*- Mode: Lisp; -*- 

;;;; TGizmo state recorder
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991, Kenneth D. Forbus, Northwestern University,
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Takes a snapshot of the current LTMS database to allow
;; a previously examined state to be regenerated and for
;; easy comparison of states. 

(defun snapshot (title &optional (*tgizmo* *tgizmo*))
  (let ((st (make-state :TITLE title
			:INDIVIDUALS
			(mapcan #'make-signed-form
				(tg-fetch '(Exists ?x)))
			:VIEW-STRUCTURE
			(mapcan #'(lambda (vform)
				    (make-signed-form `(Active ,(cadr vform))))
				(tg-fetch '(View-Instance ?x)))
			:PROCESS-STRUCTURE 
			(mapcan #'(lambda (vform)
				    (make-signed-form `(Active ,(cadr vform))))
				(tg-fetch '(Process-Instance ?x))))))
    (dolist (comp (tgizmo-comparisons *tgizmo*))
	    (cond ((and (listp (car comp))
			(eq (caar comp) 'D)
			(eq (cdr comp) 'ZERO)) ;; A Ds value
		   (let ((rel (rel-value (car comp) (cdr comp))))
		     (unless (member rel '(:BT :??))
			     (push `(,(unkeywordize rel) ,(car comp) ,(cdr comp))
				   (state-Ds-values st)))))
		  (t ;; Just a random old inequality
		   (let ((rel (rel-value (car comp) (cdr comp))))
		     (unless (member rel '(:BT :??))
			     (push `(,(unkeywordize rel) ,(car comp) ,(cdr comp))
				   (state-comparisons st)))))))
    st))

(defun make-signed-form (form)
  (case (label-of form)
	(:TRUE (list form))
	(:FALSE (list `(:NOT ,form)))
	(t nil)))

(defun unkeywordize (symbol) (intern (symbol-name symbol) 'user))

;;;; Showing cached states

(defun get-state (num &optional (*tgizmo* *tgizmo*))
  (dolist (state (tgizmo-states *tgizmo*))
	  (when (= (state-title state) num)
		(return-from get-state state))))

(defun show-state (state &key (psvs-inactive? nil)
			 (ds-values? nil)
			 (comparisons? nil)(all? nil)
			 (stream *standard-output*))
  (format stream "~%In state ~A:" (state-title state))
  (let ((actives nil)(inactives nil))
    (dolist (pia (state-process-structure state))
	    (if (eq (car pia) :NOT) (push (cadr (cadr pia)) inactives)
	      (push (cadr pia) actives)))
    (cond (actives (format stream "~%  Active processes:")
		   (dolist (a actives)
			   (format stream "~%    ~A" a)))
	  (t (format stream "~%  No known active processes.")))
    (when (or psvs-inactive? all?)
	  (cond (inactives (format stream "~%  Inactive processes:")
			   (dolist (a inactives)
				   (format stream "~%     ~A" a)))
		(t (format stream "~%  No known inactive processes.")))))
  (let ((actives nil)(inactives nil))
    (dolist (pia (state-view-structure state))
	    (if (eq (car pia) :NOT) (push (cadr (cadr pia)) inactives)
	      (push (cadr pia) actives)))
    (cond (actives (format stream "~%  Active views:")
		   (dolist (a actives)
			   (format stream "~%    ~A" a)))
	  (t (format stream "~%  No active views.")))
        (when (or psvs-inactive? all?)
	  (cond (inactives (format stream "~%  Inactive views:")
			   (dolist (a inactives)
				   (format stream "~%     ~A" a)))
		(t (format stream "~%  No known inactive views.")))))
  
  (when (or comparisons? all?)
	(format stream "~%  Known comparisons:")
	(dolist (comp (state-comparisons state))
		(format stream "~%    ~A"
			(apply #'ineq-string comp))))
  (when (or ds-values? all?)
	(format stream "~%  Known Ds values:")
	(dolist (comp (state-Ds-values state))
		(format stream "~%    ~A"
			(Ds-value-string (cadr (cadr comp)) (car comp)))))
  state)

;;;; Report generator for TGIZMO results

(defun report-states (file &optional (*tgizmo* *tgizmo*))
  (with-open-file (fout file :DIRECTION :OUTPUT)
   (format fout "~%TGizmo Report, ~A" (tgizmo-title *tgizmo*))
   (format fout "~% Scenario = ~A~% Measurements = "
	   (tgizmo-scenario *tgizmo*))
   (pprint (tgizmo-measurements *tgizmo*) fout)
   (format fout "~% Run under ~A, ~A,~%  on ~A, a ~A (~A)."
	   (lisp-implementation-type) (lisp-implementation-version)
	   (machine-instance)(machine-type)(machine-version))
   (multiple-value-bind (second minute hour date month year)
			(get-decoded-time)
    (format fout "~% Dumped ~A:~A:~A, ~A/~A/~A"
	    hour minute second month date year))
    (dolist (state (reverse (tgizmo-states *tgizmo*)))
	    (format fout "~|")
	    (show-state state :ALL? t :STREAM fout)
	    (format fout "~% ================== ~%"))))

;;;; Sorting states

(defun make-state-index (&optional (*tgizmo* *tgizmo*))
  (let ((top-index (classify-by-field (tgizmo-states *tgizmo*)
				      (lambda (s) (state-individuals s)))))
    (dolist (ientry top-index top-index)
	    (let ((vs-index (classify-by-field (cdr ientry)
					       (lambda (s) (state-view-structure s)))))
	      (dolist (vs-entry vs-index)
		      (setf (cdr vs-entry) 
			    (classify-by-field (cdr vs-entry)
					       (lambda (s) (state-process-structure s)))))
	      (setf (cdr ientry) vs-index)))))

(defun classify-by-field (state-list field &aux index entry)
  (dolist (state state-list index)
   (setq entry (assoc state index
		      :TEST #'(lambda (x y)
				(same-elements? (funcall field x) y))))
   (unless entry (push (setq entry (list (funcall field state))) index))
   (push state (cdr entry))))

(defun summarize-state-index (index &optional (stream *standard-output*))
  (dolist (ientry index)
   (dolist (i (car ientry))
    (format stream "~% ~A" i))
   (dolist (ventry (cdr ientry))
    (dolist (vi (car ventry))
     (format stream "~%   ~A" vi))
    (dolist (pentry (cdr ventry))
     (dolist (pri (car pentry))
      (format stream "~%     ~A" pri))
     (format stream "~%       ~D states." (length (cdr pentry)))))))

;;;; Comparing states

(defun same-state? (s1 s2)
  (and (same-elements? (state-individuals s1)
		       (state-individuals s2))
       (same-elements? (state-view-structure s1)
		       (state-view-structure s2))
       (same-elements? (state-process-structure s1)
		       (state-process-structure s2))
       (same-elements? (state-ds-values s1)
		       (state-ds-values s2))
       (same-elements? (state-comparisons s1)
		       (state-comparisons s2))))

(defun find-corresponding-states (tg1 tg2 &aux result)
  (dolist (s1 (tgizmo-states tg1) result)
	  (dolist (s2 (tgizmo-states tg2))
		  (when (same-state? s1 s2)
			(push (cons s1 s2) result)
			(return t)))))

(defun summarize-Ds-differences (state-list
				 &optional (stream *standard-output*))
  (multiple-value-bind (d-list common)
   (subtract-commonalities 
    (mapcar #'(lambda (s) (state-ds-values s)) state-list))
   (format stream "~% Common Ds values:")
   (if common (dolist (comp common)
	       (format stream "~%   ~A" 
		       (ds-value-string (cadr (cadr comp)) (car comp))))
     (format stream " None."))
   (do ((states state-list (cdr states))
	(diffs d-list (cdr diffs)))
       ((null states) d-list)
       (format stream "~%   For ~A:" (car states))
       (dolist (comp (car diffs))
	(format stream "~%     ~A" 
		(ds-value-string (cadr (cadr comp)) (car comp)))))))

(defun subtract-commonalities (list-of-sets &key (test #'equal) &aux int)
  (setq int (car list-of-sets))
  (dolist (set (cdr list-of-sets))
	  (setq int (intersection int set :TEST test)))
  (values (mapcar #'(lambda (set)
		      (set-difference set int :TEST #'equal)) list-of-sets)
	  int))