;; -*- Mode: Lisp; -*- 

;;;; Influence resolution for TGizmo
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991, Kenneth D. Forbus, Northwestern University
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun resolve-influences (&optional (*tgizmo* *tgizmo*)
				     &aux unknowns)
  (debugging-tgizmo :IR-GLOBAL
    "~% Resolving influences on ~A" (tgizmo-title *tgizmo*))
  (setup-IR)
  (dolist (q (tgizmo-influence-order *tgizmo*))
   (debugging-tgizmo :IR-DETAILS "~%   Resolving ~A.." q)
   (unless (tg-true? `(Resolved ,q))
    (multiple-value-bind (okay? informant reasons)
			 (resolve-influences-on q)
     (when-debugging-tgizmo :IR-DETAILS
	   (if okay? (debugging-tgizmo "~%     Resolved ~A=~D."
				       (Ds-string q) informant)
	     (debugging-tgizmo "~%     ~A unresolved, ~A:~%        ~A."
			       (Ds-string q) informant reasons)))
     (unless okay? (push q unknowns)))))
  unknowns)

;;;; Setting up for influence resolution

(defun setup-IR (&optional (*tgizmo* *tgizmo*)
		   &aux cwas)
  (debugging-tgizmo :IR-DETAILS
    "~%   Making CWA's on influences for ~A.." 
    (tgizmo-title *tgizmo*))
  (tg-run-rules)
  ;; Start by closing all the influence sets
  (with-LTRE (tgizmo-LTRE *tgizmo*)
   (dolist (q (tgizmo-quantities *tgizmo*))
	   (multiple-value-bind (m-ignore cwa work?)
             (close-set-if-needed `(DIs ,q))
	     (when work? (push cwa cwas)))
   	   (multiple-value-bind (m-ignore cwa work?)
             (close-set-if-needed `(IIs ,q))
             (when work? (push cwa cwas)))))
  (tg-run-rules)
  (debugging-tgizmo :IR-DETAILS "~%    IR CWA's done for ~A."
		    (tgizmo-title *tgizmo*))
  (when-debugging-tgizmo
   :CWAS (cond (cwas (format t "~% IR CWAs are:")
		     (dolist (cwa cwas) (pprint cwa)))
	       (t (format t "~% No new CWAs needed for IR."))))
  (when cwas (find-influence-ordering)))

;;;; Contradiction handling

(defun IR-CWA-contradiction-handler (contradictions ltms)
  ;; Retracts all invalid CWA's for DIs and IIs implicated in
  ;; each contradiction.  If all cleared, proceed with computation
  (debugging-tgizmo :IR-DETAILS "~%      IR-CWA: Active Contradictions = ~A."
		    contradictions)
  (dolist (contradiction contradictions)
   (when (violated-clause? contradiction)
    (dolist (asn (assumptions-of-clause contradiction))
      (let ((form (view-node asn)))
	(when (and (cwa-form? form)
		   (listp (car form))
		   (member (caar form) '(DIs IIs))
		   (CWA-invalid? form))
          (debugging-tgizmo :IR-DETAILS
			    "~%      IR-CWA: Retracting ~A."
			    form)
	  (retract-CWA form))))))
  (when-debugging-tgizmo :IR-DETAILS
    (format t
      "~%      IR-CWA: Contradictions still active: ~A."
      (remove-if-not #'(lambda (c) (violated-clause? c))
		     contradictions)))
  (not (some #'(lambda (c) (violated-clause? c))
	     contradictions)))

(defun retract-IR-CWAs
  (&optional (quantities (tgizmo-quantities *tgizmo*))
	     (*tgizmo* *tgizmo*))
  (dolist (q quantities)
   (retract-CWAs `(DIs ,q))
   (retract-CWAs `(IIs ,q))))

(defun resolve-influences-on (q &optional (*tgizmo* *tgizmo*))
  (cond ((tg-true? `(Directly-influenced ,q))
	 (debugging-tgizmo :IR-DETAILS "<direct>")
	 (resolve-dis-on q))
	((tg-true? `(Indirectly-influenced ,q)) 
	 (debugging-tgizmo :IR-DETAILS "<indirect>") 
	 (resolve-iis-on q))
	(t (debugging-tgizmo :IR-DETAILS "<quiet>")
	   (values t :=))))

(defun direct-influences-on (q)
  (car (tg-fetch `((DIs ,q) members ?m) :TRUE)))

(defun indirect-influences-on (q)
  (car (tg-fetch `((IIs ,q) members ?m) :TRUE)))

;;;; Finding the causal ordering for influences

(defun find-influence-ordering (&optional (*tgizmo* *tgizmo*)
					  &aux directs indirects
					  table)
  (dolist (q (tgizmo-quantities *tgizmo*))
   ;;Ignore uninfluenced parameters
   (cond ((tg-true? `(Directly-influenced ,q))
	  (push q directs)
	  (push (list q 0) table))
	 ((tg-true? `(Indirectly-influenced ,q))
	  (push q indirects)
	  (push (nconc (list q -1)
		       (mapcar #'third
				(third (indirect-influences-on q))))
		table))))
  (update-influence-table-orderings table)
;;; ******* Debugging
  (setq *table* (copy-list table))
  (setf (tgizmo-influence-order *tgizmo*)
	(mapcar #'car (sort table #'(lambda (x y)
				      (< (cadr x) (cadr y)))))))

(defun update-influence-table-orderings (table)
  ;; Suboptimal.  Easy to do better.
  (do ((okay? nil)
       (max -2) (oentry nil))
      (okay? table)
      (setq okay? t)
      (dolist (entry table)
	      (when (cddr entry)
		    ;; For those with constrainers,
		    (setq max -2)
		    (dolist (other (cddr entry))
			    (setq oentry (assoc other table
						:TEST #'equal))
			    (when oentry
				  (if (> (cadr oentry) max)
				      (setq max (cadr oentry)))))
		    (when (> (1+ max) (cadr entry))
			  (setf (cadr entry) (1+ max))
			  (setq okay? nil))))))

;;;; Resolving direct influences

;; Either returns T <sign of result> or
;;                NIL <informant> <reason>
;;  :UNKNOWNS -> Some Ds values needed as inputs weren't known.
;;  :LTE, :GTE -> All inputs were <= zero or >= zero, so more
;;                comparison information is needed. 

(defun resolve-dis-on (q &aux n entry dis lt lte eq gte gt unk)
  ;; First sort influencers by Ds values
  (setq dis (direct-influences-on q))
  (unless dis (error "Direct influences for ~A not closed." q))
  (dolist (inf (third dis))
	  (case (comparison? (setq n `(A ,(third inf))) 'ZERO)
		(:= (setq entry (cons (third inf) (eq-forms n 'zero)))
		    (push entry eq))
		(:< (setq entry (cons (third inf) (lt-forms n 'zero)))
		    (if (eq (car inf) 'I+) (push entry lt)
		      (push entry gt)))
		(:> (setq entry (cons (third inf) (gt-forms n 'zero)))
		    (if (eq (car inf) 'I+) (push entry gt)
		      (push entry lt)))
		(:<= (setq entry (cons (third inf) (lte-forms n 'zero)))
		     (if (eq (car inf) 'I+) (push entry lte)
		      (push entry gte)))
		(:>= (setq entry (cons (third inf) (gte-forms n 'zero)))
		     (if (eq (car inf) 'I+) (push entry gte)
		       (push entry lte)))
		(t (push (third inf) unk))))
  ;; Punt if some unknown
  (when unk (return-from RESOLVE-DIS-ON (values nil :UNKNOWNS unk)))
  (analyze-dis-on q lt lte eq gte gt (list dis)))

;;; Analyzing direct influences

(defun analyze-dis-on (q lt lte eq gte gt antes &aux cs?)
  (cond ((and (null lt) (null lte) ;; Clearly Ds[q]=0
	      (null gt) (null gte))
	 (justify-ir-result q nil nil eq gte gt 0 t antes)
	 (values t 0))
	((and (null lt) (null gt) (null gte)) ;; Could be 0 or -1
	 ;; Avoid redundant justifications
	 (unless (tg-false-forms? (gt-forms `(D ,q) 'ZERO))
		 (exclude-ir-result q lt lte eq gte gt 1 t antes))
	 (values nil :LTE lte))
	((and (null lt) (null gt) (null lte)) ;; Could be 0 or 1
	 (unless (tg-false-forms? (lt-forms `(D ,q) 'ZERO))
		 (exclude-ir-result q lt lte eq gte gt -1 t antes))
	 (values nil :GTE gte))
	((and (null lt) (null lte)) ;; Must be 1
	 (justify-ir-result q nil nil eq gte gt 1 t antes)
	 (values t 1))
	((and (null gt) (null gte)) ;; Must be -1
	 (justify-ir-result q lt lte eq nil nil -1 t antes)
	 (values t -1))
	(t ;; Try for cancellation
	 (multiple-value-setq (gt lt cs?) (cancel-via-identity gt lt cs?))
	 (multiple-value-setq (gt lte cs?) (cancel-via-identity gt lte cs?))
	 (multiple-value-setq (gte lt cs?) (cancel-via-identity gte lt cs?))
	 (multiple-value-setq (gte lte cs?) (cancel-via-identity gte lte cs?))
	 (multiple-value-setq (gt lt antes cs?) (cancel-via-= gt lt antes cs?))
	 (multiple-value-setq (gt lte antes cs?) (cancel-via-= gt lte antes cs?))
	 (multiple-value-setq (gte lt antes cs?) (cancel-via-= gte lt antes cs?))
	 (multiple-value-setq (gte lte antes cs?) (cancel-via-= gte lte antes cs?))
	 (cond (cs? ;; Got a cancellation
		(analyze-dis-on q lt lte eq gte gt antes))
	       (t (justify-ir-ambig q lt lte eq gte gt t antes))))))

;;; Cancellation of direct influences

(defun cancel-via-identity (gts lts cs?)
  (do ((gs gts (cdr gs))
       (cancelled? cs?)
       (match nil))
      ((null gs)
       (values (delete nil gts)
	       (delete nil lts)
	       cancelled?))
      (do ((ls lts (cdr ls)))
	  ((null ls))
	  (when (car ls)
		(when (equal (caar gs) (caar ls))
		      (setq cancelled? t)
		      (setf (car gs) nil)
		      (setf (car ls) nil))))))

(defun cancel-via-= (gts lts antes cs?)
  (do ((gs gts (cdr gs))
       (cancelled? cs?)
       (match nil))
      ((null gs)
       (values (delete nil gts)
	       (delete nil lts)
	       antes
	       cancelled?))
      (do ((ls lts (cdr ls)))
	  ((null ls))
	  (when (car ls)
		(when (equal-to? (caar gs) (caar ls))
		      (setq antes (nconc (eq-forms (caar gs) (caar ls))
					 antes)
			    cancelled? t)
		      (setf (car gs) nil)
		      (setf (car ls) nil))))))

;;;; Resolving indirect influences 

(defun resolve-IIs-on (q &aux n entry iis lt lte eq gte gt unk)
  ;; First sort influencers by Ds values
  (setq iis (indirect-influences-on q))
  (unless iis (error "Indirect influences for ~A not closed." q))
  (dolist (inf (third iis))
	  (case (comparison? (setq n `(D ,(third inf))) 'ZERO)
		(:= (setq entry (cons n (eq-forms n 'ZERO)))
		    (push entry eq))
		(:< (setq entry (cons n (lt-forms n 'ZERO)))
		    (if (eq (car inf) 'qprop) (push entry lt)
		      (push entry gt)))
		(:> (setq entry (cons n (gt-forms n 'ZERO)))
		    (if (eq (car inf) 'qprop) (push entry gt)
		      (push entry lt)))
		(:<= (setq entry (cons n (lte-forms n 'ZERO)))
		     (if (eq (car inf) 'qprop) (push entry lte)
		       (push entry gte)))
		(:>= (setq entry (cons n (gte-forms n 'ZERO)))
		     (if (eq (car inf) 'qprop) (push entry gte)
		       (push entry lte)))
		(t (push (third inf) unk))))
  ;; Punt if some unknown
  (when unk (return-from RESOLVE-IIS-ON (values nil :UNKNOWNS unk)))
  (analyze-iis-on q lt lte eq gte gt (list iis)))

(defun analyze-iis-on (q lt lte eq gte gt antes &aux cs?)
  (cond ((and (null lt) (null lte) ;; Clearly Ds[q]=0
	      (null gt) (null gte))
	 (justify-ir-result q nil nil eq gte gt 0 nil antes)
	 (values t 0))
	((and (null lt) (null gt) (null gte)) ;; Could be 0 or -1
	 ;; Avoid redundant justifications
	 (unless (tg-false-forms? (gt-forms `(D ,q) 'ZERO))
		 (exclude-ir-result q lt lte eq gte gt 1 nil antes))
	 (values nil :LTE lte))
	((and (null lt) (null gt) (null lte)) ;; Could be 0 or 1
	 (unless (tg-false-forms? (lt-forms `(D ,q) 'ZERO))
		 (exclude-ir-result q lt lte eq gte gt -1 nil antes))
	 (values nil :GTE gte))
	((and (null lt) (null lte)) ;; Must be 1
	 (justify-ir-result q nil nil eq gte gt 1 nil antes)
	 (values t 1))
	((and (null gt) (null gte)) ;; Must be -1
	 (justify-ir-result q lt lte eq nil nil -1 nil antes)
	 (values t -1))
	(t ;; Since we don't know the form of the function
	 ;; implicit in qprop, qprop-, no way to resolve
	 ;; conflicting influences without additional knowledge. 
	 (justify-ir-ambig q lt lte eq gte gt nil antes))))
  
;;;; Installing IR results

(defun ir-antecedents (antes lt lte eq gte gt &aux n)
  `(:AND ,@ antes
	 ,@ (mapcan #'cdr lt)
	 ,@ (mapcan #'cdr lte)
	 ,@ (mapcan #'cdr eq)
	 ,@ (mapcan #'cdr gte)
	 ,@ (mapcan #'cdr gt)))

(defun justify-ir-ambig (q lt lte eq gte gt direct? antes)
  (assert! `(:IMPLIES ,(ir-antecedents antes lt lte eq gte gt)
		     (Unresolved ,q))
	   (if direct? :AMBIGUOUS-DIRECT-INFLUENCES
	     :AMBIGUOUS-INDIRECT-INFLUENCES)))

(defun justify-ir-result (q lt lte eq gte gt val direct? antes)
  (assert! `(:IMPLIES ,(ir-antecedents antes lt lte eq gte gt)
		      ,(case val
			     (-1 `(:AND (Resolved ,q) ,@ (lt-forms `(D ,q) 'ZERO)))
			     (0 `(:AND (Resolved ,q) ,@ (eq-forms `(D ,q) 'ZERO)))
			     (1 `(:AND (Resolved ,q) ,@ (gt-forms `(D ,q) 'ZERO)))
			     (t (error
			 "IR result on ~A must be one of -1, 0, 1: ~A" q val))))
	   (if direct? :DIRECT-IR-RESULT :INDIRECT-IR-RESULT)))

(defun exclude-ir-result (q lt lte eq gte gt val direct? antes)
  (assert! `(:IMPLIES ,(ir-antecedents antes lt lte eq gte gt)
		      ,(case val
			    (-1 `(:NOT (:AND ,@ (lt-forms `(D ,q) 'ZERO))))
			    (0 `(:NOT (:AND ,@ (eq-forms `(D ,q) 'ZERO))))
			    (1 `(:NOT (:AND ,@ (gt-forms `(D ,q) 'ZERO))))
			    (t (error
			"IR result on ~A must be one of -1, 0, 1: ~A" q val))))
	   (if direct? :DIRECT-IR-EXCLUSION :INDIRECT-IR-EXCLUSION)))

;;;; Searching for unknown Ds values

(defun resolve-completely (thunk &optional (*tgizmo* *tgizmo*))
  ;; Executes thunk for each consistent assignment of Ds values
  (let ((unknowns (nreverse (resolve-influences))))
    (cond (unknowns ;; Need to search
	   (debugging-tgizmo :IR-DDS "~%   IR DDS: Unknown parameters:~%   ~A"
			     unknowns)
	   (With-LTRE (tgizmo-LTRE *tgizmo*)
            (DD-Search (make-Ds-choice-sets unknowns) 
		       `(progn (resolve-influences)
			       (when-debugging-tgizmo :IR-DDS
				 (format t "~%   IR DDS Results:")
				 (show-ds-values *tgizmo* ',unknowns))
			       ,thunk))))
	  (t (debugging-tgizmo :IR-DDS "~%   IR DDS: All Ds values known.")
	     (eval thunk)))))

(defun make-Ds-choice-sets (unks)
  (mapcar #'(lambda (q)
	      `((> (D ,q) ZERO)
		(= (D ,q) ZERO)
		(< (D ,q) ZERO))) unks))

;;;; Interrogatives

(defun show-IR-CWAs (&optional (*tgizmo* *tgizmo*) &aux set)
  (dolist (q (tgizmo-quantities *tgizmo*))
	  (setq set (direct-influences-on q))
	  (when set
		(format t "~% DIs[~A]={~A}."
			q (third set)))
	  (setq set (indirect-influences-on q))
	  (when set
		(format t "~% IIs[~A]={~A}."
			q (third set)))))

(defun show-ds-values (&optional (*tgizmo* *tgizmo*)
				 (qlist :NADA))
  (if (eq qlist :NADA)
      (setq qlist (tgizmo-quantities *tgizmo*)))
  (dolist (q qlist)
	  (format t "~%   ~A"
		  (Ds-value-string q
				   (rel-value `(D ,q) 'ZERO)))))
