;; -*- Mode: Lisp; -*-

;;;; Modeling language for TGizmo
;;; Last Edited: 11/7/91, by KDF

;;; Copyright (c) 1991, Kenneth D. Forbus, Northwestern University,
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun keywordize (stuff)
  (cond ((null stuff) (error "Can't keywordize nothing."))
	((listp stuff) (keywordize (car stuff)))
	(t (intern (format nil "~A" stuff) 'keyword))))

(defmacro defrule (name triggers &rest consequences)
  `(rule , (mapcar #'(lambda (trigger)
		       `(:INTERN ,trigger)) triggers)
	 (rassert! (:IMPLIES (:AND ,@ triggers)
			    (:AND ,@ consequences))
		  ,(keywordize name))))

(defmacro defPredicate (form &rest consequences)
  `(rule ((:INTERN ,form))
    (rlet ((?self ,form))
	  ,@ (translate-relations consequences :DEFPREDICATE
				  form (keywordize form)))))

(defmacro defentity (form &rest consequences)
  `(rule ((:INTERN ,form))
    (rlet ((?self ,form))
	  (rassert! (:IMPLIES ,form (Exists ,(cadr form)))
		    :DEFENTITY)
	  ,@ (translate-relations consequences :DEFENTITY
				  form (keywordize form)))))

(defmacro defview (form &rest stuff)
  (multiple-value-bind (ispec pcs qcs rels infs)
		       (parse-vp form stuff nil)
		       (debugging-tgizmo :DOMAIN-THEORY
					 "~% Defining view ~A.." form)
		       (make-vp-rules form ispec pcs qcs rels infs nil)))

(defmacro defprocess (form &rest stuff)
  (multiple-value-bind (ispec pcs qcs rels infs)
		       (parse-vp form stuff t)
		       (debugging-tgizmo :DOMAIN-THEORY
					 "~% Defining process ~A.." form)
		       (make-vp-rules form ispec pcs qcs rels infs t)))

;;;; Working with views and processes

(defun parse-vp (form stuff process?)
  ;; Does some extra syntactic checks
  (let ((ispec (cadr (member :INDIVIDUALS stuff)))
	(pcs (cadr (member :PRECONDITIONS stuff)))
	(qcs (cadr (member :QUANTITY-CONDITIONS stuff)))
	(rels (cadr (member :RELATIONS stuff)))
	(infs (cadr (member :INFLUENCES stuff))))
    (unless ispec (error "~A must have :INDIVIDUALS field: ~A"
			 (if process? "defprocess" "defview")
			 form))
    (unless (or pcs qcs)
	    (error "~A must have :PRECONDITIONS or :QUANTITY-CONDITIONS: ~A"
		   (if process? "defprocess" "defview") form))
    (cond (process?
	   (unless infs (error "Physical processes must have influences: ~A" form)))
	  (infs (error "Views cannot have influences: ~A" form)))
    ;;; Make sure no dangling variables
    (let ((*bound-vars* (cons '?self (pattern-free-variables ispec)))
	  (floating nil))
      (when (setq floating (pattern-free-variables pcs))
	    (error "Can't have free variable(s) ~A in preconditions: ~A"
		   floating form))
      (when (setq floating (pattern-free-variables qcs))
	    (error "Can't have free variable(s) ~A in quantity conditions: ~A"
		   floating form))
      (when (setq floating (pattern-free-variables rels))
	    (error "Can't have free variable(s) ~A in relations: ~A"
		   floating form))
      (if process? 
	  (when (setq floating (pattern-free-variables infs))
	    (error "Can't have free variable(s) ~A in influences : ~A"
		   floating form))))
    (values ispec pcs qcs rels infs)))

;;;; Finding and instantiating views and processes

(defun make-vp-rules (form ispec pcs qcs rels infs process?)
  (let ((antes (apply #'append 
		      (mapcar #'cdr ispec)))
	(is (mapcar #'car ispec)))
    `(rule ,(mapcar #'(lambda (ante)
			`(:INTERN ,ante))
		    antes)
	   (rlet ((?self ,form))
		 (debugging-tgizmo :MODELING "~% Found ~A: ~A."
				   ,(if process? "process" "view")
				   ?self)
	   ;; The ispecs imply the process instance
	   (rassert! (:IMPLIES (:AND ,@ antes)
			       (,(if process? 'Process-Instance
				   'View-Instance) ,form))
		    :CDI-IMPLIED)
	   ;; The existence of the individuals implies
	   ;; the existence of the process.
	   ,@ (when process? 
		 `((rassert! (:IMPLIES (:AND ,@ (mapcar #'(lambda (i)
							    `(Exists ,i)) is))
				       (Exists ,form))
			     :PROCESS-EXISTENCE)
		   (rassert! (:IMPLIES (Active ,form) (Exists ,form)) :NO-GHOSTS)))
	   ;; Active iff pc's and qc's hold
	   (rassert! (:IFF (Active ,form)
			   (:AND ,@ pcs
				 ,@ qcs))
		    :CDI-ACTIVE-CONSTRAINT)
	   ;; If active, the relations hold
	   ,@ (when rels
		    (translate-relations rels (if process? :PROCESS :VIEW)
					 '(Active ?self) (keywordize form)))
	   ;; If active process, influences hold
	   ,@ (when infs
		    (translate-relations infs (if process? :PROCESS :VIEW)
					 '(Active ?self) (keywordize form)))))))

;;;; Parsing contents of relations fields

;;; In an ``industrial-grade'' QP theory implementation,
;;;  there is typically alot more hair here.  We'll do
;;;  the minimum.

(defun translate-relations (conseqs context antes informant)
  (let ((explicit nil)
	(implicit nil))
    (dolist (c conseqs)
     (multiple-value-bind (e i)
	(translate-relation c context antes informant)
	(setq explicit (nconc e explicit))
	(setq implicit (nconc i implicit))))
    `(,@ (when explicit `((rassert! (:IMPLIES ,antes (:AND ,@ explicit))
				    ,informant)))
      ,@ implicit)))

(defun translate-relation (form context antes informant)
  (cond ((not (listp form)) (values (list form) nil))
	(t (case (car form)
		 ;; ONLY-DURING indicates that form holds exactly when cdi does.
		 (ONLY-DURING
		  (values nil
			  `((rassert! (:IFF ,antes ,(cadr form)) ,informant))))
		 ;; Quantities local to a cdi only exist when it is active.
		 (QUANTITY (if (member context '(:PROCESS :VIEW))
			       (values nil
				       `((rassert! (:IFF ,antes ,form) ,informant)))
			     (values (list form) nil)))
		 ((I+ I-) (unless (eq context :PROCESS)
				  (error "Can't have direct influence in ~A: ~A"
					 context antes))
		  (values nil `((rassert! (:IFF ,antes ,(append form (list '?SELF))
						,informant)))))
		 ((Qprop Qprop-)
		  (values nil `((rassert! (:IFF ,antes ,(append form (list '?SELF)))
					  ,informant))))
		 (t (values (list form) nil))))))
