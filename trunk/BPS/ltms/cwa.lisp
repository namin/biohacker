;;; -*- Mode: Lisp; Syntax: Common-lisp -*-

;;;; Closed-World Assumptions -- New version
;;;;  Modified: forbus on 4/27/95

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;;; Interface to set mechanism

(defun set-members (set-name &optional (*LTRE* *LTRE*)
			     &aux m-s)
  (dolist (mform (fetch `(,set-name MEMBERS ?elements)))
	  (if (true? mform) (return (setq m-s mform))))
  (cond (m-s (values (third m-s) (find-cwa-for-set m-s)))
	(t nil)))

(defun close-set-if-needed (set-name
			    &optional (*LTRE* *LTRE*))
  (multiple-value-bind (construal cwa)
		       (set-members set-name)
  (cond (cwa (values construal cwa nil))
	(t (close-set set-name)))))

(defun close-set (set-name &optional (*LTRE* *LTRE*))
   ;; UNWIND-PROTECT added for safety, otherwise a bogus CWA
   ;; could persist past a contradiction
   (let ((non-contradictory? nil))
      (unwind-protect
           (multiple-value-bind (known-members known-not)
               (get-set-information set-name)
              (let ((cwa-form (make-cwa-form set-name known-members))
                    (members-form
                     `(,set-name MEMBERS ,known-members)))
                 (retract-CWAs set-name)
                 (assume-cwa-if-needed cwa-form)
                 (justify-cwa-if-needed set-name known-members known-not
                  cwa-form members-form)
                 (setq non-contradictory? t)
                 (values known-members cwa-form t)))
         (progn 
            (unless non-contradictory?
               (retract-CWAs set-name))))))

(defmacro With-Closed-Set (set-name &rest body)
  `(multiple-value-bind (members cwa)
       (close-set-if-needed ,set-name *LTRE*)
     (With-Contradiction-Handler (ltre-ltms *LTRE*)
       #'(lambda (clauses ltms)
	   (set-cwa-handler clauses ltms ,set-name 
			    cwa :LOST-CWA))
       (let ((answer (catch cwa ,@ body)))
	 (cond ((eq answer :LOST-CWA) (values nil nil))
	       (t (values t members)))))))

;;;; 2nd-level procedures -- can be called by experts 

(defun find-cwa-for-set (m-s)
  (dolist (asn (assumptions-of m-s)) 
	  (when (and (cwa-form? asn)
		     (equal (car asn) (car m-s)))
		(return-from FIND-CWA-FOR-SET asn))))

(defun cwa-form? (form) (eq (cadr form) 'CWA))

(defun make-cwa-form (set-name members)
  `(,set-name CWA ,members))

(defun parse-cwa-form (cwa-form)
  (values (car cwa-form) (caddr cwa-form)))

(defun get-set-information (set-name &optional (*LTRE* *LTRE*))
  (let ((known-in nil) (known-out nil))
    (dolist (possible
	     (fetch `(,set-name HAS-MEMBER ?member)))
      (cond ((true? possible)
	     (push (caddr possible) known-in))
	    ((false? possible)
	     (push (caddr possible) known-out))))
    (values known-in known-out)))

(defun assume-cwa-if-needed (cwa-form)
  (when (false? cwa-form); ; Going to see a contradiction soon!
	(propagate-unknownness (get-tms-node cwa-form)))
  (assume! cwa-form :CWA))

(defun justify-cwa-if-needed (name members not-members
				   cwa-form members-form)
   ;; Since the same construal can be generated multiple times
   ;; in problem-solving, it is important to avoid creating
   ;; redundant clauses.  The previous version checked a cache
   ;; associated with the datum for the cwa-form.  This version
   ;; is simpler, reifying the contents of the clause in the database
   ;; and using a rule (defined in setrule.lisp, with the other cwa
   ;; rule) to create the clause the first time each justification is
   ;; created.  We are exploiting the fact that fetch will return the
   ;; same assertions in the same order, otherwise we would have to sort
   ;; the MEMBERS and NOT-MEMBERS lists to ensure that each CWA-JUSTIFICATION
   ;; statement was canonical.
   (assert!
    `(CWA-JUSTIFICATION
      (:AND (SET ,name)
       ,@ (mapcar #'(lambda (el)
                       `(,name HAS-MEMBER ,el))
             members)
       ,@ (mapcar #'(lambda (el)
                       `(:NOT (,name HAS-MEMBER ,el)))
             not-members)
       ,cwa-form)
      ,members-form)
    :SET-CWA-CLOSURE))

(defun fetch-cwa-for (name)
  (dolist (cwa (fetch `(,name CWA . ?x)))
	  (when (true? cwa) (return-from fetch-cwa-for cwa))))

;;;; Contradiction handling and backing up for CWA's

;;; This procedure detects when a CWA lurks within
;;; one of the LTMS' contradictions, and does a THROW 
;;; to a designated tag to signal the assumer that
;;; the set is no longer believed. 

(defun set-cwa-handler (clauses ltms set-name cwa tag
				&aux asns losers)
  (let ((cwa-node (get-tms-node cwa)))
   (dolist (cl clauses) ;; For each contradictory clause,
    (setq asns (assumptions-of-clause cl))
    (when (and (member cwa-node asns) ;; Quick filter
	       (CWA-invalid? cwa))
	  (retract-CWA cwa)
	  (throw cwa tag)))))

(defun retract-CWA (cwa)
  (retract! cwa (already-assumed? cwa)))

(defun CWA-invalid? (cwa)
  (multiple-value-bind (set-name presumed-els)
   (parse-cwa-form cwa)
   (dolist (el presumed-els)
    (unless (true? `(,set-name HAS-MEMBER ,el)) 
	    (return-from CWA-invalid? T)))
   (dolist (hm-form (fetch `(,set-name has-member ?el)))
    (when (true? hm-form)
     (unless (member (third hm-form) presumed-els
		     :TEST #'equal)
	     (return-from CWA-invalid? t))))))

(defun retract-CWAs (set)
  (dolist (cwa (fetch `(,set CWA ?members)))
   (if (and (known? cwa)
	    (already-assumed? cwa)) 
       (retract! cwa (already-assumed? cwa)))))

;;;; A simple example

(defun cwa-interactive-test (&optional (debugging? nil))
  (In-LTRE (create-ltre "CWA Test" :DEBUGGING debugging?))
  (bps-load-file *ltre-path* *set-rule-file*)
  (dolist (data '((set (Parts System))  ;; Assume initial parts
		  ((Parts System) HAS-MEMBER valve)
		  ((Parts System) HAS-MEMBER meter)
		  ((Parts System) HAS-MEMBER pump)))
    (assume! data :INITIAL-OBSERVATIONS))
  (do ((form nil)
       (stop? nil)
       (partslist nil))
      (stop? nil)
      (With-Closed-Set
       '(Parts System)
       (setq partslist 
	     (remove-if-not #'(lambda (form)
				(true? form))
			    (fetch `((Parts System) MEMBERS ?els))))
       (cond ((cdr partslist)
	      (format t "~%BUG: Conflicting membership forms.")
	      (dolist (pl partslist)
		      (format t "Parts(System) = ~A" pl)))
	     (t (format t "~% Parts are: ~A" (third (car partslist)))))
       (cond ((member form '(quit stop end exit)) (setq stop? t))
	     (t (format t "~%>")
		(setq form (read))
		(print (eval form))
		(run-rules))))))

;;;; Shakedown procedure

(defun cwa-shakedown ()
  (in-ltre (create-ltre "CWA Test One" :DEBUGGING t))
  (bps-load-file *ltre-path* *set-rule-file*)
  (uassert! '(set foo) :USER)
  (with-closed-set 'foo
		   (uassume! '(foo has-member a) :A-IN))
  (with-closed-set 'foo
		   (uassume! '(foo has-member b) :B-IN))
  (with-closed-set 'foo (run-rules))
  (if (true? `(foo members (b a)))
      (format t "~% (A B) closure okay.")
    (break "First error"))
  (with-closed-set 'foo
		   (uassume! '(foo has-member c) :C-IN))
  (with-closed-set 'foo (run-rules))
  (if (true? `(foo members (c b A)))
      (format t "~% (A B C) closure okay.")
    (break "Second error"))
  (retract! '(foo has-member a) :A-IN *ltre* nil)
  (with-closed-set 'foo (run-rules))
  (if (true? '(foo members (c b)))
      (format t "~% Retraction handled okay.")
    (break "Third error"))
  (retract-CWAs 'foo)
  (uassume! '(foo has-member a) :A-IN)
  (with-closed-set 'foo (run-rules))
  (if (true? '(foo members (c b a)))
      (format t "~% Unouting handled okay.")
    (break "Fourth error"))
  (format t "~% If no breaks occurred, CWA's look okay."))
  
  