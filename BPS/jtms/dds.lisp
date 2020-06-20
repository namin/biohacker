;;; -*- Mode: Lisp; Syntax: Common-lisp; -*-
;;;;  Modified: forbus on Thurs Apr 18 8:58:35 1996

;;;; Based on the LTMS version
;;;; Dependency-directed search facility
;;;; Last Edited 4/27/94, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defmacro assuming (asn &body body)
  `(unwind-protect (let ((already? (in? ,asn)))
     (unless already?
       (assume! ,asn :try))
     ,@body
     (unless already?
       (retract! ,asn :try)))))


(defvar *debug-dds* nil)

(defmacro debug-dds (str &rest args)
  `(if *debug-dds* (format t ,str ,@ args)))

(defun assert-choices! (choice-sets)
  (loop for choices in choice-sets do
        (assert! `(:OR ,@choices) :CHOICE))
  choice-sets)

(defun prune (choice-sets)
  ;;(sort (mapcar #'(lambda (choices) (remove-if #'false? choices)) choice-sets) #'< :key #'length)
  (sort choice-sets #'< :key #'length))

(defun DD-init ()
  (eval (contradiction 'DD-Search-NoGood *jtre*)))

(defun DD-Search (choice-sets end &aux answer marker choices)
  (when (null choice-sets)
    (debug-dds "~%    DDS: Found solution.")
    (eval end)
    (return-from DD-Search nil))
  (setq marker (list 'DDS (car choice-sets)))
  (setq choice-sets (prune choice-sets))
  (setq choices (car choice-sets))
  (dolist (choice choices)
    (debug-dds "~%    DDS: Considering ~A..." choice)
    (cond ;;((false? choice) ;skip if known loser
	  ;; (debug-dds "~%    DDS: ~A already known nogood." choice))
	  ((in? choice) ;continue if known
	   (debug-dds "~%    DDS: ~A true by implication." choice)
	   (DD-Search (cdr choice-sets) end)
	   (return nil))
          (t (debug-dds "~%    DDS: Assuming ~A." choice)
             (with-Contradiction-Handler (jtre-jtms *jtre*)
              #'(lambda (jtms clauses &aux asns)
                   (debug-dds "~%    DDS: Entering handler for ~A with ~A~A."
                    choice clauses
                    ;;(mapcar #'(lambda (c) (violated-clause? c))
		    ;;    clauses)
		    clauses
		    )
                   (dolist (cl clauses)
                      (setq asns (assumptions-of-node cl))
                      (debug-dds "~%    DDS: Assumptions are: ~A"
                       (mapcar #'view-node asns))
                      (dolist (asn asns)
                         (when (or (equal choice (view-node asn))
                                   (and (listp choice) (eq (car choice) :NOT)
                                        (equal (cadr choice) (view-node asn))))
                            (throw marker
                               (cons :LOSERS ;; Assign labels before any retraction
                                  ;; Failure to do so can result in incorrect nogoods.
                                  (mapcar #'view-node
					  (delete asn asns))))))))
	      (unwind-protect
              (setq answer (catch marker
                              (Assuming choice
                               (run-rules *jtre*)
                               (DD-Search (cdr choice-sets) end))))
              (when (and (listp answer)
                         (eq (car answer) :LOSERS))
                 (debug-dds "~%    DDS: ~A inconsistent with ~A."
			    choice (cdr answer))
		 (retract! choice :try)
                 ;; (assert! 'DD-Search-Nogood
		 ;; 	  `(Combo ,choice
		 ;; 		 ,@ (cdr answer)))
                  )))))))

;;;; A familiar example

(defun Test-DD-search (&optional (debugging? t))
  (in-JTRE (create-jtre "DDS Test" :DEBUGGING debugging?))
  (eval (contradiction 'Contra *jtre*))
  (eval '(rule ((:IN A) (:IN C))
	       (rassert! Contra (Combo A C))))
  (eval '(rule ((:IN B) (:IN E))
	       (rassert! Contra (Combo B E))))
  (dd-init)
  (DD-Search '((A B) (C D) (E F)) 
	     '(show-DD-test-solution)))
	    
(defun show-DD-test-solution (&aux result)
  (dolist (var '(F E D C B A))
    (when (in? var *jtre*) (push var result)))
  (format t "~% Consistent solution: (~A)." result))
