;;; -*- Mode: Lisp; Syntax: Common-lisp; -*-
;;;;  Modified: forbus on Thurs Apr 18 8:58:35 1996

;;;; Dependency-directed search facility
;;;; Last Edited 4/27/94, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *debug-dds* nil)

(defmacro debug-dds (str &rest args)
  `(if *debug-dds* (format t ,str ,@ args)))

(defun DD-Search (choice-sets end &aux answer marker choices)
  (when (null choice-sets)
    (debug-dds "~%    DDS: Found solution.")
    (eval end)
    (return-from DD-Search nil))
  (setq marker (list 'DDS (car choice-sets)))
  (setq choices (car choice-sets))
  (dolist (choice choices)
    (debug-dds "~%    DDS: Considering ~A..." choice)
    (cond ((false? choice) ;skip if known loser
	   (debug-dds "~%    DDS: ~A already known nogood." choice))
	  ((true? choice) ;continue if known
	   (debug-dds "~%    DDS: ~A true by implication." choice)
	   (DD-Search (cdr choice-sets) end)
	   (return nil))
          (t (debug-dds "~%    DDS: Assuming ~A." choice)
             (with-Contradiction-Handler (ltre-ltms *ltre*)
              #'(lambda (clauses ltms &aux asns)
                   (debug-dds "~%    DDS: Entering handler for ~A with ~A~A."
                    choice clauses
                    (mapcar #'(lambda (c) (violated-clause? c))
                       clauses))
                   (dolist (cl clauses)
                      (setq asns (assumptions-of-clause cl))
                      (debug-dds "~%    DDS: Assumptions are: ~A"
                       (mapcar #'view-node asns))
                      (dolist (asn asns)
                         (when (or (equal choice (view-node asn))
                                   (and (listp choice) (eq (car choice) :NOT)
                                        (equal (cadr choice) (view-node asn))))
                            (throw marker
                               (cons :LOSERS ;; Assign labels before any retraction
                                  ;; Failure to do so can result in incorrect nogoods.
                                  (mapcar #'signed-view-node
                                     (delete asn asns))))))))
              (setq answer (catch marker
                              (Assuming (list choice) *ltre*
                               (run-rules *ltre*)
                               (DD-Search (cdr choice-sets) end))))
              (when (and (listp answer)
                         (eq (car answer) :LOSERS))
                 (debug-dds "~%    DDS: ~A inconsistent with ~A."
                  choice (mapcar #'view-node (cdr answer)))
                 (assert! `(:NOT (:AND ,choice
                                  ,@ (cdr answer)))
                  :DD-SEARCH-NOGOOD)))))))

;;;; A familiar example

(defun Test-DD-search (&optional (debugging? t))
  (in-LTRE (create-ltre "DDS Test" :DEBUGGING debugging?))
  (eval '(rule ((:TRUE A) (:TRUE C))
	       (rassert! (:NOT (:AND A C)) :DOMAIN-NOGOOD)))
  (eval '(rule ((:TRUE B) (:TRUE E))
	       (rassert! (:NOT (:AND B E)) :DOMAIN-NOGOOD)))
  (DD-Search '((A B) (C D) (E F)) 
	     '(show-DD-test-solution)))
	    
(defun show-DD-test-solution (&aux result)
  (dolist (var '(F E D C B A))
    (when (true? var *ltre*) (push var result)))
  (format t "~% Consistent solution: (~A)." result))
