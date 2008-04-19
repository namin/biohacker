;; -*- Mode: Lisp; -*-

;;;; Antecedent Planner (a.k.a. Plan-A)
;; Last edited: 1/29/93, KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; This algorithm assumes that every choice in each choice set
;; has become an assumption, but that states have not yet been
;; generated.  

;; Notice how the ability to use environments as explicit
;; objects lets us revert back to CPS-like code!

(defun Plan-a (start goal &optional (*plnpr* *plnpr*))
  ;; Here start is a specific environment.
  ;; The goal is a list of conjunctions
  (do ((queue (list (list start))
	      (nconc (cdr queue) new-sprouts))
       (new-sprouts nil nil)
       (found? nil) (result nil)
       (number-examined 1 (1+ number-examined)))
      ((or found? (null queue))
       (values (setf (getf (plnpr-plist *plnpr*)
			   :PLAN) found?)
	       number-examined))
    (cond ((satisfies-goal? (caar queue) goal)
	   (setq found? (car queue)))
	  (t (dolist (op-inst (find-applicable-operators
				(caar queue)))
	      (setq result
		    (apply-operator (caar queue)
				    op-inst))
	      (unless (member result (car queue))
               (debug-plnpr t
		    "~%  Reaching ~A via ~A on ~A.."
		    result op-inst (caar queue))
	      (push (cons result
			  (cons op-inst (car queue)))
		    new-sprouts)))))))



