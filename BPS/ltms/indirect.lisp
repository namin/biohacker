;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;;; Indirect proof mechanism for LTRE
;;; Last Edited 1/29/93, by KDF

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *LTRE*))

(defun try-indirect-proof (fact &optional (*LTRE* *LTRE*))
  (unless (known? fact)
    (with-contradiction-handler (ltre-ltms *ltre*)
      #'(lambda (contradictions ltms &aux assumptions)
	  (setq assumptions
		(assumptions-of-clause
		 (car contradictions)))
	  (let ((the-node
		 (find (datum-tms-node (referent fact T))
		       assumptions)))
	    (when the-node
	      (let ((status (tms-node-label the-node)))
		(retract-assumption the-node)
		(add-nogood the-node status
			    assumptions)))))
      ;; Assume the negation
       (assuming `((:NOT ,fact)) *LTRE*
		 (run-rules)))
   (known? fact)))

;;;; Example of indirect proof

(defun indirect-proof-example ()
  (in-ltre (create-ltre "Indirect Proof Example"))
  (assert! '(:OR p q) 'user)
  (assert! '(:IMPLIES p r) 'user)
  (assert! '(:IMPLIES q r) 'user)
  (known? 'r)
  (try-indirect-proof 'r)
  (known? 'r))
