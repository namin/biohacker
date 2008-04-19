;; -*- Mode: Lisp; Syntax: Common-lisp -*-

;;;; Example of dependency-directed search using JTRE
;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1986--1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Statistics
(defvar *n-assumptions* 0)
(defvar *placements* nil)

(proclaim '(special *JTRE*))

(defvar *queen-rules-file*
  #+ILS "/u/bps/code/jtms/jqrule.rbin"
  #+PARC "virgo:/virgo/dekleer/bps/code/jtms/jqrule.lisp"
  #+MCL "Macintosh HD:BPS:jtms:jqrule.fasl")

(defun test-queens (from to)
  (do ((n from (1+ n)))
      ((> n to))
      (gc)
      (time (n-queens n))
      (format t "~% For n=~D, ~D solutions, ~D assumptions."
	     n (length *placements*) *n-assumptions*)))

(defun n-queens (n &optional (debugging? nil))
  (setup-queens-puzzle n debugging?)
  (solve-queens-puzzle (make-queens-choice-sets n))
  (length *placements*))

;;;; Setup and search

(defun setup-queens-puzzle (n &optional (debugging? nil))
  (in-JTRE (create-jtre (format nil "~D-Queens JTRE" n) 
			:DEBUGGING debugging?))
  (setq *placements* nil
	*n-assumptions* 0)
  (load *queen-rules-file*))

(defun make-queens-choice-sets (n)
  (do ((column 1 (1+ column))
       (column-queens nil nil)
       (choice-sets nil))
      ((> column n) (nreverse choice-sets))
    (dotimes (row n)
     (push `(Queen ,column ,(1+ row)) column-queens))
    (push (nreverse column-queens) choice-sets)))

(defun solve-queens-puzzle (choice-sets)
  (cond ((null choice-sets) (gather-queens-solution))
	(t (dolist (choice (car choice-sets))
	    (unless (in? `(not ,choice) *jtre*)
	     ;respect nogood information
     (multiple-value-bind (nogood? asns)
      (try-in-context choice
       `(solve-queens-puzzle ',(cdr choice-sets))
       *jtre*)
      (incf *n-assumptions*)
      (when nogood?
	    ;;This assumption lost, so justify the negation
	    ;; based on the other relevant assumptions.
	    (assert! `(not ,choice)
		     `(Nogood ,@ 
			      (remove choice asns))))))))))

;;;; JTMS approximation to try-in-context

(defun try-in-context (asn thunk jtre &aux try-marker result)
  (setq try-marker (cons 'TRY asn))
  (with-contradiction-handler (jtre-jtms jtre)
        #'(lambda (jtms contras)
	    (try-contradiction-handler
	     contras jtms asn try-marker jtre))
        (unwind-protect
	  (progn (unless (in? asn jtre)
		   (setq result (catch 'TRY-CONTRADICTION-FOUND
				  (assume! asn try-marker jtre)))
		   (when (and (listp result) (eq (car result) :ASNS))
		     (return-from TRY-IN-CONTEXT
				  (values t (mapcar #'view-node
						    (cdr result)))))
		   (setq result (catch 'TRY-CONTRADICTION-FOUND
				  (run-rules jtre)))
		   (when (and (listp result)  (eq (car result) :ASNS))
		     (return-from TRY-IN-CONTEXT
				  (values t (mapcar #'view-node
						    (cdr result)))))
		   (eval thunk) ;; use the thunk
		   (progn (retract! asn try-marker t)
			  (return-from TRY-IN-CONTEXT
				       (values nil nil))))))))

(defun try-contradiction-handler (contras jtms asn marker *JTRE*
					  &aux node)
  (unless (eq jtms (jtre-jtms *JTRE*))
    (error "~%High Contradiction Weirdness: ~A not jtms for ~A!"
	   jtms *JTRE*))
  (unless contras (return-from TRY-CONTRADICTION-HANDLER nil))
  (unless asn (return-from TRY-CONTRADICTION-HANDLER nil))
  (setq node (get-tms-node asn))
  (dolist (cnode contras)
    (let ((asns (assumptions-of-node cnode)))
      (when (member node asns)
	(retract! asn marker)
	(throw 'TRY-CONTRADICTION-FOUND (cons :ASNS asns))))))

;;; Other helpers

(defun queens-okay? (x1 y1 x2 y2)
  (not (or (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))))

(defun gather-queens-solution ()
  (push (remove-if #'(lambda (q) (out? q *jtre*))
		   (fetch `(Queen ?c ?r) *jtre*))
	*placements*))

(defun show-queens-solution (solution &aux n)
  (setq n (length solution))
  (dotimes (i n)
    (terpri)
    (dotimes (j n)
      (format t "~A"
	      (if (member `(queen ,i ,j) solution
			  :TEST #'equal) "Q" "-")))))