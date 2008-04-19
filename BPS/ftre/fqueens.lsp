;; -*- Mode: Lisp; -*-

;;;; N-Queens puzzle, using FTRE.
;;;;  Modified: forbus on Tue Apr 2 10:22:45 1996

;;; Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Statistics
(defvar *n-assumptions* 0) ;number of assumptions made (statistics)
(defvar *placements* nil) ;successful solutions

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

(defun setup-queens-puzzle (n debugging?)
  (in-ftre 
   (create-ftre (format nil "~D queens" n)
		:DEBUGGING debugging?
		:MAX-DEPTH (+ n 1)))
  (setq *placements* nil
	*n-assumptions* 0)
  (bps-load-file *ftre-path* *fqueen-rule-file*))

(defun make-queens-choice-sets (n)
  (do ((column 1 (1+ column))
       (column-queens nil nil)
       (choice-sets nil))
      ((> column n) (nreverse choice-sets))
    (dotimes (row n)
     (push `(Queen ,column ,(1+ row)) column-queens))
    (push (nreverse column-queens) choice-sets)))

;;; The chronological search itself

(defun solve-queens-puzzle (choice-sets)
  (cond ((fetch 'contradiction)
	 (return-from solve-queens-puzzle nil))
	(choice-sets ;; Make next choice
	 (dolist (choice (car choice-sets))
	  (incf *n-assumptions*)
	  (try-in-context choice
	     `(solve-queens-puzzle ',(cdr choice-sets)))))
	(t ;; Got a consistent set of placements
	 (gather-queens-solution))))

;;;; Utilities

(defun queens-okay? (x1 y1 x2 y2)
  (not (or (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))))

(defun gather-queens-solution ()
  (push (fetch '(Queen ?x ?y) *ftre*) *placements*))

(defun show-queens-solution (solution &aux n)
  (setq n (length solution))
  (dotimes (i n)
    (terpri)
    (dotimes (j n)
      (format t "~A"
	      (if (member `(queen ,i ,j) solution
			  :TEST #'equal) "Q" "-")))))


