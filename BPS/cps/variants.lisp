;; -*- Mode: Lisp; -*-

;;; Different search strategies
;;; Last Edited: 1/29/93, KDF

;;; Copyright (c) 1986-1991, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Small variations in BSOLVE suffice to
;; implement a variety of search strategies.

(defun dsolve (initial pr)
  (do ((queue (list (make-path :CURRENT initial
			       :SO-FAR (list initial)
			       :PR pr))
	      ; Small change, large difference!
	      (append new-paths (cdr queue)))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
	(format t "~%Found goal state: ~A"
		(funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))	
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%State explored: ~A"
	      (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~%New operator instances:")
      (print-new-paths new-paths))))

;;; Best-solve

(defun best-solve (initial pr)
  (unless (and (pr-distance-remaining pr)
	       (functionp (pr-distance-remaining pr)))
    (error "~%Distance estimation procedure must be defined first!"))
  (do ((queue
	(list (make-path :CURRENT initial :SO-FAR (list initial) :PR pr
			:DISTANCE (funcall (pr-distance-remaining pr)
					   (list initial))))
	      ;merge new paths, keeping queue sorted by distance to goal.
	(let ((nqueue (cdr queue)))
	  (dolist (path new-paths nqueue)
	  (setf (path-distance path) ; estimate distance
		(funcall (pr-distance-remaining pr) (path-so-far path)))
	  (setq nqueue
		(merge 'list (list path) nqueue ; insert in order
		       #'< :KEY #'(lambda (x) (path-distance x)))))))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
	(format t "~%Found goal state: ~A"
		(funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))	
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%State explored: ~A"
	      (funcall (pr-state-printer pr)  (path-current (car queue))))
      (format t "~%New operator instances:")
      (print-new-paths new-paths))))

;;; Beam solve

(defun beam-solve (initial pr &optional (n 3))
  (do ((queue
	(list (make-path :CURRENT initial :SO-FAR (list initial)
			:PR pr :DISTANCE (list initial)))
	(let ((nqueue (cdr queue)))
	  (dolist (path new-paths)
	    (setf (path-distance path) ; estimate distance
		  (funcall (pr-distance-remaining pr) (path-so-far path)))
	    (setq nqueue (merge 'list (list path) nqueue ; insert in order
				#'<
				:KEY #'(lambda (x) (path-distance x)))))
	  (when (> (length nqueue) n) ;; clips all but first n
	    (setf (cdr (nthcdr (1- n) nqueue)) nil))
		nqueue))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
	(format t "~%Found goal state: ~A"
		(funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))	
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%State explored: ~A"
	      (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~%New operator instances:")
      (print-new-paths new-paths))))
