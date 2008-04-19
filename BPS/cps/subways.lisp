;; -*- Mode: Lisp; -*-

;;;; Test problem for CPS -- Navigating the Boston Subway
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1986-1991, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;; Problem is to get from stop A to stop B on the subway.

;; States correspond to what station we are at.  The only operator
;; is TAKE-LINE(<start station> <destination> <line>), which is
;; implemented procedurally using datastructures representing the
;; subway map.

;;; The subway map is described in terms of two structs, one for
;;; stations and the other for lines.  We assume, simplistically,
;;; that you can get from one station on a line to any other station
;;; on that line in one TAKE-LINE operation.

(in-package :COMMON-LISP-USER)

;;; First, the subway map.

(defvar *stations* nil) ;; List of symbols for station names.
(defvar *lines* nil)    ;; List of symbols for line names.

(defstruct (subway-station (:PRINT-FUNCTION
			     (lambda (inst str ignore)
			       (format str "<Station ~A>"
				       (subway-station-name inst)))))
	   (name nil) 
	   (lines nil)
	   (coordinates nil)) ;; For advanced versions of CPS

(defstruct (subway-line (:PRINT-FUNCTION
			  (lambda (inst str ignore)
			    (format str "<Subway ~A>"
				    (subway-line-name inst)))))
	   (name nil)
	   (stations nil))

;;;; Maintaining the subway map

;; The macros DEFLINE and DEFSTATION simplify constructing a
;; subway map.  The name of each line and station must be symbols.
;; The value of the symbol becomes the corresponding struct.

(defmacro defline (line-name)
 `(progn (setq ,line-name (make-subway-line :NAME ',line-name))
    (push ',line-name *lines*)))

(defmacro defstation (name lines &optional (x 0) (y 0))
`(progn (setq ,name (make-subway-station
		      :NAME ',name
		      :LINES ',lines
		      :COORDINATES (cons ,x ,y)))
   ,@ (mapcar #'(lambda (line)
		  `(push ',name (subway-line-stations ,line))) lines)
(push ',name *stations*)))

(defun clear-subway-map ()
  ;; Good ecology requires removing pointers to the structs.
  (dolist (station *stations*) (makunbound station))
  (dolist (line *lines*) (makunbound line))
  (setq *stations* nil *lines* nil))

(defun setup-subway-problem (goal-state)
  (make-problem :NAME goal-state
		:GOAL-RECOGNIZER 
		#'(lambda (state)
		    (subway-states-identical? state goal-state))
		:OPERATOR-APPLIER 'subway-operator-finder
		:OPERATORS '(TAKE-LINE)
		:STATES-IDENTICAL? 'subway-states-identical?
		:PATH-FILTER 'prune-subway-path?
		:STATE-PRINTER #'(lambda (f) (format nil "~A" f))
		:SOLUTION-ELEMENT-PRINTER 'print-path-element
		:DISTANCE-REMAINING
		#'(lambda (state)
		    (subway-distance state `(,goal-state)))))

;;; Procedures for CPS interface

(defun subway-states-identical? (state1 state2) (eq state1 state2)) 

(defun subway-operator-finder (state ignore &aux sprouts)
;;; Look up the lines that the station is on, and return
;;; the list of stations on that line.
(dolist (line (subway-station-lines (symbol-value state))
	      sprouts)
  (dolist (station (remove state
                    (subway-line-stations (symbol-value line))))
   (push (cons `(TAKE-LINE ,state ,line ,station) station)
	 sprouts))))

(defun prune-subway-path? (path)
  ; Flushes those paths which use the same subway line
  ; twice in a row.   Such solutions are scenic but silly.
    (eq (caddr (cadr (path-so-far path)))
	(caddr (cadr (cddr (path-so-far path))))))

(defun print-path-element (from-state take-line)
  (format NIL "Take the ~A to ~A."
	  (caddr take-line) from-state))

;;;; For CPS variants that use distance estimates.

(defun subway-distance (path1 path2)
;;; Uses Euclidean distance between grid coordinates
  (labels ((sqr (x) (* x x)))
  (sqrt (+ (sqr (- (car (subway-station-coordinates
			  (symbol-value (car path1))))
		  (car (subway-station-coordinates
			 (symbol-value (car path2))))))
	     (sqr (- (cdr (subway-station-coordinates
			    (symbol-value (car path1))))
		  (cdr (subway-station-coordinates
			 (symbol-value (car path2))))))))))


