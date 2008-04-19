;; -*- Mode: Lisp; -*-

;;;; Pattern matcher for algebraic manipulation systems
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This version is inspired by one of G.J. Sussman's scheme matchers.
;;; We eschew continuation-passing, for clarity.

;;; There are two kinds of variables.
;;; Element variables match a single element of a list.
;;; Segment variables match a (perhaps empty) piece of a list.
;;; Element variables are (? <var name> <optional restriction>)
;;;  where <var name> is a symbol, and the restriction is a one-place
;;;  procedure which returns non-nil if the binding satisfies it.
;;; Segment variables are like element variables, but start with ??.

(in-package :COMMON-LISP-USER)

;;; The entry point is MATCH, which takes a pattern, an expression,
;;; and an alist of bindings.

(defun match (pat dat &optional (dict nil))
  (cond ((eq dict :FAIL) :FAIL) ;; Propagate lossage
	((eq pat dat) dict)
	((element-var? pat)
	 (match-element-var pat dat dict))
	((not (consp pat))
	 (if (equal? pat dat) dict :FAIL))
	((segment-var? (car pat))
	 (match-segment-var pat dat dict))
	((not (consp dat)) :FAIL)
	(t (match (cdr pat) (cdr dat)
		  (match (car pat) (car dat) dict)))))

(defun match-element-var (pat dat dict &aux entry pred)
  (setq entry (lookup-var pat dict))
  (cond (entry 
	 (if (equal? (cadr entry) dat) dict :FAIL))
	(t (setq pred (var-restriction pat))
	   (cond ((or (not pred)
		      (funcall pred dat))
		  (bind-element-var (var-name pat) dat dict))
		 (t :FAIL)))))

(defvar *tol* 1.0e-6)

(defun equal? (a b)
  (cond ((and (floatp a) (floatp b)) (< (abs (- a b)) *tol*))
	(t (equal a b))))

;;;; Finding matches for segment variables
;; This is non-deterministic, hence requires iteration.

(defun match-segment-var (pat dat dict &aux entry pred end rest)
  (setq entry (lookup-var (car pat) dict))
  (cond (entry ;; check for match
         (setq rest 
	       (check-segment dat (segment-beg entry)
			      (segment-end entry)))
	 (if (eq rest :FAIL) :FAIL
	     (match (cdr pat) rest dict)))
	(t ;; Search for alternate segment bindings
	 (try-segment-bindings (car pat) (cdr pat) dat dict))))

(defun check-segment (dat beg end)
  (cond ((eq beg end) dat)
	((not (consp dat)) :FAIL)
	((equal? (car dat) (car beg))
	 (check-segment (cdr dat) (cdr beg) end))
	(t :FAIL)))

(defun try-segment-bindings (var pat dat dict &aux name pred beg)
  (setq name (var-name var)
	pred (var-restriction var)
	beg dat)
  (do ((end dat (cdr end))
       (ndict nil))
      ((null end)
       (cond ((or (null pred)
		  (funcall pred (segment->list beg nil)))
	      (match pat nil ;; Try the very end
		     (bind-segment-var name beg nil dict)))
	     (t :FAIL)))
    (when (or (null pred)
	      (funcall pred (segment->list beg end)))
      (setq ndict (match pat end 
			 (bind-segment-var name beg end dict)))
      (unless (eq ndict :FAIL)
	      (return-from TRY-SEGMENT-BINDINGS ndict)))))

;;;; Defining variables

(defun pattern-variable? (x) (or (element-var? x) (segment-var? x)))
(defun element-var? (x) (and (consp x) (eq (car x) '?)))
(defun segment-var? (x) (and (consp x) (eq (car x) '??)))
(defun var-name (x) (cadr x))
(defun var-restriction (x) (caddr x))

;; Dictionary entries take the form
;; (<name> <position> <value>), where <position> is NIL if an element
;;   variable, and (<beg> . <end>) if a segment variable.

;; Accessing entries
(defun lookup-var (var dict) (assoc (var-name var) dict))

(defun var-value (var dict &aux entry)
  (setq entry (lookup-var var dict))
  (unless entry (error "Not bound variable: ~A, ~A." var dict))
  (cond ((= (length entry) 2) (cadr entry)) ;; element variable
	(t (segment->list (cadr entry) (caddr entry)))))

(defun segment-beg (entry) (cadr entry))
(defun segment-end (entry) (caddr entry))

(defun segment->list (start end)
  (do ((point start (cdr point))
       (l nil))
      ((eq point end) (nreverse l))
    (push (car point) l)))

;; Updating dictionaries
(defun bind-element-var (name dat dict)
  (cons (list name dat) dict))
(defun bind-segment-var (name beg end dict)
  (cons (list name beg end) dict))

;; Performing substitutions
(defun substitute-in (exp dict)
  (cond ((null exp) nil)
	((element-var? exp) (var-value exp dict))
	((consp exp)
	 (cond ((segment-var? (car exp))
		(append (var-value (car exp) dict)
			(substitute-in (cdr exp) dict)))
	       ((eq (car exp) :EVAL)
		(eval (substitute-in (cadr exp) dict)))
	       ((and (consp (car exp)) (eq (caar exp) :SPLICE))
		(append (substitute-in (cadar exp) dict)
			(substitute-in (cdr exp) dict)))
	       (t (cons (substitute-in (car exp) dict)
			(substitute-in (cdr exp) dict)))))
	(t exp)))

