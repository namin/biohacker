#lang racket

(provide (all-defined-out))

;;; There are two kinds of variables.
;;; Element variables match a single element of a list.
;;; Segment variables match a (perhaps empty) piece of a list.
;;; Element variables have the form (? <var name> <optional restriction>)
;;;  where <var name> is a symbol, and the restriction is a one-place
;;;  procedure which returns non-nil if the potential binding satisfies it.
;;; Segment variables are like element variables, but start with ??.

;;; The basic entry point is MATCH, which takes a pattern, a datum expression,
;;; and an alist of bindings.

(define (matcher pat dat [dict '()])
  (cond ((eq? dict ':fail) ':fail) ;; Propagate lossage
	((eq? pat dat) dict) ;; Easy win
	((element-var? pat)
	 (match-element-var pat dat dict))
	((not (pair? pat))
	 (if (~equal? pat dat) dict ':fail))
	((segment-var? (car pat))
	 (match-segment-var pat dat dict))
	((not (pair? dat)) ':fail)
	(else (matcher (cdr pat) (cdr dat)
		       (matcher (car pat) (car dat) dict)))))

(define (match-element-var pat dat dict)
  (define entry (lookup-var pat dict))
  (cond (entry 
	 (if (~equal? (cadr entry) dat) dict ':fail))
	(else (let ((pred (var-restriction pat)))
	        (cond ((or (null? pred)
		           ((eval pred) dat))
		       (bind-element-var (var-name pat) dat dict))
		      (else ':fail))))))

(define *tol* 1.0e-6)

(define (~equal? a b)
  (cond ((and (real? a) (real? b)) (< (abs (- a b)) *tol*))
	(else (equal? a b))))

;;;; Finding matches for segment variables
;; This is non-deterministic, hence requires iteration.

(define (match-segment-var pat dat dict)
  (define entry (lookup-var (car pat) dict))
  (cond (entry ;; check for match
         (let ((rest 
	         (check-segment dat (segment-beg entry)
			        (segment-end entry))))
	   (if (eq? rest ':fail) ':fail
	       (matcher (cdr pat) rest dict))))
	(else ;; Search for alternate segment bindings
	 (try-segment-bindings (car pat) (cdr pat) dat dict))))

(define (check-segment dat beg end)
  (cond ((eq? beg end) dat)
	((not (pair? dat)) ':fail)
	((~equal? (car dat) (car beg))
	 (check-segment (cdr dat) (cdr beg) end))
	(else ':fail)))

(define (try-segment-bindings var pat dat dict)
  (with-handlers
   ([(lambda (x) #f) (lambda (x) x)])
   (let ((name (var-name var))
         (pred (var-restriction var))
         (beg dat))
     (do ((end dat (cdr end))
          (ndict '()))
         ((null? end)
          (cond ((or (null? pred)
		     (pred (segment->list beg '())))
	         (matcher pat '() ;; Try the very end
		          (bind-segment-var name beg '() dict)))
	        (else ':fail)))
       (when (or (null? pred)
	         (pred (segment->list beg end)))
         (set! ndict (matcher pat end 
		              (bind-segment-var name beg end dict)))
         (unless (eq? ndict ':fail)
	   (raise ndict)))))))

;;;; Defining variables

(define (pattern-variable? x) (or (element-var? x) (segment-var? x)))
(define (element-var? x) (and (pair? x) (eq? (car x) '?)))
(define (segment-var? x) (and (pair? x) (eq? (car x) '??)))
(define (var-name x) (cadr x))
(define (var-restriction x) (if (null? (cddr x)) '() (caddr x)))

;; Dictionary entries take the form
;; (<name> <position> <value>), where <position> is NIL if an element
;;   variable, and (<beg> . <end>) if a segment variable.

;; Accessing entries
(define (lookup-var var dict) (assoc (var-name var) dict))

(define (var-value var dict)
  (define entry (lookup-var var dict))
  (unless entry (error 'var-value (format "Not bound variable: ~a, ~a." var dict)))
  (cond ((= (length entry) 2) (cadr entry)) ;; element variable
	(else (segment->list (cadr entry) (caddr entry)))))

(define (segment-beg entry) (cadr entry))
(define (segment-end entry) (caddr entry))

(define (segment->list start end)
  (do ((point start (cdr point))
       (l '()))
      ((eq? point end) (reverse l))
    (set! l (cons (car point) l))))

;; Updating dictionaries
(define (bind-element-var name dat dict)
  (cons (list name dat) dict))
(define (bind-segment-var name beg end dict)
  (cons (list name beg end) dict))

;; Performing substitutions
(define (substitute-in exp dict)
  (cond ((null? exp) '())
	((element-var? exp) (var-value exp dict))
	((cons? exp)
	 (cond ((segment-var? (car exp))
		(append (var-value (car exp) dict)
			(substitute-in (cdr exp) dict)))
	       ((eq? (car exp) ':eval)
		(eval (substitute-in (cadr exp) dict)))
	       ((and (pair? (car exp)) (eq? (caar exp) ':splice))
		(append (eval (substitute-in (cadar exp) dict))
			(substitute-in (cdr exp) dict)))
	       (else (cons (substitute-in (car exp) dict)
			   (substitute-in (cdr exp) dict)))))
	(else exp)))
