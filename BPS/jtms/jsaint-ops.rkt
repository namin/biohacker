#lang racket

(require "utils.rkt")
(require (for-syntax "utils.rkt"))
(require "jtms.rkt")
(require "jtre.rkt")
(require (for-syntax "funify.rkt"))
(require compatibility/defmacro)
(provide (all-defined-out))

 ;;;; Helpers for operator definition

(begin-for-syntax

 (define (calculate-subproblem-list subproblems)
   (define counter -1)
  ;; Takes list of entries whose form is (?result-var ?form)
  ;; and returns a list of (?goal-var ?form)
  (map #'(lambda (pair)
	   (inc! counter)
	   (list (string->symbol (format "?goal~d" counter)) 
		 (simplifying-form-of (cadr pair))))
       subproblems))

 (define (simplifying-form-of alg-goal)
  ;; Run simplifier on subgoals, just in case.
  (cond ((null? alg-goal) '())
	((not (pair? alg-goal)) alg-goal)
	((eq? (car alg-goal) 'integral) ;; simplify as needed
	 `(integral (:eval (simplify ,(quotize (cadr alg-goal))))
	   ,(caddr alg-goal)))
	(else (cons (simplifying-form-of (car alg-goal))
		 (simplifying-form-of (cdr alg-goal))))))

 (define (calculate-solution-rule-parts sub-pairs res-pairs)
   (define counter -1)
   (define antes '())
   (define triggers '())
  (set! triggers
	(map (lambda (subpair respair)
	       (inc! counter)
	       (let ((rvar (string->symbol (format "?result~d" counter))))
		 (push! rvar antes)
		 `(:in (solution-of ,(car subpair) ,(car respair))
		       :VAR ,rvar)))
		sub-pairs res-pairs))
  (values triggers (reverse antes)))

 (define (keywordize stuff)
   (cond ((null? stuff) (error "Can't keywordize nothing."))
	 ((pair? stuff) (keywordize (car stuff)))
	 (else (string->symbol (format "~a" stuff))))))

(defmacro defintegration (name trigger . keyed-items)
  (define subproblems (cadr (member ':subproblems keyed-items)))
  (define result (cadr (member ':result keyed-items)))
  (define test (cadr (member ':test keyed-items)))
  (unless result 
    (error "Integration operator must have result form"))
  `(rule ((:in (expanded (integrate ,trigger)) :var ?starter
	       ,@(if (not (null? test)) `(:test ,test) '())))
	 (rlet ((?integral ,trigger)
		(?problem (integrate ,trigger)))
	       (rlet ((?op-instance (,name ?integral)))
	     (rassert! (operator-instance ?op-instance)
		       :op-instance-definition)
	     ;; if no subproblems, just create solution
     ,@ (cond ((null? subproblems)
	       `((rlet ((?solution
			 (:eval (simplify ,(quotize result)))))
		  (rassert! (solution-of ?problem ?solution)
		   (,(keywordize name)
		     (operator-instance ?op-instance))))))
	      (else ;; usual case
	       (let ((subs (calculate-subproblem-list subproblems))) 
		 `((rassert! (suggest-for ?problem ?op-instance)
		    (:intopexpander ?starter))
   (rule ((:in (expanded (try ?op-instance)) :var ?try))
	   (rlet ,subs
		 ,@ (map #'(lambda (sub)
			     `(queue-problem ,(car sub) ?problem))
			 subs)
		 (rassert! (and-subgoals (try ?op-instance)
					 ,(map #'car subs))
			   (,(keywordize (format "~a-def" name))
			    ?try))
		 ;; solution detector
		 ,(let-values (((triggers antes)
		                (calculate-solution-rule-parts subs subproblems)))
		    `(rule (,@ triggers)
			   (rlet ((?solution
				    (:eval (simplify ,(quotize result)))))
				 (rassert! (solution-of ?problem ?solution)
					   (,(keywordize name)
					     ,@ antes)))))))))))))))

(define (jsaint-ops)
  '())
