#lang racket

(require "utils.rkt")
(require (for-syntax "utils.rkt"))
(require "jtms.rkt")
(require "jtre.rkt")
(require (for-syntax "funify.rkt"))
(require "simplify.rkt")
(require compatibility/defmacro)
(provide (all-defined-out))

 ;;;; Helpers for operator definition

(begin-for-syntax

 (define (calculate-subproblem-list subproblems)
   (define counter -1)
  ;; Takes list of entries whose form is (?result-var ?form)
  ;; and returns a list of (?goal-var ?form)
   (map (lambda (pair)
	  (inc! counter)
	   (list (string->symbol (format "?goal~a" counter)) 
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
	       (let ((rvar (string->symbol (format "?result~a" counter))))
		 (push! rvar antes)
		 `(:in (solution-of ,(car subpair) ,(car respair))
		       :var ,rvar)))
		sub-pairs res-pairs))
  (values triggers (reverse antes)))

 (define (keywordize stuff)
   (cond ((null? stuff) (error "Can't keywordize nothing."))
	 ((pair? stuff) (keywordize (car stuff)))
	 (else (string->symbol (format "~a" stuff))))))

;;;; Defining operators

(defmacro defintegration (name trigger . keyed-items)
  (define subproblems (cadr-if (member ':subproblems keyed-items)))
  (define result (cadr-if (member ':result keyed-items)))
  (define test (cadr-if (member ':test keyed-items)))
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
                     ,@(cond ((or (not subproblems) (null? subproblems))
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
		                              ,@(map (lambda (sub)
			                               `(queue-problem ,(car sub) ?problem))
			                             subs)
		                              (rassert! (and-subgoals (try ?op-instance)
					                              ,(map car subs))
			                                (,(keywordize (format "~a-def" name))
			                                 ?try))
		                              ;; solution detector
		                              ,(let-values (((triggers antes)
		                                             (calculate-solution-rule-parts subs subproblems)))
		                                 `(rule (,@triggers)
			                                (rlet ((?solution
				                                (:eval (simplify ,(quotize result)))))
				                              (rassert! (solution-of ?problem ?solution)
					                                (,(keywordize name)
					                                 ,@antes)))))))))))))))

(define (jsaint-ops)

(defintegration integral-of-constant
  (integral ?t ?var) 
  :test (not (occurs-in? ?var ?t))
  :result (* ?t ?var))

(defintegration integral-of-self
  (integral ?exp ?exp)
  :result (/ (expt ?exp 2) 2))

(defintegration move-constant-outside
  (integral (* ?const ?nonconst) ?var)
  :test (and (not (occurs-in? ?var ?const))
	     (occurs-in? ?var ?nonconst))
  :subproblems ((?int (integrate (integral ?nonconst ?var))))
  :result (* ?const ?int))

(defintegration integral-of-sum
  (integral (+ ?t1 ?t2) ?var)
  :subproblems ((?int1 (integrate (integral ?t1 ?var)))
		(?int2 (integrate (integral ?t2 ?var))))
  :result (+ ?int1 ?int2))

(defintegration integral-of-nary-sum
  (integral (+ ?t1 ?t2 . ?trest) ?var)
  :subproblems ((?int1 (integrate (integral ?t1 ?var)))
		(?int2 (integrate (integral ?t2 ?var)))
		(?intr (integrate (integral (+ . ?trest) ?var))))
  :test (not (null ?trest))
  :result (+ ?int1 ?int2 ?intr))

(defintegration integral-of-uminus
  (integral (- ?term) ?var)
  :subproblems ((?int (integrate (integral ?term ?var))))
  :result (- ?int))

(defintegration integral-of-minus
  (integral (- ?t1 ?t2) ?var)
  :subproblems ((?int1 (integrate (integral ?t1 ?var)))
		(?int2 (integrate (integral ?t2 ?var))))
  :result (- ?int1 ?int2))

(defintegration integral-of-sqr
  (integral (sqr ?var) ?var)
  :result (/ (expt ?var 3) 3))

(defintegration integral-of-polyterm
  (integral (expt ?var ?n) ?var)
  :test (not (same-constant? ?n -1))
  :result (/ (expt ?var (+ 1 ?n)) (+ 1 ?n)))

;;;; some exponentials and trig functions

(defintegration simple-e-integral
  (integral (expt %e ?var) ?var)
  :result (expt %e ?var))

(defintegration e-integral
  (integral (expt %e (* ?a ?var)) ?var)
  :test (not (occurs-in? ?var ?a))
  :result (/ (expt %e (* ?a ?var)) ?a))

(defintegration non-e-power-integral
  (integral (expt ?b (* ?a ?var)) ?var)
  :test (and (not (occurs-in? ?var ?a))
	     (not (occurs-in? ?var ?b)))
  :result (/ (expt ?b (* ?a ?var)) (* ?a (log ?b %e))))

(defintegration log-integral
  (integral (log ?var %e) ?var)
  :result (- (* ?var (log ?var %e)) ?var))

(defintegration sin-integral
  (integral (sin (* ?a ?var)) ?var)
  :test (not (occurs-in? ?var ?a))
  :result (- (/ (cos (* ?a ?var)) ?a)))

(defintegration cos-integral
  (integral (cos (* ?a ?var)) ?var)
  :test (not (occurs-in? ?var ?a))
  :result (/ (sin (* ?a ?var)) ?a))

(defintegration sin-sqr-integral
  (integral (sqr (sin ?var)) ?var)
  :result (- (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

(defintegration cos-sqr-integral
  (integral (sqr (cos ?var)) ?var)
  :result (+ (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

;;;; some not-so-clever operators

(defintegration sintocossqrsub
  (integral ?exp ?var)
  :test (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :subproblems
  ((?int (integrate (integral
		     (:eval (subst `(sqrt (- 1 (expt (cos ,?var) 2)))
				   `(sin ,?var)
				   ?exp)) ?var))))
  :result ?int)

(defintegration costosinsqrsub
  (integral ?exp ?var)
  :test (and (occurs-in? ?var ?exp)
	     (occurs-in? `(cos ,?var) ?exp))
  :subproblems
  ((?int (integrate (integral
		     (:eval (subst `(sqrt (- 1 (expt (sin ,?var) 2)))
				   `(cos ,?var)
				   ?exp)) ?var))))
  :result ?int)

(defintegration sinsqrtotancossub
  (integral ?exp ?var)
  :test (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :subproblems ((?int (integrate (integral
				  (:eval (subst `(* (sqr (tan ,?var))
						    (sqr (cos ,?var)))
						`(sin ,?var)
						?exp))
				  ?var))))
  :result ?int)
)

(define (queue-problem problem parent)
  'TODO)
