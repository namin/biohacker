#lang racket

(require "utils.rkt")
(require (for-syntax "utils.rkt"))
(require "jtms.rkt")
(require "jtre.rkt")
(require "funify.rkt")
(require (for-syntax "funify.rkt"))
(require "simplify.rkt")
(require compatibility/defmacro)
(require "jsaint-rules.rkt")
(provide (all-defined-out))

(struct
 jsaint
 (title;; ""        ;; Name for printing
  jtre;; nil        ;; Associated JTRE
  agenda;; nil      ;; List of queued subproblems
  problem;; nil     ;; When solved, we are done.
  solution;; nil    ;; Cached answer.
  n-subproblems;; 0 ;; Statistic
  max-tasks;; 20    ;; resource bound
  debugging;; nil   ;; Debugging flag
        )
 #:mutable
 #:methods gen:custom-write
 [(define (write-proc this port mode)
    (fprintf port "<Agenda: ~a>" (jsaint-title this)))]
 )

(define *jsaint* #f)

(define (create-jsaint title problem #:debugging (debugging #f) #:max-tasks (max-tasks #f))
  (let ((ag (jsaint
             title
             (create-jtre (format "JTRE of ~a" title))
             '()
             problem
             '()
             0
             (if (integer? max-tasks) max-tasks 20)
             debugging)))
    (in-jtre (jsaint-jtre ag))
    (change-jtms (jtre-jtms (jsaint-jtre ag))
                 #:contradiction-handler jsaint-contradiction-handler)
    (use-jsaint ag)
    ag))

(define-syntax debugging-jsaint
  (syntax-rules ()
    [(_ js msg arg ...)
     (when (jsaint-debugging js) (printf msg arg ...))]))

(define (change-jsaint js
                       #:debugging (debugging ':nada)
                       #:problem (problem ':nada)
                       #:max-tasks (max-tasks ':nada))
  (unless (eq? debugging ':nada) (set-jsaint-debugging! js debugging))
  (unless (eq? problem ':nada) (set-jsaint-problem! js problem))
  (unless (eq? max-tasks ':nada) (set-jsaint-max-tasks! js max-tasks)))

(define (change-jsaint-debug-all js)
  (set-jsaint-debugging! js #t)
  (set-jtre-debugging! (jsaint-jtre js) #t)
  (set-jtms-debugging! (jtre-jtms (jsaint-jtre js)) #t))

(define-syntax with-jsaint
  (syntax-rules ()
    [(_  js body ...)
     (if (eq? js *jsaint*)
         (begin body ...)
         (let ((old-js *jsaint*))
           (use-jsaint js)
           (let ((r (begin body ...)))
             (use-jsaint old-js)
             r)))]))

(define (use-jsaint js) (set! *jsaint* js))

;;;; User entry point

(define (solve-integral integral
                       #:title (title (symbol->string (gensym)))
                       #:debugging (debugging #f)
                       #:debugging-all (debugging-all #f)
                       #:max-tasks (max-tasks 20))
  ;; Remove redudancies and canonicalize input.
  (set! integral (eval (quotize (simplifying-form-of integral))))
  (use-jsaint (create-jsaint title integral
                             #:debugging debugging
                             #:max-tasks max-tasks))
  (queue-problem (jsaint-problem *jsaint*) '())
  (with-jtre (jsaint-jtre *jsaint*)
            (jsaint-rules)
            (jsaint-operators))
  (when debugging-all
    (change-jsaint-debug-all *jsaint*))
  (run-jsaint *jsaint*))

(define (explain-result [js *jsaint*])
  (with-jsaint
   js
   (cond
    ((null? (jsaint-solution *jsaint*))
     (printf "\n Problem not solved yet."))
    ((eq? (jsaint-solution *jsaint*) ':failed-problem)
     (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
                                    (jsaint-jtre *jsaint*)))
     (printf "\n Failed to find a solution."))
    ((eq? (jsaint-solution *jsaint*) ':failed-empty)
     (printf "\n Ran out of things to do.")
     (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*))
                                    (jsaint-jtre *jsaint*))))
    (else (printf "\n Solved the problem:")
          (explore-network (get-tms-node
                            `(solution-of ,(jsaint-problem *jsaint*)
                                          ,(jsaint-solution *jsaint*))
                            (jsaint-jtre *jsaint*)))))))

;;;; Basic algorithm

(define (run-jsaint js)
  (with-jsaint
   js
   (cond
    ((not (null? (jsaint-solution *jsaint*)))
     (values (jsaint-solution *jsaint*) *jsaint*)) ;; Don't re-solv
    ((> (jsaint-n-subproblems *jsaint*)
        (jsaint-max-tasks *jsaint*))
     (values ':time-out *jsaint*)) ;; Respect resource limits
    (else
     (do ((done? #f)
          (solution (fetch-solution (jsaint-problem *jsaint*) *jsaint*)
                    (fetch-solution (jsaint-problem *jsaint*) *jsaint*))
          (failure-signal `(Failed (Integrate ,(jsaint-problem *jsaint*)))))
         (done? (values (jsaint-solution *jsaint*) *jsaint*))
       ;;(show-ao-graph)
       (cond
        ((not (null? solution))
         (set-jsaint-solution! *jsaint* solution)
         (debugging-jsaint
          *jsaint*
          "~\n ~a: Solved original problem." (jsaint-title *jsaint*))
         (set! done? #t))
        ((in? failure-signal (jsaint-jtre *jsaint*))
         (debugging-jsaint
          *jsaint*
          "\n ~a: Failed on original problem."
          (jsaint-title *jsaint*))
         (set-jsaint-solution! *jsaint* ':failed-problem)
         (set! done? #t))
        ((null? (jsaint-agenda *jsaint*))
         (debugging-jsaint
          *jsaint* "~% ~a: Agenda empty."
          (jsaint-title *jsaint*))
         (set-jsaint-solution! *jsaint* ':failed-empty)
         (set! done? #t))
        (else
         (let ((sub (car (jsaint-agenda *jsaint*))))
           (set-jsaint-agenda! *jsaint* (cdr (jsaint-agenda *jsaint*)))
           (process-subproblem (cdr sub))))))))))

(define (process-subproblem item)
  (define jtre (jsaint-jtre *jsaint*))
  (debugging-jsaint *jsaint* "\n  Trying to solve ~a." item)
  (open-subproblem item)
  (cond
   ((not (null? (fetch-solution item *jsaint*)))
    ;; Bookkeeping is done by pdis rules
    (debugging-jsaint *jsaint* "\n    ..already solved.")
    #t)
   ((ormap (lambda (f) (in? f jtre)) ;; Already expanded
                (fetch `(and-subgoals ,item ?subproblems) jtre))
    (debugging-jsaint *jsaint* "~\n   ..already expanded.")
    #t)
   (else
    (let ((suggestions '()))
      (for ([suggestion (fetch `(suggest-for ,item ?operator) jtre)])
           (when (in? suggestion jtre)
             (queue-problem `(try ,(third suggestion)) item)
             (push! `(try ,(third suggestion)) suggestions)))
      ;; Presume extra subgoals don't come along.
      (assert! `(or-subgoals ,item ,suggestions) ':or-subgoals jtre)
      (run-rules jtre)))))

(define (open-subproblem item)
  (define jtre (jsaint-jtre *jsaint*))
  (assert! `(expanded ,item) ':expand-agenda-item jtre)
  (assume! `(open ,item) ':expand-agenda-item jtre)
  ;; Look for quick win, extra consequences.
  (run-rules jtre))

;;;; Queuing problems
;; Queue entries take the form (<difficulty> . <subproblem>)
;; Difficulty estimates are based on the form of the subproblem
;; alone, since there could be multiple parents for a subproblem.

(define (queue-problem problem parent)
  (define entry (cons (estimate-difficulty problem) problem))
  (debugging-jsaint *jsaint* "\n   Queueing ~a, difficulty = ~a"
                    problem (car entry))
  (set-jsaint-agenda!
   *jsaint*
   (insert-in-order
    entry
    (jsaint-agenda *jsaint*)
    (lambda (a b) (< (car a) (car b))))))

(define (insert-in-order entry lst order)
  (cond ((null? lst) (list entry))
        ((order entry (car lst))
         (cons entry lst))
        (else
         (cons (car lst) (insert-in-order entry (cdr lst) order)))))

(define (estimate-difficulty problem)
  (+ (max-depth problem) (count-symbols problem)))

(define (count-symbols pr)
  (cond ((null? pr) 0)
        ((list? pr)
         (foldl + 0 (map count-symbols pr)))
        (else 1)))

(define (max-depth pr)
  (cond ((not (list? pr)) 1)
        (else (+ 1 (foldl max 0 (map max-depth pr))))))

;;;; Auxiliary routines

(define (fetch-solution problem [js *jsaint*])
  (with-jsaint
   js
   (with-handlers
    ([(lambda (x) (not (exn? x)))
      (lambda (x) x)])
    (let ((jtre (jsaint-jtre *jsaint*)))
      (for ([solution (fetch `(solution-of ,problem ?answer) jtre)])
           (when (in? solution jtre)
             (raise (third solution))))
      '()))))

(define (jsaint-contradiction-handler contradictions jtms)
  (ask-user-handler contradictions jtms)) ;; default

 ;;;; Helpers for operator definition

 (define (simplifying-form-of alg-goal)
  ;; Run simplifier on subgoals, just in case.
  (cond ((null? alg-goal) '())
        ((not (pair? alg-goal)) alg-goal)
        ((eq? (car alg-goal) 'integral) ;; simplify as needed
         `(integral (:eval (simplify ,(quotize (cadr alg-goal))))
           ,(caddr alg-goal)))
        (else (cons (simplifying-form-of (car alg-goal))
                    (simplifying-form-of (cdr alg-goal))))))

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

;;;; Interrogatives

;;; SHOW-PROBLEM highlights the assertions relevant to
;;; the given problem.

(define (show-problem pr [js *jsaint*])
  (with-jsaint
   js
   (printf "\n~a:: (~a)" pr (estimate-difficulty pr))
   (with-jtre
    (jsaint-jtre *jsaint*)
    (let ((stuff (fetch `(parent-of ,pr ?x ?type))))
      (cond
       ((not (null? stuff))
        (printf "\n Parent(s): ")
        (for ([p stuff])
             (if (in? p)
                 (printf "\n   ~a, ~a."
                         (third p) (fourth p))
                 (printf "\n    BUG: Should be in: ~a" p))))
       (else
        (printf "\n No parents found.")))
      (if (not (null? (fetch `(expanded ,pr)))) (printf "\n Expanded,")
          (printf "~% Not expanded,"))
      (if (not (null? (fetch `(open ,pr))))
          (if (in? `(open ,pr)) (printf " open,")
              (printf " closed,"))
          (printf " not opened,"))
      (if (in? `(relevant ,pr)) (printf " relevant.")
          (printf " not relevant."))
      (cond ((begin
               (set! stuff (fetch-solution pr))
               (not (null? stuff)))
             (printf "\n Solved, solution = ~a" stuff))
            ((and (begin
                    (set! stuff (fetch `(failed ,pr)))
                    (if (not (null? stuff))
                        (begin
                          (set! stuff (car stuff))
                          #t)
                        #f))
                  (in? stuff)) (printf "\n  Failed."))
            ((not (equal? (car pr) 'try))
             (printf "\n Neither solved nor failed."))))
    (let ((ands (fetch `(and-subgoals ,pr ?ands))))
      (when (not (null? ands)) (printf "\n And subgoals:")
            (for ([subg (third (car ands))])
                 (printf "\n   ~a" subg))
            (printf ".")))
    (let ((ors (fetch `(or-subgoals ,pr ?ors))))
      (when (not (null? ors)) (printf "\n Or subgoals:")
            (for ([subg (third (car ors))])
                 (printf "\n   ~a" subg))
            (printf "."))))))

;;;; Textual display of an AND/OR graph

(define (show-ao-graph [js *jsaint*])
  (with-jsaint
   js
   (let* ((problems (get-problems))
          (depth-table (update-ao-depth-table
                        (jsaint-problem *jsaint*)
                        0 (list (cons (jsaint-problem *jsaint*) 0))
                        (list (jsaint-problem *jsaint*)))))
     (set! depth-table
           (sort depth-table (lambda (x y) (< (cdr x) (cdr y)))))
     (for ([pair depth-table])
          (printf "\n ~a:" (cdr pair))
          (show-problem (car pair))))))

(define (update-ao-depth-table now depth depths path)
  (inc! depth)
  (for ([child (get-children now)])
   (unless (member child path) ;; Yes, can loop!
    (let ((entry (assoc child depths)))
      (unless entry
        (set! entry (cons child 0))
        (push! entry depths))
      (when (> depth (cdr entry))
        (set! depths (replace-cdr-of-entry entry depth depths))
        (set! depths (update-ao-depth-table
                      child depth depths (cons child path)))))))
  depths)

(define (replace-cdr-of-entry entry v a)
  (cond
   ((null? a) (error 'replace-cdr-of-entry "entry not found"))
   ((eq? (car entry) (caar a))
    (cons (cons (car entry) v) (cdr a)))
   (else (cons (car a) (replace-cdr-of-entry entry v (cdr a))))))

(define (get-children gp [js *jsaint*])
  (with-jsaint
   js
   (for/list ([maybe-kid (fetch `(parent-of ?x ,gp ?type)
                                (jsaint-jtre *jsaint*))]
              #:when (in? maybe-kid (jsaint-jtre *jsaint*)))
             (cadr maybe-kid))))

(define (get-problems [js *jsaint*])
  (with-jsaint
   js
   (map cadr (fetch '(expanded ?x) (jsaint-jtre *jsaint*)))))

;;;; Debugging

(define (try-jsaint problem [title "JSAINT Test"] #:debugging-all (debugging-all #f))
  (solve-integral problem #:debugging #t #:title title #:debugging-all debugging-all))

(define problem1 '(integrate (integral 1 x)))
(define problem2 '(integrate (integral (+ x 5) x)))
(define problem3 '(integrate (integral (* 46 (log x %e)) x)))
(define problem4 '(integrate
                   (integral (+ 0.63
                                (* 3.2 (sin (* 1.7 x)))
                                (* 4 (expt %e (* 2 x)))) x)))
(define problem5 '(integrate (integral (cos x) x)))
(define problem6 '(integrate (integral (+ (* 3 x) (cos x)) x)))

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

(define (jsaint-operators)

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
  :test (not (null? ?trest))
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

(defintegration sin-x-integral
  (integral (sin ?var) ?var)
  :result (- (cos ?var)))

(defintegration cos-x-integral
  (integral (cos ?var) ?var)
  :result (sin ?var))

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
