#lang racket

(require "match.rkt")
(provide (all-defined-out))

(define *simplify-cache* (make-hash))

(define (simplify exp)
  (or (hash-ref *simplify-cache* exp #f)
      (let ((r (simplify-it exp *algebra-rules*)))
        (hash-set! *simplify-cache* exp r)
        r)))

(define (clear-simplify-cache)
  (hash-clear! *simplify-cache*))

(define (simplify-it exp rules)
  (define result
    (try-matcher-rules
     (if (list? exp) (map simplify exp)
	 exp)
     rules))
  (if (equal? result exp) result
      (simplify-it result rules)))

(define (try-matcher-rules exp rules)
  (with-handlers
   ([(lambda (x) (not (exn? x)))
     (lambda (x) x)])
   (for ([rule rules])
        (let ((bindings (matcher (rule-pattern rule) exp '())))
          ;;      FOR DEBUGGING
          ;;      (unless (eq bindings :FAIL)
          ;;      (format t "~% Matched ~A on ~A." exp rule)
          ;;      (dolist (binding bindings)
          ;;      (format t "~% ~A: ~A." (car binding)
          ;;                (var-value (list '? (car binding)) bindings))))
          (unless (eq? bindings ':fail)
	    (when (check-predicate (rule-predicate rule) bindings)
	      (raise (substitute-in (rule-skeleton rule) bindings))))))
   ;; Return the original expression by default
   exp))

(define (check-predicate proc bindings)
  (if proc
      (eval (substitute-in proc bindings))
       #t))

(define (rule-pattern rule) (car rule))
(define (rule-predicate  rule) (cadr rule))
(define (rule-skeleton rule) (caddr rule))

;;;; Algebra utilities

(define (alg< e1 e2) ;; Sort predicate for algebraic expressions
  (cond ((~equal? e1 e2) #f)
	((pair? e1)
	 (if (pair? e2)
	     (if (~equal? (car e1) (car e2))
		 (alg< (cdr e1) (cdr e2))
		 (alg< (car e1) (car e2)))
	     #f))
	((pair? e2) #t)
	((symbol? e1)
	 (if (symbol? e2)
	     (string<? (symbol->string e1) (symbol->string e2))
             #f))
	((symbol? e2) #t)
	((and (number? e1) (number? e2)) (< e1 e2))
	(else (error 'alg< (format "alg< cannot compare these: ~a, ~a." e1 e2)))))

(define (alg= e1 e2) (not (or (alg< e1 e2) (alg< e2 e1))))
 
(define (sorted? list pred)
  (cond ((or (null? list) (null? (cdr list))) #t)
	((pred (cadr list) (car list)) #f)
	(else (sorted? (cdr list) pred))))

(define (+/*? exp) (or (eq? exp '+) (eq? exp '*)))

(define (same-constant? exp constant)
  (and (number? exp)
       (if (real? exp) (~equal? exp constant)
	   (= exp constant))))

(define (zero? exp) (same-constant? exp 0))
(define (one? exp) (same-constant? exp 1))

;;;; Extra utilities

(define (occurs-in? exp1 exp2) 
  (cond ((equal? exp1 exp2) #t)
	((null? exp2) #f)
	((pair? exp2)
	 (or (occurs-in? exp1 (car exp2))
	     (occurs-in? exp1 (cdr exp2))))
        (else #f)))


;;;; Rules for algebraic simplification

(define *algebra-rules* `(
;; Flush degenerate cases
(((? op +/*?) (? e)) '() (? e))
((+ (? zero zero?) (?? e)) '() (+ (?? e)))
((- (? zero zero?) (? e)) '() (- (? e)))
((- (? e) (? zero zero?)) '() (? e))
((- (? e) (? e)) '() 0)
((* (? one one?) (?? e)) '() (* (?? e)))
((* (? zero zero?) (?? e)) '() 0)
((expt (? e) (? zero zero?)) '() 1)
((expt (? e) (? one one?)) '() (? e))
((log (? one one?) (? base)) '() 0)
((log (? base) (? base)) '() 1)
((log (expt (? base) (? val)) (? base)) '() (? val))
((expt (? base) (log (? val) (? base))) '() (? val))
;; Equivalences involving powers
((* (? e) (? e)) '() (sqr (? e)))
((expt (? e) (? two ,(lambda (exp) (same-constant? exp 2))))
 '() (sqr (? e)))
((sqrt (sqr (? e))) '() (abs (? e)))
((sqr (sqrt (? e))) '() (? e))

;; Combine numerical constants
(((? op +/*?) (? e1 number?) (? e2 number?) (?? e3))
 '()
 ((? op) (:eval ((? op) (? e1) (? e2))) (?? e3)))
((- (- (? e1) (? e2))) '() (- (? e2) (? e1))) ;; strip
((- (? e1 number?) (? e2 number?)) '() (:eval (- (? e1) (? e2))))
((- (? e1 number?)) '() (:eval (- (? e1))))
((- (? e1) (? e2 number?)) '() (+ (- (? e2)) (? e1)))
((- (? e1 number?) (+ (? e2 number?) (?? e3)))
 '() (- (:eval (- (? e1) (? e2))) (+ (?? e3))))
((- (? e1 number?) (- (? e2 number?) (?? e3)))
 '() (+ (:eval (- (? e1) (? e2))) (?? e3)))
((+ (? e1 number?) (- (? e2 number?) (?? e3)))
 '() (- (:eval (+ (? e1) (? e2))) (?? e3)))
((sqr (? e1 number?)) '() (:eval (* (? e1)  (? e1))))
((sqrt (? e1 number?)) '() (:eval (sqrt (? e1))))
((expt (? e1 number?) (? e2 number?)) '() (:eval (expt (? e1) (? e2))))
((/ (? e1 number?) (? e2 number?)) '() (:eval (/ (? e1) (? e2))))
((* (? e1 number?) (/ (? e2) (? e3 number?))) '()
 (* (:eval (/ (? e1) (? e3))) (? e2)))
((/ (* (? e1 number?) (? e2)) (? e3 number?)) '()
 (* (:eval (/ (? e1 number?) (? e3 number?))) (? e2)))
((* (?? pre) (- (? term)) (?? post)) '()
 (* (?? pre) (* -1 (? term)) (?? post)))
((abs (? e number?)) '() (:eval (abs (? e))))
((log (? x number?) (? base number?)) 
 '() (:eval (/ (log (? x)) (log (? base)))))
;; Flatten +,*
(((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
 '()
 ((? op) (?? e1) (?? e2) (?? e3)))
;; Combine like terms
((+ (?? pre) (* (? f1) (? thing)) (* (? f2) (? thing)) (?? post)) '()
 (+ (?? pre) (* (* (? f1) (? f2)) (? thing)) (?? post)))
((+ (?? pre) (* (? f1) (? thing)) (?? mid) (? thing) (?? post)) '()
 (+ (?? pre) (* (+ 1 (? f1)) (? thing)) (?? mid) (?? post)))
;; Canonicalize +,*
(((? op +/*?) (?? terms))
 (not (sorted? (quote (? terms)) alg<))
 ((? op) (:splice (sort (quote (? terms)) alg<))))))
