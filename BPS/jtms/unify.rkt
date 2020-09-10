#lang racket

(provide (all-defined-out))

(define (variable? x)
  (and (symbol? x)
       (char=? #\? (string-ref (symbol->string x) 0))))

(define (unify a b [bindings '()])
  (cond
   ((equal? a b) bindings)
   ((variable? a) (unify-variable a b bindings))
   ((variable? b) (unify-variable b a bindings))
   ((or (not (pair? a)) (not (pair? b))) ':fail)
   (else
    (let ((bindings (unify (car a) (car b) bindings)))
      (if (eq? ':fail bindings)
          ':fail
          (unify (cdr a) (cdr b) bindings))))))

(define (unify-variable var exp bindings)
  (let ((val (assoc var bindings)))
    (cond
     (val (unify (cdr val) exp bindings))
     ((free-in? var exp bindings) (cons (cons var exp) bindings))
     (else ':fail))))

(define (free-in? var exp bindings)
  ;; Returns #f if var occurs in exp, assuming bindings.
  (cond
   ((null? exp) #t)
   ((equal? var exp) #f)
   ((variable? exp)
    (let ((val (assoc exp bindings)))
      (if val
          (free-in? var (cdr val) bindings)
          #t)))
   ((not (pair? exp)) #t)
   ((free-in? var (car exp) bindings)
    (free-in? var (cdr exp) bindings))
   (else #f)))
