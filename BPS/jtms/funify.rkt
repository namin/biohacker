#lang racket

(require "unify.rkt")
(provide (all-defined-out))

(define (quotize pattern)
  (cond
   ((null? pattern) '())
   ((variable? pattern) pattern)
   ((not (pair? pattern)) (list 'quote pattern))
   ((eq? (car pattern) ':eval) (cadr pattern))
   (else `(cons ,(quotize (car pattern))
                ,(quotize (cdr pattern))))))

(define (generate-match-body pattern vars extra-test)
  'TODO)

;;; Finding free variables in a pattern

(define (pattern-free-variables0 pattern bound-vars)
  (pattern-free-vars1 pattern '() bound-vars))

(define (pattern-free-vars1 pattern vars bound-vars)
  (cond
   ((null? pattern) vars)
   ((variable? pattern)
    (if (or (member pattern vars)
            (member pattern bound-vars))
        vars
        (cons pattern vars)))
   ((not (pair? pattern)) vars)
   (else (pattern-free-vars1
          (cdr pattern)
          (pattern-free-vars1 (car pattern) vars bound-vars)
          bound-vars))))
