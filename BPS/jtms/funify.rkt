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

(define (pattern-free-variables pattern)
  #f)

(define (generate-match-body pattern vars extra-test)
  'TODO)

