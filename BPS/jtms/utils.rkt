#lang racket

(provide (all-defined-out))

(define-syntax-rule (push! val lst) ;; pushes val to list lst at first pos
  (set! lst (cons val lst)))
(define-syntax-rule (pop! lst) ;; pops the first position
  (cond
    ((null? lst) '())
    (else (let ((popped (car lst))) (set! lst (rest lst)) popped))))

(define-syntax-rule (inc! x)
  (begin
    (set! x (+ x 1))
    x))

(define (sublis d x)
  (cond ((null? x) '())
        ((pair? x)
         (cons (sublis d (car x))
               (sublis d (cdr x))))
        (else
         (let ((av (assoc x d)))
           (if av
               (cdr av)
               x)))))

(define (atom? x)
  (not (pair? x)))

(define (subst new old x)
  (cond ((null? x) '())
        ((pair? x)
         (cons (subst new old (car x))
               (subst new old (cdr x))))
        ((eq? old x) new)
        (else x)))

(define (cadr-if x)
  (and x (cadr x)))

