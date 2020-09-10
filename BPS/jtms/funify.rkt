#lang racket

(require "utils.rkt")
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

;;;; Open-coding unification

(define (generate-match-body0 pattern vars extra-test bound-vars)
  (let ((structure-tests '())
        (var-alist '())
        (equal-tests #f)
        (binding-specs '()))
    (for
     [(test (generate-unify-tests pattern vars '() 'p))]
     (cond ((variable? (car test))
            ;; test looks like (?x (nth p) (nth p) ...)
            (set! equal-tests
                  (append (generate-pairwise-tests (cdr test))
                          equal-tests))
            (when extra-test
                (push! (cons (car test) (car (last test)))
                       var-alist))
            (push! (car (last test)) binding-specs))
           (else (push! test structure-tests))))
    (set! extra-test (sublis var-alist extra-test))
    (when (pattern-free-variables0 extra-test bound-vars)
      (error 'generate-match-body0 (format "Rule test includes free variable: ~a"
                                          extra-test)))
    (values (append structure-tests equal-tests
                    (if extra-test (list extra-test) '()))
            binding-specs)))

(define (generate-pairwise-tests tests)
  (cond
   ((or (null? tests) (null? (cdr tests))) '())
   (else (cons (list 'equal? (car tests) (cadr tests))
               (generate-pairwise-tests (cdr tests))))))

(define (generate-unify-tests pattern vars tests path)
  'TODO)
