#lang racket

(require "utils.rkt")
(require "unify.rkt")
(provide (all-defined-out))

(define (quotize pattern)
  (cond
   ((null? pattern) ''())
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
        (equal-tests '())
        (binding-specs '()))
    (for
     [(test (generate-unify-tests pattern vars '() 'p))]
     (cond ((variable? (car test))
            ;; test looks like (?x (nth p) (nth p) ...)
            (set! equal-tests
                  (append (generate-pairwise-tests (cdr test))
                          equal-tests))
            (when extra-test
                (push! (cons (car test) (last test))
                       var-alist))
            (push! (last test) binding-specs))
           (else (push! test structure-tests))))
    (set! extra-test (sublis var-alist extra-test))
    (when (not (null? (pattern-free-variables0 extra-test bound-vars)))
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

;;; Generate a list of explicit tests for matching
;;; the given pattern. Assumes that the pattern
;;;    to be tested will be in variable "P".
;;; Tests are returned in backward order.
;;; (generate-unify-tests '(foo ?x) nil nil 'P)
;;;     returns:    '((NULL? (CDR (CDR P)))
;;;                   (EQUAL? ?X (CAR (CDR P)))
;;;                   (PAIR? (CDR P))
;;;                   (EQUAL? (QUOTE FOO) (CAR P))
;;;                   (PAIR? P))

(define (generate-unify-tests pattern vars tests path)
  (cond ((null? pattern)
                ;this is the end
         (cons `(null? ,path) tests))
        ((member pattern vars)
         ;; must see if the pattern has been bound elsewhere,
         ;; and if it has, test to see if the element here is
         ;; consistent with that earlier binding.
         (let ((previous (assoc pattern tests)))
           (cond (previous ;add this position to test it
                  ;;(push! path (cdr previous))
                  (add-assoc-value tests pattern path)
                  tests)
                 (else (cons (list pattern path) tests)))))
        ;; if variable, it must be bound so test
        ;; against the current value.
        ((variable? pattern) (cons `(equal? ,pattern ,path)
                                   tests))
        ;; if not a list, then see if equal
        ((number? pattern)
         (cons `(and (number? ,path) (= ,pattern ,path))
               tests))
        ((atom? pattern) (cons `(equal? ',pattern ,path) tests))
        ;; recurse on a list
        (else (generate-unify-tests (cdr pattern) vars
                 (generate-unify-tests (car pattern) vars
                                       ;avoid lisp errors
                                       (cons `(pair? ,path)
                                             tests)
                                            ;extend the path
                                       (list 'car path))
                 ;extend path in other direction
                 (list 'cdr path)))))

(define (add-assoc-value tests pattern path)
  (cond
   ((null? tests) '())
   ((equal? (caar tests) pattern)
    (cons (cons (caar tests)
                (cons path (cdar tests)))
          (cdr tests)))
   (else
    (cons (car tests) (add-assoc-value (cdr tests) pattern path)))))
