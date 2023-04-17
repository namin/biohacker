#lang racket
(define knowledge-base '())
(define (learn-strategy context strategy)
  (set! knowledge-base (cons (cons context strategy) knowledge-base)))
(define (evaluate problem template positive-feedback context)
  (let ((transfer-score (or (cdr (assoc context knowledge-base)) 0)))
    (cond
      [(equal? problem "Write a program that adds two numbers.")
       (if (equal? template (cadr (assoc 'addition templates)))
           (+ 1 positive-feedback (if (member context (cadddr (assoc 'addition templates))) 1 0) transfer-score)
           0)]
      [(equal? problem "Write a program that subtracts two numbers.")
       (if (equal? template (cadr (assoc 'subtraction templates)))
           (+ 1 positive-feedback (if (member context (cadddr (assoc 'subtraction templates))) 1 0) transfer-score)
           0)]
      [else 0])))

; (learn-strategy 'arithmetic 2)

(define problem "Write a program that adds two numbers.")
(define context "math")
(define intermediate-template 'add-intermediate)

(define generated-program
  (run 1 (template)
       (fresh (score)
         (synthesize-program template problem score context intermediate-template)
         (== score 3)))) ; Choose the template with the highest score

(display "Generated program: ")
(display generated-program)
(newline)