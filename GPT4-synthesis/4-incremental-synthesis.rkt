#lang racket
#lang racket
(require minikanren)

; Define the program templates with contexts and intermediate templates
(define templates
  '((addition (lambda (x y) (+ x y)) 0 '("math" "arithmetic") 'add-intermediate)
    (subtraction (lambda (x y) (- x y)) 0 '("math" "arithmetic") 'sub-intermediate)
    (add-intermediate (lambda (x y) (+ x y)) 0 '("math" "arithmetic"))
    (sub-intermediate (lambda (x y) (- x y)) 0 '("math" "arithmetic"))))

; Define the evaluation function
(define (evaluate problem template positive-feedback context)
  (cond
    [(equal? problem "Write a program that adds two numbers.")
     (if (equal? template (cadr (assoc 'addition templates)))
         (+ 1 positive-feedback (if (member context (cadddr (assoc 'addition templates))) 1 0))
         0)]
    [(equal? problem "Write a program that subtracts two numbers.")
     (if (equal? template (cadr (assoc 'subtraction templates)))
         (+ 1 positive-feedback (if (member context (cadddr (assoc 'subtraction templates))) 1 0))
         0)]
    [else 0]))

; Define the synthesizer
(define (synthesize-program template problem score context intermediate)
  (fresh (t positive-feedback)
    (== t (cadr (assoc template templates)))
    (== positive-feedback (caddr (assoc template templates)))
    (== score (evaluate problem t positive-feedback context))
    (== intermediate (cadddr (assoc template templates)))
    (conde
      [(== template 'addition)
       (== problem "Write a program that adds two numbers.")]
      [(== template 'subtraction)
       (== problem "Write a program that subtracts two numbers.")])))

; Example usage
(define problem "Write a program that adds two numbers.")
(define context "arithmetic")
(define intermediate-template 'add-intermediate)

(define generated-program
  (run 1 (template)
       (fresh (score)
         (synthesize-program template problem score context intermediate-template)
         (== score 2)))) ; Choose the template with the highest score

(display "Generated program: ")
(display generated-program)
(newline)