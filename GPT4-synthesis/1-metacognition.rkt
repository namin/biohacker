#lang racket
#lang racket
(require minikanren)

; Define the program templates
(define templates
  '((addition (lambda (x y) (+ x y)))
    (subtraction (lambda (x y) (- x y)))))

; Define the evaluation function
(define (evaluate problem template)
  (cond
    [(equal? problem "Write a program that adds two numbers.")
     (if (equal? template (cdr (assoc 'addition templates))) 1 0)]
    [(equal? problem "Write a program that subtracts two numbers.")
     (if (equal? template (cdr (assoc 'subtraction templates))) 1 0)]
    [else 0]))

; Define the synthesizer
(define (synthesize-program template problem score)
  (fresh (t)
    (== t (cdr (assoc template templates)))
    (== score (evaluate problem t))
    (conde
      [(== template 'addition)
       (== problem "Write a program that adds two numbers.")]
      [(== template 'subtraction)
       (== problem "Write a program that subtracts two numbers.")])))

; Example usage
(define problem "Write a program that adds two numbers.")
(define generated-program
  (run 1 (template)
       (fresh (score)
         (synthesize-program template problem score)
         (== score 1)))) ; Choose the template with the highest score

(display "Generated program: ")
(display generated-program)
(newline)