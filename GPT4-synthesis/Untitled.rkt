#lang racket
(require minikanren)

(define (synthesize-program template problem)
  (conde
    [(== template '(lambda (x y) (+ x y)))
     (== problem "Write a program that adds two numbers.")]
    [(== template '(lambda (x y) (- x y)))
     (== problem "Write a program that subtracts two numbers.")]))

; Example usage
(define problem "Write a program that adds two numbers.")
(define generated-program (run 1 (template) (synthesize-program template problem)))

(display "Generated program: ")
(display generated-program)
(newline)
