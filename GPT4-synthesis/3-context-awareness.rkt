#lang racket
#lang racket
(require minikanren)

; Define the program templates with contexts
(define templates
  '((addition (lambda (x y) (+ x y)) 0 '("math" "arithmetic"))
    (subtraction (lambda (x y) (- x y)) 0 '("math" "arithmetic"))))

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
(define (synthesize-program template problem score context)
  (fresh (t positive-feedback)
    (== t (cadr (assoc template templates)))
    (== positive-feedback (caddr (assoc template templates)))
    (== score (evaluate problem t positive-feedback context))
    (conde
      [(== template 'addition)
       (== problem "Write a program that adds two numbers.")]
      [(== template 'subtraction)
       (== problem "Write a program that subtracts two numbers.")])))

; User feedback function
(define (update-feedback template)
  (let ((entry (assoc template templates)))
    (set! templates (remove entry templates))
    (set! templates (cons (list (car entry) (cadr entry) (+ 1 (caddr entry)) (cadddr entry)) templates))))

; Example usage
(define problem "Write a program that adds two numbers.")
(define context "arithmetic")
(define generated-program
  (run 1 (template)
       (fresh (score)
         (synthesize-program template problem score context)
         (== score 2)))) ; Choose the template with the highest score

(display "Generated program: ")
(display generated-program)
(newline)

; Simulate user feedback
(update-feedback 'addition)

; Synthesize another program after receiving feedback
(define generated-program-2
  (run 1 (template)
       (fresh (score)
         (synthesize-program template problem score context)
         (== score 3)))) ; Choose the template with the updated score

(display "Generated program after receiving feedback: ")
(display generated-program-2)
(newline)