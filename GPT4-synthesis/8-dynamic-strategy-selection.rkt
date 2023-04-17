#lang racket
(define (update-knowledge-base context strategy reward)
  (let ((current-value (or (cdr (assoc context knowledge-base)) 0)))
    (set! knowledge-base
          (cons (cons context (+ current-value reward))
                (remove (assoc context knowledge-base) knowledge-base)))))


(define (synthesize-program template problem score context intermediate)
  (fresh (t positive-feedback strategy)
    (== t (cadr (assoc template templates)))
    (== positive-feedback (caddr (assoc template templates)))
    (== strategy (car (assoc template templates)))
    (== score (evaluate problem t positive-feedback context))
    (== intermediate (cadddr (assoc template templates)))
    (conde
      [(== template 'addition)
       (== problem "Write a program that adds two numbers.")]
      [(== template 'subtraction)
       (== problem "Write a program that subtracts two numbers.")])))


(define (learn-from-experience problem context intermediate-template reward)
  (let* ((generated-program-and-strategy
           (run 1 (template strategy)
                (fresh (score)
                  (synthesize-program template problem score context intermediate-template)
                  (== score 2))))
         (generated-program (caar generated-program-and-strategy))
         (strategy (cadar generated-program-and-strategy)))
    (update-knowledge-base context strategy reward)
    generated-program))

(define problem "Write a program that adds two numbers.")
(define context "arithmetic")
(define intermediate-template 'add-intermediate)

(define generated-program (learn-from-experience problem context intermediate-template 1))

(display "Generated program: ")
(display generated-program)
(newline)