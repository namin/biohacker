#lang racket
(define (generate-explanation template)
  (cond
    [(equal? template 'addition)
     "The synthesizer chose the addition template because the problem required adding two numbers."]
    [(equal? template 'subtraction)
     "The synthesizer chose the subtraction template because the problem required subtracting two numbers."]
    [else "The synthesizer could not find a suitable template for the given problem."]))
(define (synthesize-program template problem score context intermediate)
  (fresh (t positive-feedback explanation)
    (== t (cadr (assoc template templates)))
    (== positive-feedback (caddr (assoc template templates)))
    (== score (evaluate problem t positive-feedback context))
    (== intermediate (cadddr (assoc template templates)))
    (== explanation (generate-explanation template))
    (conde
      [(== template 'addition)
       (== problem "Write a program that adds two numbers.")]
      [(== template 'subtraction)
       (== problem "Write a program that subtracts two numbers.")])))
(define (synthesize-api req)
  (define problem (extract-binding/single 'problem (request-bindings req)))
  (define context (extract-binding/single 'context (request-bindings req)))
  (define intermediate-template (extract-binding/single 'intermediate (request-bindings req)))

  (define generated-program-and-explanation
    (run 1 (template explanation)
         (fresh (score)
           (synthesize-program template problem score context intermediate-template)
           (== score 2)))) ; Choose the template with the highest score

  (define generated-program (caar generated-program-and-explanation))
  (define explanation (cadar generated-program-and-explanation))

  (response/json
    (hash "generated_program" generated-program
          "explanation" explanation)))


;curl -X POST "http://localhost:8080/synthesize" \
;     -d "problem=Write a program that adds two numbers." \
;     -d "context=arithmetic" \
;     -d "intermediate=add-intermediate"