#lang racket
#lang racket
(require web-server/http)
(require web-server/servlet)
(require web-server/servlet-env)
(require json)

; Include the synthesizer code here (synthesize-program, evaluate, templates, etc.)

; Define the API handler
(define (synthesize-api req)
  (define problem (extract-binding/single 'problem (request-bindings req)))
  (define context (extract-binding/single 'context (request-bindings req)))
  (define intermediate-template (extract-binding/single 'intermediate (request-bindings req)))

  (define generated-program
    (run 1 (template)
         (fresh (score)
           (synthesize-program template problem score context intermediate-template)
           (== score 2)))) ; Choose the template with the highest score

  (response/json
    (hash "generated_program" generated-program)))

; Define the API server
(define-values (dispatch generate-url)
  (dispatch-rules
   [("synthesize" . synthesize-api)]))

; Start the web server
(serve/servlet dispatch
               #:port 8080
               #:servlet-path "")