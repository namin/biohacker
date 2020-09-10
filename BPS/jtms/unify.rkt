#lang racket

(provide (all-defined-out))

(define (variable? x)
  (and (symbol? x)
       (char=? #\? (string-ref (symbol->string x) 0))))
