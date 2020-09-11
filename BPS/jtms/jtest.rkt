#lang racket

(require "jtre.rkt")
(provide (all-defined-out))

(in-jtre (create-jtre "Test One"))
(referent '(foo 1) #t)
(fetch '(foo 1))
