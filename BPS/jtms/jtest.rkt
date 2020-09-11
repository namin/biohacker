#lang racket

(require "jtre.rkt")
(provide (all-defined-out))

(in-jtre (create-jtre "Test One" #:debugging #t))
(referent '(foo 1) #t)
(fetch '(foo 1))
(in? '(foo a))
(run-rules)
(uassume! '(foo a) ':user)
(in? '(foo a))
