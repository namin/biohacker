#lang racket
(require "ltms.rkt")
(define (ex1)
   (define ltms_ (create-ltms "Simple Example" #:debugging #t))
  (define-values (x y z r)
    (values
    (tms-create-node ltms_ "x" #:assumptionp #t)
    (tms-create-node ltms_ "y" )
    (tms-create-node ltms_ "z" )
    (tms-create-node ltms_ "r" )
    ))
  (add-formula ltms_ `(:OR ,x ,y))
  (add-formula ltms_ `(:OR (:NOT ,y) ,z))
  (add-formula ltms_ `(:OR (:NOT ,z) ,r))
  (enable-assumption x ':FALSE)
  (explain-node r)
  )
  
    
