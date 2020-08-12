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

(define (test-and)
  (define ltms_ (create-ltms "LTMS" #:debugging #t))
  (define-values (ok z x y)
    (values
     (tms-create-node ltms_ "ok" #:assumptionp #t)
     (tms-create-node ltms_ "z" #:assumptionp #t)
     (tms-create-node ltms_ "x" #:assumptionp #t)
     (tms-create-node ltms_ "y" #:assumptionp #t)))
  (add-formula ltms_ `(:IMPLIES ,ok (:IFF ,z (:AND ,x ,y))))
  (pretty-print-clauses ltms_)
  )
  
(define (test1)
  (define ltms_ (create-ltms "TEST1" #:debugging #t))
  (define-values (x y)
    (values
     (tms-create-node ltms_ "x")
     (tms-create-node ltms_ "y"))
    )
  (add-formula ltms_ `(:OR ,x ,y))
  (add-formula ltms_ `(:OR ,x (:NOT ,y)))
  (when (true-node? x)
    (error "TEST1 failed")))
  
(define (test-and1)
  (define ltms_ (create-ltms "LTMS"))
  (define-values
    (ok z=1 z=0 x=1 x=0 y=1 y=0)
    (values
     (tms-create-node ltms_ "ok" #:assumptionp #t)
	 (tms-create-node ltms_ "z=1" #:assumptionp #t)
	 (tms-create-node ltms_ "z=0" #:assumptionp #t)
	 (tms-create-node ltms_ "x=1" #:assumptionp #t)
	 (tms-create-node ltms_ "x=0" #:assumptionp #t)
	 (tms-create-node ltms_ "y=1" #:assumptionp #t)
	 (tms-create-node ltms_ "y=0" #:assumptionp #t)
	))
  (add-formula ltms_ `(:TAXONOMY ,z=1 ,z=0))
  (add-formula ltms_ `(:TAXONOMY ,x=1 ,x=0))
  (add-formula ltms_ `(:TAXONOMY ,y=1 ,y=0))
  
;  (add-formula *ltms* `(:implies ,ok (:iff ,z=1 (:and ,x=1 ,y=1))))
  (add-formula ltms_ `(:IFF ,z=1 (:AND ,x=1 ,y=1)))

  (pretty-print-clauses ltms_))


(define (text-xor)
  (let* ((ltms_ (create-ltms "LTMS"))
	(ok (tms-create-node ltms_ "ok" #:assumptionp #t))
	(z=1 (tms-create-node ltms_ "z=1" #:assumptionp #t))
	(z=0 (tms-create-node ltms_ "z=0" #:assumptionp #t))
	(x=1 (tms-create-node ltms_ "x=1" #:assumptionp #t))
	(x=0 (tms-create-node ltms_ "x=0" #:assumptionp #t))
	(y=1 (tms-create-node ltms_ "y=1" #:assumptionp #t))
	(y=0 (tms-create-node ltms_ "y=0" #:assumptionp #t))
	)
  (add-formula ltms_ `(:TAXONOMY ,z=1 ,z=0))
  (add-formula ltms_ `(:TAXONOMY ,x=1 ,x=0))
  (add-formula ltms_ `(:TAXONOMY ,y=1 ,y=0))
  
;  (add-formula ltms_ `(:implies ,ok (:iff ,z=1 (:and ,x=1 ,y=1))))
;  (add-formula ltms_ `(:iff ,z=1 (:taxonomy ,x=1 ,y=1)))


  (pretty-print-clauses ltms_)))
