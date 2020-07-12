  #lang racket

(require "bms.rkt")
(require "logic.rkt")

  (oplus (interval 0.5 0.5) (interval 0.3 0.5))

  (oplus (interval 0 1) (interval 0 1))

  (oplus (interval 0 1) (interval 0.3 0.5)) ;; [0 1] is like the neutral element: 0 means no support, 1 means not true

  (oplus (interval 0.01 0.99) (interval 0.3 0.5)) ;; consistent

  (oplus (interval 0.5 1) (interval 0.5 1)) ;; div by zero but not following invariant

  (oplus (interval 0 0) (interval 0.3 0.6)) ;; [0 0] is like 1

(define (getinfo . lst) ;; prints interval [s s^] for all nodes in lst
  (map tms-node-belief lst))

(define (ex3)
  (define j (create-jbms "Simple" #:debugging #f))
  (define na (tms-create-node j 'a #:belief (interval 0.5 0)))
  (define nb (tms-create-node j 'b #:belief (interval 0.7 0)))
  (define nc (tms-create-node j 'c #:belief (interval 0.15 0.24)))
  (define nd (tms-create-node j 'd #:belief (interval 0.4 0)))
  (displayln (getinfo na nb nc nd)) 
  (justify-node 'j1 nc (list na nb) (interval 1.0 0.0))
  (displayln (getinfo na nb nc nd))
  (justify-node 'j2 nc (list nd) (interval 0.0 0.7))
  (displayln (getinfo na nb nc nd))
  )

(define (ex4)
  (define j (create-jbms "Simple" #:debugging #t))
  (define na (tms-create-node j 'a #:belief (interval 0.5 0)))
  (define nb (tms-create-node j 'b #:belief (interval 0.7 0)))
  (define nc (tms-create-node j 'c #:belief (interval 0.4 0)))
  (displayln (getinfo na nb nc))
  (justify-node 'j1 nc (list nb) (interval 0 0.7))
  (displayln (getinfo na nb nc))
  (justify-node 'j2 nb (list na) (interval 0.55 0.22))
  (displayln (getinfo na nb nc))
  )

(define (ex5)
  (define j (create-jbms "Simple" #:debugging #t))
  (define nb (tms-create-node j 'b #:belief (interval 0.5 0)))
  (define nc (tms-create-node j 'c #:belief (interval 0.1 0)))
  (define nd (tms-create-node j 'd #:belief (interval 0.2 0)))
  (justify-node 'j1 nc (list nb) (interval 0.4 0))
  (justify-node 'j2 nd (list nc) (interval 0.6 0))
  (justify-node 'j3 nb (list nd) (interval 0.5 0))
  )
  
