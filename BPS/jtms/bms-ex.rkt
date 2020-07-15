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

(define (ex2) ;; paper example
  (define j (create-jbms "Simple" #:debugging #f))
  (define na (tms-create-node j 'a #:belief (interval 0.5 0)))
  (define nb (tms-create-node j 'b #:belief (interval 0.7 0)))
  (define nc (tms-create-node j 'c #:belief (interval 0 0)))
  (define nd (tms-create-node j 'd #:belief (interval 0.4 0)))
  (displayln (getinfo na nb nc nd)) 
  (justify-node 'j1 nc (list na nb) (interval 1.0 0.0))
  (displayln (getinfo na nb nc nd))
  (justify-node 'j2 nc (list nd) (interval 0.0 0.7))
  (displayln (getinfo na nb nc nd))
  )

(define (ex3) ;;Simple chains
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

(define (ex4) ;;Simple chains
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

(define (ex5) ;;infinite
  (define j (create-jbms "Simple" #:debugging #t #:tolerance 0.0001))
  (define nb (tms-create-node j 'b #:belief (interval 0.5 0)))
  (define nc (tms-create-node j 'c #:belief (interval 0.1 0)))
  (define nd (tms-create-node j 'd #:belief (interval 0.2 0)))
  (justify-node 'j1 nc (list nb) (interval 0.4 0))
  (justify-node 'j2 nd (list nc) (interval 0.6 0))
  (justify-node 'j3 nb (list nd) (interval 0.5 0))
  (display "\n")
  (displayln (getinfo nb nc nd))
  )
  
(define (ex6) ;;based on KB 
  (define j (create-jbms "KB" #:debugging #f))
  (define nab (tms-create-node j 'a-b #:belief (interval 0.5 0.5)))
  
  (define nbc (tms-create-node j 'b-c #:belief (interval 0.5 0.5)))
  
  (define nbd (tms-create-node j 'b-d #:belief (interval 0.5 0.5)))
  
  (define nac (tms-create-node j 'a-c #:belief (interval 0.5 0.5)))
  (define nde (tms-create-node j 'd-e #:belief (interval 0.5 0.5)))
  (define ngr (tms-create-node j 'graph #:belief (interval 0.5 0.5)))
  
  (define nae (tms-create-node j 'a-e #:belief (interval 0.5 0.5)))

  (justify-node 'j1 ngr (list nab nbc nbd nac nde) (interval 0.5 0.5))

  (justify-node 'j2 nae (list ngr) (interval 0 0))
  )

  
 (define (ex7) ;;based on KB 
   (define j (create-jbms "KB" #:debugging #f))
   (define njhk (tms-create-node j 'j-h-k #:belief (interval 0.5 0.5)))

   (define njk (tms-create-node j 'j-k #:belief (interval 0.5 0.5)))

   (define nkl (tms-create-node j 'k-l #:belief (interval 0.5 0.5)))

   (define njkl (tms-create-node j 'j-k-l #:belief (interval 0.5 0.5)))
   (define njl (tms-create-node j 'j-l #:belief (interval 0.5 0.5)))

   (justify-node 'j1 njk (list njhk) (interval 0.5 0.5))

   (justify-node 'j2 njkl (list njk nkl) (interval 0 0))


   (justify-node 'j3 njl (list njkl) (interval 0 0))

   )
