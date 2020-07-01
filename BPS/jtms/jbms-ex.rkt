#lang

(require "jbms.rkt")

(oplus (interval 0.5 0.5) (interval 0.3 0.5))

(oplus (interval 0 1) (interval 0 1))

(oplus (interval 0 1) (interval 0.3 0.5)) ;; [0 1] is like the neutral element: 0 means no support, 1 means not true

(oplus (interval 0.01 0.99) (interval 0.3 0.5)) ;; consistent

(oplus (interval 0.5 1) (interval 0.5 1)) ;; div by zero but not following invariant

(oplus (interval 0 0) (interval 0.3 0.6)) ;; [0 0] is like 1


(define ex1
(define j (create-jbms "hello" #:debugging #t))
(define na (tms-create-node j 'a #:belief (interval 0.1 0.6)))
(define nb (tms-create-node j 'b #:belief (interval 0.2 0.5)))
(define nf (tms-create-node j 'f #:belief (interval 0.3 0.4)))
(justify-node 'j1 nf (list na nb))
(why-node nf)
(enable-assumption na) ;; still have to attacth the belief for now
(enable-assumption nb)
(why-node nf)
(tms-node-belief nf)
(tms-node-belief na)
(tms-node-belief nb)
(combine-beliefs nf na nb)
)

(define ex2
  (define j (create-jbms "hello" #:debugging #t))
  (define na (tms-create-node j 'a #:belief (interval 0.1 0.6)))
  (define nb (tms-create-node j 'b #:belief (interval 0.2 0.5)))
  (define nc (tms-create-node j 'c #:belief (interval 0.1 0.6)))
  (define nd (tms-create-node j 'd #:belief (interval 0.2 0.5)))
  (define ne (tms-create-node j 'e #:belief (interval 0.2 0.5)))
  (justify-node 'j1 nb (list na))
  (justify-node 'j2 nb (list nd))
  (justify-node 'j2 nc (list nb))
  (justify-node 'j2 nd (list nc))
  (justify-node 'j2 nd (list ne))
  (enable-assumption na) ;; still have to attacth the belief for now

  (tms-node-belief na)
  (tms-node-belief nb)
  (tms-node-belief nc)
  (tms-node-belief nd)

  (enable-assumption ne)

  (tms-node-belief nb) ;; B is not more supported than it should. See discussion on cycles.
)
