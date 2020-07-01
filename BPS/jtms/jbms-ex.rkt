#lang

(require "jbms.rkt")

(oplus (interval 0.5 0.5) (interval 0.3 0.5))

(oplus (interval 0 1) (interval 0 1))

(oplus (interval 0 1) (interval 0.3 0.5)) ;; does not make sense?

(oplus (interval 0.01 0.99) (interval 0.3 0.5)) ;; consistent

(oplus (interval 0.5 1) (interval 0.5 1)) ;; div by zero :(

(define j (create-jbms "hello" #:debugging #t))
(define na (tms-create-node j 'a #:belief (interval 0.1 0.6)))
(define nb (tms-create-node j 'b #:belief (interval 0.2 0.5)))
(define nf (tms-create-node j 'b #:belief (interval 0.3 0.4)))
(justify-node 'j1 nf (list na nb))
(why-node nf)
(enable-assumption na) ;; still have to attacth the belief for now
(enable-assumption nb)
(why-node nf)
(tms-node-belief nf) ;; TODO: need to updateo
(tms-node-belief na) ;; TODO: need to updateo
(tms-node-belief nb) ;; TODO: need to updateo
(combine-beliefs nf na nb)
