#lang racket

(require "jtms.rkt")

(define j (create-jtms "hello"))
(tms-create-node j 'a)

(define hello-jtms (create-jtms "hello again" #:debugging #t))
(debugging-jtms hello-jtms "hello from jtms")

(define (ex1)
  (define *jtms* (create-jtms "Simple Example" #:debugging #t))
  (define na (tms-create-node *jtms* 'a #:assumptionp #t))
  (define nb (tms-create-node *jtms* 'b #:assumptionp #t))
  (define nc (tms-create-node *jtms* 'c #:assumptionp #t))
  (define nd (tms-create-node *jtms* 'd #:assumptionp #t))
  (define ne (tms-create-node *jtms* 'e #:assumptionp #t))
  (define nf (tms-create-node *jtms* 'f #:assumptionp #t))
  (define ng (tms-create-node *jtms* 'g #:assumptionp #t))

  (justify-node 'j1 nf (list na nb))
  (justify-node 'j2 nf (list nb nc))
  (justify-node 'j3 nf (list na ne))
  (justify-node 'j4 nf (list nd ne))

  (enable-assumption na)
  (enable-assumption nb)
  (enable-assumption nc)
  (enable-assumption nd)

  (explore-network ng)
  ;;(explore-network nf)

  (define contra (tms-create-node *jtms* 'Loser #:contradictoryp #t))
  (justify-node 'j5 contra (list ne nf))

  )

(ex1)
