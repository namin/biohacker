#lang racket

(require "pjtms.rkt")

(define j (create-jtms "hello"))
(tms-create-node j 'a)

(define hello-jtms (create-jtms "hello again" #:debugging #t))
(debugging-jtms hello-jtms "hello from jtms")

(define (ex1)

  (define *tms* (create-jtms "Simple Example" #:debugging #t))
  (define na #f)
  (define nb #f)
  (define nc #f)
  (define nd #f)
  (define ne #f)
  (define nf #f)
  (define ng #f)
  
  (let*-values (((tms ta) (tms-create-node *tms* 'a #:assumptionp #t))
                ((tms tb) (tms-create-node tms 'b #:assumptionp #t))
                ((tms tc) (tms-create-node tms 'c #:assumptionp #t))
                ((tms td) (tms-create-node tms 'd #:assumptionp #t))
                ((tms te) (tms-create-node tms 'e #:assumptionp #t))
                ((tms tf) (tms-create-node tms 'f #:assumptionp #t))
                ((tms tg) (tms-create-node tms 'g #:assumptionp #t)))
    (set! *tms* tms)
    (set! na ta)
    (set! nb tb)
    (set! nc tc)
    (set! nd td)
    (set! ne te)
    (set! nf tf)
    (set! ng tg))
  
  (set! *tms* (justify-node *tms* 'j1 nf (list na nb)))
  (set! *tms* (justify-node *tms* 'j2 ne (list nb nc)))
  (set! *tms* (justify-node *tms* 'j3 ng (list na ne)))
  (set! *tms* (justify-node *tms* 'j4 ng (list nd ne)))

  (set! *tms* (enable-assumption *tms* na))
  (set! *tms* (enable-assumption *tms* nb))
  (set! *tms* (enable-assumption *tms* nc))
  (set! *tms* (enable-assumption *tms* nd))

  (explore-network *tms* ng)
  (explore-network *tms* nf)

  (define contra #f)
  (let-values (((tms t) (tms-create-node *tms* 'Loser #:contradictoryp #t)))
    (set! *tms* tms)
    (set! contra t))

  (set! *tms* (justify-node *tms* 'j5 contra (list ne nf))))

(define (ex3)

  (define *tms* (create-jtms "Multiple support example"))
  (define assumption-a
    (let-values (((tms t) (tms-create-node *tms* 'A #:assumptionp #t)))
      (set! *tms* tms) t))
  (define assumption-c
    (let-values (((tms t) (tms-create-node *tms* 'C #:assumptionp #t)))
      (set! *tms* tms) t))
  (define assumption-e
    (let-values (((tms t) (tms-create-node *tms* 'E #:assumptionp #t)))
      (set! *tms* tms) t))
  (define node-h
    (let-values (((tms t) (tms-create-node *tms* 'h)))
      (set! *tms* tms) t))
  (set! *tms* (enable-assumption *tms* assumption-a))
  (set! *tms* (enable-assumption *tms* assumption-c))
  (set! *tms* (enable-assumption *tms* assumption-e))
  (set! *tms* (justify-node *tms* 'R1 node-h (list assumption-c assumption-e)))
  (define node-g
    (let-values (((tms t) (tms-create-node *tms* 'g)))
      (set! *tms* tms) t))
  (set! *tms* (justify-node *tms* 'R2 node-g (list assumption-a assumption-c)))
  (define contradiction
    (let-values (((tms t) (tms-create-node *tms* 'CONTRADICTION #:contradictoryp #t)))
      (set! *tms* tms) t))

  (set! *tms* (justify-node *tms* 'R3 contradiction (list node-g)))
  )

(ex1)
(ex3)
