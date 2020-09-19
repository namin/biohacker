#lang racket

(require "tinytms.rkt")

(define tms (make-tms "hello"))
(tms-create-node tms 'a)

(define hello-tms (make-tms "hello again" #:debugging #t))
(debugging-tms hello-tms "hello from tms")

(define (ex1)
  (define *tms* (make-tms "Simple Example" #:debugging #t))
  (define na (tms-create-node *tms* 'a #:assumption? #t))
  (define nb (tms-create-node *tms* 'b #:assumption? #t))
  (define nc (tms-create-node *tms* 'c #:assumption? #t))
  (define nd (tms-create-node *tms* 'd #:assumption? #t))
  (define ne (tms-create-node *tms* 'e #:assumption? #t))
  (define nf (tms-create-node *tms* 'f #:assumption? #t))
  (define ng (tms-create-node *tms* 'g #:assumption? #t))

  (justify-node 'j1 nf (list na nb))
  (justify-node 'j2 ne (list nb nc))
  (justify-node 'j3 ng (list na ne))
  (justify-node 'j4 ng (list nd ne))

  (enable-assumption na)
  (enable-assumption nb)
  (enable-assumption nc)
  (enable-assumption nd)

  (why-node ng)
  (why-node nf)

  (define contra (tms-create-node *tms* 'Loser #:contradictory? #t))
  (justify-node 'j5 contra (list ne nf))

  )

(define (ex3)
  (define *tms* (make-tms "Multiple support example"))
  (define assumption-a (tms-create-node *tms* 'A #:assumption? #t))
  (define assumption-c (tms-create-node *tms* 'C #:assumption? #t))
  (define assumption-e (tms-create-node *tms* 'E #:assumption? #t))
  (define node-h (tms-create-node *tms* 'h))
  (enable-assumption assumption-a)
  (enable-assumption assumption-c)
  (enable-assumption assumption-e)
  (justify-node 'R1 node-h (list assumption-c assumption-e))
  (define node-g (tms-create-node *tms* 'g))
  (justify-node 'R2 node-g (list assumption-a assumption-c))
  (define contradiction (tms-create-node *tms*
					 'CONTRADICTION #:contradictory? #t))
  (justify-node 'R3 contradiction (list node-g)))

(ex1)
(ex3)
