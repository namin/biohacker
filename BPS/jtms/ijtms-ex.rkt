#lang racket

(require "ijtms.rkt")
(require rackunit)

(define (ex1-b-ok)
  (define *tms* (create-jtms "Simple Example" #:debugging #t))

  (set! *tms* (tms-create-node *tms* 'a #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'b #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'c #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'd #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'e #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'f #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'g #:assumptionp #t))

  (set! *tms* (if-ok-justify-node *tms* 'j1 'f (list 'a 'b)))
  (set! *tms* (if-ok-justify-node *tms* 'j2 'e (list 'b 'c)))
  (set! *tms* (if-ok-justify-node *tms* 'j3 'g (list 'a 'e)))
  (set! *tms* (if-ok-justify-node *tms* 'j4 'g (list 'd 'e)))

  (set! *tms* (tms-create-node *tms* 'Loser #:contradictoryp #t))

  (set! *tms* (if-ok-justify-node *tms* 'j5 'Loser (list 'e 'f)))

  (set! *tms* (if-ok-enable-assumption *tms* 'a))
  (set! *tms* (if-ok-enable-assumption *tms* 'b))
  (check-false (if-ok-enable-assumption *tms* 'c))
  (set! *tms* (enable-assumption *tms* 'd))

  (why-node *tms* 'Loser)

  (printf "\nOK\n\n"))

(define (ex1-ok)
  (define *tms* (create-jtms "Simple Example" #:debugging #t))

  (set! *tms* (tms-create-node *tms* 'a #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'b #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'c #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'd #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'e #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'f #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'g #:assumptionp #t))

  (set! *tms* (if-ok-justify-node *tms* 'j1 'f (list 'a 'b)))
  (set! *tms* (if-ok-justify-node *tms* 'j2 'e (list 'b 'c)))
  (set! *tms* (if-ok-justify-node *tms* 'j3 'g (list 'a 'e)))
  (set! *tms* (if-ok-justify-node *tms* 'j4 'g (list 'd 'e)))

  (set! *tms* (enable-assumption *tms* 'a))
  (set! *tms* (enable-assumption *tms* 'b))
  (set! *tms* (enable-assumption *tms* 'c))
  (set! *tms* (enable-assumption *tms* 'd))

  (set! *tms* (tms-create-node *tms* 'Loser #:contradictoryp #t))

  (check-false (if-ok-justify-node *tms* 'j5 'Loser (list 'e 'f)))

  (why-node *tms* 'Loser)

  (printf "\nOK\n\n"))

(define (ex1)

  (define *tms* (create-jtms "Simple Example" #:debugging #t))

  (set! *tms* (tms-create-node *tms* 'a #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'b #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'c #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'd #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'e #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'f #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'g #:assumptionp #t))

  (set! *tms* (justify-node *tms* 'j1 'f (list 'a 'b)))
  (set! *tms* (justify-node *tms* 'j2 'e (list 'b 'c)))
  (set! *tms* (justify-node *tms* 'j3 'g (list 'a 'e)))
  (set! *tms* (justify-node *tms* 'j4 'g (list 'd 'e)))

  (set! *tms* (enable-assumption *tms* 'a))
  (set! *tms* (enable-assumption *tms* 'b))
  (set! *tms* (enable-assumption *tms* 'c))
  (set! *tms* (enable-assumption *tms* 'd))

  (explore-network *tms* 'g)
  (explore-network *tms* 'f)

  (set! *tms* (tms-create-node *tms* 'Loser #:contradictoryp #t))

  (set! *tms* (justify-node *tms* 'j5 'Loser (list 'e 'f))))

(define (ex3)

  (define *tms* (create-jtms "Multiple support example"))
  (set! *tms* (tms-create-node *tms* 'A #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'C #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'E #:assumptionp #t))
  (set! *tms* (tms-create-node *tms* 'h))

  (set! *tms* (enable-assumption *tms* 'A))
  (set! *tms* (enable-assumption *tms* 'C))
  (set! *tms* (enable-assumption *tms* 'E))
  (set! *tms* (justify-node *tms* 'R1 'h (list 'C 'E)))

  (set! *tms* (tms-create-node *tms* 'g))

  (set! *tms* (justify-node *tms* 'R2 'g (list 'A 'C)))

  (set! *tms* (tms-create-node *tms* 'CONTRADICTION #:contradictoryp #t))
  (set! *tms* (justify-node *tms* 'R3 'CONTRADICTION (list 'g))))

;; these are not enabled, because they are interactive
;;(ex1)
;;(ex3)
(ex1-ok)
(ex1-b-ok)
