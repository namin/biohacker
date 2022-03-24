#lang racket

(require "ijtms.rkt")
(require rackunit)

(define (arguing1)
  (define *alice* (tms-for 'Alice))
  (define *bob* (tms-for 'Bob))

  (hears *bob* '(assumption (Alice is smart)))
  (hears *bob* '(assumption (Alice is fun)))
  (hears *bob* '(contradiction (Alice does not exist)))
  (believes *bob* '(assumption (Alice is smart)))
  (believes *bob* '(justification
                    (Alice does not exist)
                    reason ((Alice is smart) (Alice is fun))))

  (hears *alice* '(assumption (Alice is smart)))
  (hears *alice* '(assumption (Alice is fun)))
  (define alice-justification '(justification (Alice is fun) reason ((Alice is smart))))
  (believes *alice* alice-justification)

  (check-true (believes? *bob* '(Alice is smart)))
  (check-false (accepts? *bob* alice-justification))

  (check-false (believes? *alice* '(Alice is smart)))
  (check-false (believes? *alice* '(Alice is fun)))
  (believes *alice* '(assumption (Alice is smart)))
  (check-true (believes? *alice* '(Alice is smart)))
  (check-true (believes? *alice* '(Alice is fun)))
  )
(arguing1)
