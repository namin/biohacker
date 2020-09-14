#lang racket

(require "utils.rkt")
(require "jtms.rkt")
(require "jtre.rkt")
(provide (all-defined-out))

(define (jsaint-rules)

;;; expand pointers
(rule ((:in (and-subgoals ?parent ?children) :var ?def))
      (for/list ((child ?children))
        (rlet ((?child (:eval child)))
              (rassert! (parent-of ?child ?parent :and)
                        (:def-of-and ?def))
              (rule ((:in (failed ?child) :var ?delinquent))
                    (rassert! (failed ?parent)
                              (:and-failure ?def ?delinquent)))))
      (assert! `(solved ,?parent)
                `(:and-success ,?def
                  ,@(map (lambda (child)
                           `(solved ,child))
                         ?children))))

(rule ((:in (or-subgoals ?parent ?children) :var ?def
            :test (not (null? ?children))))
      (for/list ((child ?children))
        (rlet ((?child (:eval child)))
              (rassert! (parent-of ?child ?parent :or)
                        (:def-of-or ?def))
              (rule ((:in (solved ?child) :var ?winner))
                    (rassert! (solved ?parent)
                              (:or-success ?winner ?def)))))
      (assert! `(failed ,?parent)
               `(:or-failure ,?def
                             ,@(map (lambda (child)
                                      `(failed ,child))
                                    ?children))))

(rule ((:in (parent-of ?child ?parent ?type) :var ?lineage))
      (rassert! (relevant ?child)
                (:still-working-on (open ?parent) ?lineage)))

(rule ((:in (solution-of ?problem ?answer) :var ?found))
      (rassert! (solved ?problem) (:found-answer ?found)))

(rule ((:in (or-subgoals (integrate ?expr) nil) :var ?no-ideas))
      (rassert! (failed (integrate ?expr)) (:no-methods ?no-ideas)))

(rule ((:in (solved ?problem))) ;; can only happen once
      (retract! `(open ,?problem) ':expand-agenda-item #t))

(rule ((:in (failed ?problem)))
      (retract! `(open ,?problem) ':expand-agenda-item #t))

  )
