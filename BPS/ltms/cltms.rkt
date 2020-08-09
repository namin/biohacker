#lang racket
(provide (all-defined-out))
(define (subsumed? lits trie)
  (with-handlers ((ret (lambda (x) x)))
    (match-define (list it slot) (list #f #f))
    (for ((entry trie))
      (unless lits (raise #f))
      (set! slot (member (car entry) lits))
      (when slot
        (unless (list? (cdr entry)) (raise (cdr entry)))
        (when (set! it (subsumed? (cdr slot) (cdr entry)))
            (raise it))))))

(define (ret x)
  #t)

(define (add-to-trie cl ltms) ;; TODO 
  void
  )

(define (delay-sat? . op)
  void
  )

(define (index-clause . op)
  void
  )
