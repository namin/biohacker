#lang racket

(require "utils.rkt")
(require "jtms.rkt")
(require "jtre.rkt")
(provide (all-defined-out))

(define *n-assumptions* 0)
(define *placements* '())

(define (test-queens from to)
  (for [(n (in-range from (+ to 1)))]
       (collect-garbage)
       (time (n-queens n))
       (printf "\n For n=~a, ~a solutions, ~a assumptions."
               n (length *placements*) *n-assumptions*)))

(define (n-queens n [debugging? #f])
  (setup-queens-puzzle n debugging?)
  (solve-queens-puzzle (make-queens-choice-sets n))
  (length *placements*))

(define (setup-queens-puzzle n [debugging? #f])
  (in-jtre (create-jtre (format "~a-Queens JTRE" n)
                        #:debugging debugging?))
  (set! *placements* '())
  (set! *n-assumptions* 0)
  (contradiction 'queens-capture *jtre*)
  (rule ((:in (queen ?column1 ?row1) :var ?q1)
         (:in (queen ?column2 ?row2) :var ?q2
              :test (not (or (= ?column1 ?column2)
			     (queens-okay? ?column1 ?row1
				           ?column2 ?row2)))))
        (rassert! queens-capture (death ?q1 ?q2)))
  )

(define (make-queens-choice-sets n)
  (for/list
   ([c (in-range 1 (+ 1 n))])
   (for/list
    ([r (in-range 1 (+ 1 n))])
    `(queen ,c ,r))))

(define (solve-queens-puzzle choice-sets)
  (cond
   ((null? choice-sets) (gather-queens-solution))
   (else
    (for ([choice (car choice-sets)])
         (unless (in? `(not ,choice) *jtre*)
           ;;respect nogood information
           (let-values
               (((nogood? asns)
                 (try-in-context
                  choice
                  (lambda () (solve-queens-puzzle (cdr choice-sets)))
                  *jtre*)))
             (inc! *n-assumptions*)
             (when nogood?
	       ;;This assumption lost, so justify the negation
	       ;; based on the other relevant assumptions.
               (assert! `(not ,choice)
                        `(nogood ,@(remove choice asns))))))))))

;;;; JTMS approximation to try-in-context

(define (try-in-context asn thunk jtre)
  (let ((try-marker (cons 'try asn))
        (result #f))
    (with-handlers
     ([(lambda (x) (and (pair? x) (eq? (car x) 'try-in-context)))
       (lambda (x) (values (cadr x) (caddr x)))])
     (with-contradiction-handler
      (jtre-jtms jtre)
      (lambda (jtms contras)
        (try-contradiction-handler
         contras jtms asn try-marker jtre))
      (begin
        (unless (in? asn jtre)
          (set! result
                (with-handlers
                 ([(lambda (x) (and (pair? x) (eq? (car x) 'try-contradiction-found)))
                   (lambda (x) (cdr x))])
                 (assume! asn try-marker jtre)))
          (when (and (pair? result) (eq? (car result) ':asns))
            (raise `(try-in-context #t ,(map view-node (cdr result)))))
          (set! result (with-handlers
                        ([(lambda (x) (and (pair? x) (eq? (car x) 'try-contradiction-found)))
                          (lambda (x) (cdr x))])
                        (run-rules jtre)))
          (when (and (pair? result) (eq? (car result) ':asns))
            (raise `(try-in-context #t ,(map view-node (cdr result)))))
          (thunk)
          (retract! asn try-marker #t)
          (raise `(try-in-context #f #f))))))))

(define (try-contradiction-handler contras jtms asn marker jtre)
  (with-jtre
   jtre
   (unless (eq? jtms (jtre-jtms *jtre*))
     (error 'try-contradiction-handler (format "\nHigh contradiction weirdness: ~a not jtms for ~a" jtms *jtre*)))
   (when (and (not (null? contras)) asn)
     (let ((node (get-tms-node asn)))
       (for
        ([cnode contras])
        (let ((asns (assumptions-of-node cnode)))
          (when (member node asns)
            (retract! asn marker)
            (raise `(try-contradiction-found :asns . ,asns)))))))))

;;; Other helpers

(define (queens-okay? x1 y1 x2 y2)
  (not (or (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))))

(define (gather-queens-solution)
  (push!
   (remove-if (lambda (q) (out? q *jtre*))
              (fetch `(queen ?c ?r) *jtre*))
   *placements*))

(define (remove-if p . xs)
  (apply filter (lambda x (not (apply p x))) xs))

(define (show-queens-solution solution)
  (let ((n (length solution)))
    (for ([i (in-range 0 n)])
         (newline)
         (for ([j (in-range 0 n)])
              (printf "~a"
                      (if (member `(queen ,(+ 1 i) ,(+ 1 j)) solution)
                          "Q" "-"))))))

;; regression tests
(define (assumptions-of-node-not-empty-second-time)
  (setup-queens-puzzle 2 #t)
  (change-jtms (jtre-jtms *jtre*) #:debugging #t)
  (assume! '(queen 1 1) '(try queen 1 1))
  (assume! '(queen 2 1) '(try queen 2 1))
  (run-rules)
  (assume! '(queen 2 2) '(try queen 2 2))
  (run-rules))
