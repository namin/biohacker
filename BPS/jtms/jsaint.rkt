#lang racket

(require "utils.rkt")
(require "jtms.rkt")
(require "jtre.rkt")
(provide (all-defined-out))

(struct
 jsaint
 (title;; ""        ;; Name for printing
  jtre;; nil        ;; Associated JTRE
  agenda;; nil      ;; List of queued subproblems
  problem;; nil     ;; When solved, we are done.
  solution;; nil    ;; Cached answer.
  n-subproblems;; 0 ;; Statistic
  max-tasks;; 20    ;; resource bound
  debugging;; nil   ;; Debugging flag
        )
 #:mutable
 #:methods gen:custom-write
 [(define (write-proc this port mode)
    (fprintf port "<Agenda: ~a>" (jsaint-title this)))]
 )

(define *jsaint* #f)

(define (create-jsaint title problem :#debugging (debugging #f) #:max-tasks (max-tasks #f))
  (let ((ag (jsaint
             title
             (create-jtre (format "JTRE of ~a" title))
             '()
             problem
             '()
             0
             (if (integer? max-tasks) max-tasks 20)
             debugging)))
    (in-jtre (jsaint-jtre ag))
    (change-jtms (jtre-jtms (jsaint-jtre ag))
                 #:contradiction-handler jsaint-contradiction-handler)
    (use-jsaint ag)))

(define-syntax debugging-jsaint
  (syntax-rules ()
    [(_ js msg arg ...)
     (when (jsaint-debugging js) (printf msg arg ...))]))

(define (change-jsaint js
                       #:debugging (debugging ':nada)
                       #:problem (problem ':nada)
                       #:max-tasks (max-tasks ':nada))
  (unless (eq? debugging ':nada) (set-jsaint-debugging! js debugging))
  (unless (eq? problem ':nada) (set-jsaint-problem! js problem))
  (unless (eq? max-tasks ':nada) (set-jsaint-max-tasks! js max-tasks)))

(define-syntax with-jsaint
  (syntax-rules ()
    [(_  js body ...)
     (if (eq? js *jsaint*)
         (begin body ...)
         (let ((old-js *jsaint*))
           (use-jsaint js)
           (let ((r (begin body ...)))
             (use-jsaint old-js)
             r)))]))

(define (use-jsaint js) (set! *jsaint* js))

;;;; Auxiliary routines

(define (jsaint-contradiction-handler contradictions jtms)
  (ask-user-handler contradictions jtms)) ;; default
