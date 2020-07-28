#lang racket

(provide (all-defined-out))

(struct ltms
        (
         title ;;nil)
         node-counter ;;0)             ;; unique namer for nodes.
         clause-counter ;;0)             ;; unique namer for justifications.
         nodes ;;nil)                  ;; list of all tms nodes.
         clauses ;;nil)                  ;; list of all justifications
         debugging ;;nil)              ;; debugging flag
         checking-contradictions ;;#t)  ;; For external systems
         node-string ;;nil)
         contradiction-handler ;;nil)
         pending-contradictions ;;nil)
         enqueue-procedure ;;nil)
         complete ;;nil)
         violated-clauses ;;nil)
         queue ;;nil)
         conses ;;nil)
         delay-sat ;;nil)
         cons-size ;;nil)
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<LTMS: ~a>" (ltms-title this)))]
        ;;#:transparent
        )

(struct tms-node
        (
         index ;;0)             ;; unique namer for nodes
         datum ;;nil)           ;; positive inference engine datum.
         label ;;:UNKNOWN)      ;; :UNKNOWN, :TRUE, or :FALSE.
         support ;;nil)         ;; clause which supports it,
         true-clauses ;;nil)    ;; clauses in which this node is true
         false-clauses ;;nil)   ;; clauses in which this node is false
         mark ;;nil)            ;; Marker for sweep algorithms
         assumption? ;; nil)
         true-rules ;; nil)     ;; rules run when the node is true
         false-rules ;; nil)    ;; rules run when the node is false
         ltms ;; nil)           ;; LTMS it is part of.
         true-literal ;;nil)	;; True literal. 
         false-literal ;;nil)   ;; False literal.
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<NODE: ~a>" (node-string this)))]
        )

(struct clause ;;clause
        (index ;;0)
         informant
         literals
         pvs
         length
         sats
         status)
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<Clause ~d>" (just-index this)))]
        )

(define (make-clause #:index (index 0)
                     #:informant informant
                     #:literals literals
                     #:pvs (pvs 0)
                     #:length (length 0)
                     #:sats (sats 0)
                     #:status status)

  (clause
   index
   informant
   literals
   pvs
   length
   sats
   status
   ))

;;; Simple utilities:

(define (node-string node)
  ((ltms-node-string (tms-node-ltms node)) node))

(define-syntax debugging-ltms
  (syntax-rules ()
    [(_  ltms msg e* ...)
     (let ((args (list e* ...)))
       (let ((args (if (and (not (null? args)) (tms-node? (car args)))
                       (cons (node-string (car args)) (cdr args))
                       args)))
         (when (ltms-debugging ltms)
           (apply printf msg args))))
     ]))

(define (ltms-error string thing)
  (error string thing))

(define (default-node-string n)
  (format "~a" (tms-node-datum n)))

(define (satisfied-clause? clause)
  (lambda (literal)
    (equal? (tms-node-label (car literal))
        (cdr literal)))
  (clause-literals clause))

(define-syntax violated-clause?
  (syntax rules ()
    [(_ clause ...)
     (= (clause-pvs clause) 0)]))

(define-syntax walk-clauses
  (syntax rules ()
     [(_ ltms f ...)
      (if (ltms-complete ltms)
          (walk-trie f (ltms-clauses ltms))
          (map f (ltms-clauses ltms)))]))

(define (create-ltms title
                     #:node-string (node-string default-node-string)
                     #:debugging (debugging #f)
                     #:checking-contradictions (checking-contradictions #t)
                     #:contradiction-handler (contradiction-handler ask-user-handler)
                     #:enqueue-procedure (enqueue-procedure #f)
                     #:cache-datums? (cache-datums? #t)
                     #:complete (complete #f)
                     #:delay-sat (delay-sat #t)
                     )
  (ltms
   title
   0
   0
   (when cache-datums? (make-hash))
   '()
   debugging
   checking-contradictions
   node-string
   contradiction-handler
   '()
   enqueue-procedure
   complete
   '()
   '()
   '()
   delay-sat
   0))

(define (change-ltms ltms
                     #:contradiction-handler (contradiction-handler #f contra?)
                     #:node-string (node-string #f)
                     #:enqueue-procedure (enqueue-procedure #f)
                     #:debugging (debugging #f debugging?)
                     #:checking-contradictions (checking-contradictions #f checking?)
                     #:complete (complete #f complete?)
                     #:delay-sat (delay-sat #f delay-sat?)
                     )
  (when node-string (set-ltms-node-string! ltms node-string))
  (when  debugging? (set-ltms-debugging! ltms debugging))
  (when checking?
    (set-ltms-checking-contradictions! ltms
                                       checking-contradictions))
  (when contra?
    (set-ltms-contradiction-handler! ltms contradiction-handler))
  (when enqueue-procedure
    (set-ltms-enqueue-procedure! ltms enqueue-procedure))
   (when complete?
    (set-ltms-complete! ltms complete))
   (when delay-sat?
    (set-ltms-delay-sat! ltms delay-sat)))

(define  (unknown-node? node) (equal? (tms-node-label node) ':UNKNOWN))
(define  (known-node? node) (not (equal? (tms-node-label node) ':UNKNOWN)))
(define  (true-node? node) (equal? (tms-node-label node) ':TRUE))
(define  (false-node? node) (equal? (tms-node-label node) ':FALSE))

