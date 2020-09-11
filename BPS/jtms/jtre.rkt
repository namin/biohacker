#lang racket

(require "utils.rkt")
(require "jtms.rkt")
(require "unify.rkt")
(require "funify.rkt")
(provide (all-defined-out))

;; jinter

(struct
 jtre
 (title                   ; Pretty name
  jtms                    ; Pointer to its JTMS
  dbclass-table;; nil     ; Table of dbclasses
  datum-counter;; 0       ; Unique ID for asserts
  rule-counter;; 0        ; Unique ID for rules
  debugging;; nil         ; If non-NIL, show basic operations
  queue;; nil             ; Rule queue
  rules-run;; 0           ; Statistic
  )
 #:mutable
 #:methods gen:custom-write
 [(define (write-proc this port mode)
    (fprintf port "<JTRE: ~a>" (jtre-title this)))]
 )

(define *jtre* #f)

(define-syntax with-jtre
  (syntax-rules ()
    [(_  jtre body ...)
     (if (eq? jtre *jtre*)
         (begin body ...)
         (let ((old-jtre *jtre*))
           (set! *jtre* jtre)
           (let ((r (begin body ...)))
             (set! *jtre* old-jtre)
             r)))]))

(define (in-jtre jtre)
  (set! *jtre* jtre))

(define-syntax debugging-jtre
  (syntax-rules ()
    [(_  jtre msg e* ...)
     (let ((args (list e* ...)))
       (when (jtre-debugging jtre)
         (apply printf msg args)))
     ]))

(define (create-jtre title #:debugging (debugging #f))
  (let ((j
         (jtre
          title
          (create-jtms (list ':jtms-of title)
                       #:node-string view-node)
          (hasheq)
          0
          0
          debugging
          '()
          0)))
    (change-jtms (jtre-jtms j)
                 #:enqueue-procedure
                 (lambda (rule) (enqueue rule j)))
    j))


(define (change-jtre jtre #:debugging (debugging ':nada))
  (unless (eq? debugging ':nada)
    (set-jtre-debugging! jtre debugging)))

;;;; Running JTRE

(define (uassert! fact [just 'user])
  (assert! fact just)
  (run-rules *jtre*))

(define (uassume! fact reason)
  (assume! fact reason *jtre*)
  (run-rules *jtre*))

;; jdata

(struct
 dbclass
 (name    ; Corresponding symbol
  jtre    ; JTRE it is part of.
  facts   ; Associated facts
  rules   ; Associated rules
  )
 #:mutable
 #:methods gen:custom-write
 [(define (write-proc this port mode)
    (fprintf port "<Dbclass ~a>" (dbclass-name this)))]
 )

(struct
 datum
 (id                   ; Unique ID for easy lookup
  lisp-form            ; Expression for pattern-matching
  tms-node;; nil       ; Pointer into TMS
  dbclass              ; Dbclass of the corresponding pattern
  assumption?;; nil    ; if non-nil, indicates informant
  plist;; nil          ; local property list
  )
 #:mutable
 #:methods gen:custom-write
 [(define (write-proc this port mode)
    (fprintf port "<Datum: ~a>" (datum-id this)))]
  )

;;;; Making statements

(define (assert! fact just [jtre *jtre*])
  (let* ((datum (referent fact #t))
         (node (datum-tms-node datum)))
    (unless (list? just) (set! just (list just)))
    (debugging-jtre "\n    Asserting ~a via ~a." fact just)
    (justify-node (car just) node
                  (map (lambda (f) (datum-tms-node (referent f #t)))
                       (cdr just)))
    datum))

;; TODO: is rassert! needed?

(define (quiet-assert! fact just [jtre *jtre*])
  (with-jtre
   jtre
   (with-contradiction-check (jtre-jtms *jtre*) (assert! fact just))))

(define (assume! fact reason [jtre *jtre*])
  (with-jtre
   jtre
   (let* ((datum (referent fact #t))
          (node (datum-tms-node datum)))
     (cond
      ((not (datum-assumption? datum))
       (set-datum-assumption?! datum reason)
       (debugging-jtre "\n    Assuming ~a via ~a." fact reason)
       (assume-node node))
      ((eq? reason (datum-assumption? datum)))
      (else
       (error
        'assume!
        (format "Fact ~a assumed because of ~a assumed again because of ~a"
                (show-datum datum)
                (datum-assumption? datum)
                reason)))))))

(define (already-assumed? fact)
  (let ((r (referent fact)))
    (and r (datum-assumption? r))))

;;;; Retraction

(define (retract! fact [just 'user] [quiet? #f] [jtre *jtre*])
  (with-jtre
   jtre
   (let ((datum (referent fact #t))
         (node (datum-tms-node datum)))
     (cond
      ((not (tms-node-assumption? node))
       (unless quiet?
         (display (format "\n~a isn't an assumption." (show-datum datum)))))
      ((not (in-node? node))
       (unless quiet?
         (display (format "\nThe assumption ~a is not currently in." fact))))
      ((eq? just (datum-assumption? datum))
       (debugging-jtre "\n    Retracting ~a via ~a." fact just)
       (set-datum-assumption?! datum #f)
       (retract-assumption node))
      ((not quiet?)
       (display (format "\n~a is not source of assumption for ~a" just fact))))
     node)))

;; TODO: is rretract! needed?

(define (contradiction fact [jtre *jtre*])
  (with-jtre
   jtre
   (make-contradiction (datum-tms-node (referent fact #t)))))

;;;; Interface and display of data

(define (in? fact [jtre *jtre*])
  (with-jtre
   jtre
   (let ((r (referent fact)))
     (and r
          (in-node? (datum-tms-node r))))))

(define (out? fact [jtre *jtre*])
  (with-jtre
   jtre
   (let ((r (referent fact)))
     (or (not r)
         (out-node? (datum-tms-node r))))))

(define (why? fact [jtre *jtre*])
  (with-jtre
   jtre
   (let ((r (referent fact)))
     (when r
       (why-node (datum-tms-node r))))))

(define (assumptions-of fact [jtre *jtre*])
  (map view-node
       (assumptions-of-node
        (datum-tms-node (referent fact #t)))))

;; TODO: fetch


;;;; More display-intensive procedures

;; TODO: wfs

;; TODO: say-datum-belief

;; TODO: show-justifications

;; TODO: show-data

;;;; Database system

(define (bound? x)
  'TODO);; boundp

(define (variable-value x)
  'TODO) ;; symbol-value

(define (get-dbclass fact [jtre *jtre*])
  (with-jtre
   jtre
   (cond ((null? fact) (error 'get-dbclass "\n NIL can't be a dbclass."))
	 ((pair? fact) (get-dbclass (car fact) *jtre*))
	 ((variable? fact)
	  (cond ((bound? fact)
		 (get-dbclass (variable-value fact) *jtre*))
	        (else (error 'get-dbclass (format "\nDbclass unbound: ~a" fact)))))
	 ((symbol? fact)
          (let ((h (hash-ref (jtre-dbclass-table *jtre*) fact #f)))
	    (cond (h h)
	          (else (let ((dc (dbclass fact *jtre* '() '())))
		          (hash-set! (jtre-dbclass-table *jtre*)
                                     fact
			             dc)
		          dc)))))
	 (else (error 'get-dbclass (format "Bad dbclass type: ~a" fact))))))


(define (referent fact [virtual? #f] [jtre *jtre*])
  (with-jtre
   jtre
   (if virtual? (insert fact) (referent1 fact))))

(define (referent1 fact)
  (let ((c #f))
    (for ([candidate (dbclass-facts (get-dbclass fact))])
         #:break c
         (when (equal? (datum-lisp-form candidate) fact)
           (set! c candidate)))
    c))

(define (insert fact)
  (let ((datum (referent1 fact)))
    (cond
     (datum (values datum #t))
     (else
      (set! datum
            (datum (let* ((id (+ 1 (jtre-datum-counter *jtre*)))
                          (_ (set-jtre-datum-counter! *jtre* id)))
                     id)
                   fact
                   'TODO
                   (get-dbclass fact)
                   #f
                   '()))
      (set-datum-tms-node! datum (tms-create-node (jtre-jtms *jtre*) datum))
      (set-dbclass-facts! (datum-dbclass datum)
                          (cons datum (dbclass-facts (datum-dbclass datum))))
      (try-rules datum)
      (values datum #f)))))

(define (get-candidates pattern)
  (dbclass-facts (get-dbclass pattern)))

(define (map-dbclass proc [jtre *jtre*])
  (with-jtre
   jtre
   (hash-map
    (jtre-dbclass-table *jtre*)
    (lambda (name dbclass)
      (proc dbclass)))))

(define (get-tms-node fact [jtre *jtre*])
  (with-jtre
   jtre
   (datum-tms-node (referent fact #t))))


(define (view-node node)
  (datum-lisp-form (tms-node-datum node)))

;;;; More query routines

(define (show-datum node)
  (format "~a" (datum-lisp-form datum)))

(define (get-datum (num [jtre *jtre*]))
  (with-jtre
   jtre
   (with-handlers
    ([datum? (lambda (x) x)])
    (map-dbclass
     (lambda (dbclass)
       (for ([datuum (dbclass-facts dbclass)])
            (when (= (datum-id datum) num)
              (raise datum))))))))

(define (get-just num [jtre *jtre*])
  (with-jtre
   jtre
   (with-handlers
    ([just? (lambda (x) x)])
    (for ([just (jtms-justs (jtre-jtms *jtre*))])
         (when (= (just-index just) num)
           (raise just))))))

;; jrules

(struct
 jrule
 (id           ; Unique ID for easy lookup
  jtre         ; The JTRE it is part of
  dbclass      ; Dbclass of associated pattern
  matcher      ; Procedure that performs the match.
  body         ; Procedure that does the work.
  )
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc this port mode)
     (fprintf port "<Rule ~a>" (jrule-id this)))]
  )

(define *file-counter* 0)
(define *file-prefix* "")

(define (rule-file prefix)
  (set! *file-counter* 0)
  (set! *file-prefix* prefix))

;;;; Building and installing rules

(define-syntax rule
  (syntax-rules ()
    [(_ triggers body ...)
     (do-rule triggers (list body ...))]))

(define *rule-procedures* '())
(define *bound-vars* '())
(define (do-rule triggers body)
  (set! *rule-procedures* '())
  (set! *bound-vars* '())
  (let ((index-form
         (build-rule (car triggers)
                     (subst 'internal-rule
                            'rule
                            (make-nested-rule
                             (cdr triggers) body)))))
    `(begin ,@*rule-procedures* ,index-form)))

;; TODO?
(define (subst new old lst)
  lst)

(define-syntax internal-rule
  (syntax-rules ()
    [(_ triggers body ...)
     (add-internal-rule
      (car triggers)
      (make-nested-rule (cdr triggers) (list body ...)))]))

(define (make-nested-rule triggers body)
  (if (null? triggers)
      body
      (add-internal-rule
       (car triggers)
       (make-nested-rule (cdr triggers) body))))

(define-syntax add-internal-rule
  (syntax-rules ()
    [(_ trigger body)
     (build-rule trigger body)]))

;;;; Details of rule-building

(define (build-rule trigger body)
  (let-values (((pattern condition var test) (parse-rule-trigger trigger)))
    (let ((match-procedure
           (generate-match-procedure pattern var test condition))
          (body-procedure
           (generate-body-procedure pattern condition var body)))
      (push! match-procedure *rule-procedures*)
      (push! body-procedure *rule-procedures*)
      `(insert-rule
        (get-dbclass ,(get-trigger-dbclass pattern))
        ;; return form to index rule
        ;; the match function for a rule
        ,(if *bound-vars*
             `(lambda (p)
                (,(car match-procedure) p ,@*bound-vars*))
             (cadr match-procedure))
        ;; the body function for rule
        ,(if (or *bound-vars*
                 (not (eq? condition ':intern)))
             (let ((tv (reverse (pattern-free-variables trigger))))
               (unless (eq? condition ':intern)
                 (push! 'trigger-node tv))
               `(lambda ,tv
                  (,(cadr body-procedure) ,@tv
                   ,@(scratchout tv *bound-vars*))))
             (cadr body-procedure))))))

(define (parse-rule-trigger trigger)
  (values (cadr trigger)
          (if (member (car trigger) '(:intern :in :out))
              (car trigger)
              (error
               'parse-rule-trigger
               (format
                "\n Unknown belief condition ~a in trigger ~a."
                (car trigger) trigger)))
          (cadr (member ':var (cddr trigger)))
          (cadr (member ':test (cddr trigger)))))

(define (get-trigger-dbclass trigger)
  (cond ((variable? trigger)
         (if (member trigger *bound-vars*) trigger
             (error 'get-trigger-dbclass (format "\nTrigger dbclass is unbound -- ~a" trigger))))
        ((atom? trigger) (list 'quote trigger))
        (else (get-trigger-dbclass (car trigger)))))

(define (atom? x)
  (not (pair? x)))

;;;; Generating the body function

(define-syntax with-pushed-variable-bindings
  (syntax-rules ()
    [(_ new-bindings body ...)
     (let ((old-bound-vars *bound-vars*))
       (set! *bound-vars*
             (append new-bindings
                     (scratchout new-bindings *bound-vars*)))
       (let ((r (begin body ...)))
         (set! *bound-vars* old-bound-vars)
         r))]))

(define (generate-body-procedure pattern condition var body)
  (let ((newly-bound (pattern-free-variables pattern)))
    (when var (push! var newly-bound))
    (set! body (with-pushed-variable-bindings
                newly-bound (fully-expand-body body)))
    (let ((env (append
                newly-bound
                (scratchout newly-bound *bound-vars*))))
      (unless (eq? condition ':intern)
        (push! 'trigger-node env))
      (let ((fname (generate-rule-procedure-name pattern)))
        `(define (,fname ,@env)
           ,@(cond ((eq? condition ':intern) body) ;; Just do it
                   (else ;; Must check and see if the node's belief state
                         ;; matches the rule's requirements
                    `((cond
                       ((,(cond
                           ((eq? condition ':in) in-node?)
                           ((eq? condition ':out) out-node?)
                           (else (error generate-body-procedure
                                        (format "~a bad condition" condition))))
                         trigger-node) ,@body)
                       (else
                        (push! (list ',fname ,@env)
                               ,(cond
                                 ((eq? condition ':in)
                                  '(tms-node-in-rules trigger-node))
                                 ((eq? condition ':out)
                                  '(tms-node-out-rules trigger-node))))))))))))))

(define (generate-match-procedure pattern var test condition)
  (let-values (((tests binding-specs)
                 (generate-match-body pattern (pattern-free-variables pattern) test)))
    `(define (,(generate-rule-procedure-name pattern)
              p ,@*bound-vars*)
       ;;first arg, p, is the pattern
       (if (and ,@tests)
           (values #t (list ,@(if var '(p) '())
                            ,@(reverse binding-specs))
                   ,(if (eq? condition ':intern) #f #t))
           #f))))

(define (scratchout l1 l2)  ;non-destructive and order-preserving
  ;;(dolist (el1 l1 l2) (setq l2 (remove el1 l2)))
  (remove* l1 l2))

(define (generate-rule-procedure-name pattern)
  (string->symbol (format "~a-~a-~a" *file-prefix* pattern (inc! *file-counter*))))

;;;; Recursive macroexpansion

(define (fully-expand-body body)
  body
  ;; TODO
  )

;;;; Running rules

(define (insert-rule dbclass matcher body)
  (with-jtre
   (dbclass-jtre dbclass)
   (let* ((id (+ 1 (jtre-rule-counter *jtre*)))
          (_ (set-jtre-rule-counter! *jtre* id))
          (rule (jrule id *jtre* dbclass matcher body)))
     (set-dbclass-rules! dbclass (cons rule (dbclass-rules dbclass)))
     (for ([candidate (dbclass-facts dbclass)])
          (try-rule-on rule candidate)))))

(define (try-rules datum)
  (for ([rule (dbclass-rules (datum-dbclass datum))])
       (try-rule-on rule datum)))

(define (try-rule-on (rule datum))
  (with-jtre
   (dbclass-jtre (datum-dbclass datum))
   (let-values (((okay? bindings node?) ((jrule-matcher rule) (datum-lisp-form datum))))
     (when okay?
       (when node?
         (push! (datum-tms-node datum) bindings))
       (enqueue (cons (jrule-body rule) bindings) *jtre*)))))

(define (run-rules [jtre *jtre*])
  (with-jtre
   jtre
   (let loop ((form (dequeue *jtre*))
              (counter 0))
     (if (null? form)
         (begin
           (debugging-jtre "\n    ~a rules run." counter)
           (set-jtre-rules-run! *jtre* (+ counter (jtre-rules-run *jtre*))))
         (begin
           (apply (car form) (cdr form))
           (loop (dequeue *jtre* (+ 1 counter))))))))

(define (rules-waiting? jtre)
  (not (null? (jtre-queue jtre))))

(define (enqueue rule jtre)
  (set-jtre-queue! jtre (cons rule (jtre-queue jtre))))

(define (dequeue jtre)
  (set-jtre-queue! jtre (cdr (jtre-queue jtre))))

;; funify

(define (pattern-free-variables pattern)
  (pattern-free-variables0 pattern *bound-vars*))

(define (generate-match-body pattern vars extra-test)
  (generate-match-body0 pattern vars extra-test *bound-vars*))

