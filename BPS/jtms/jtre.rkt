#lang racket

(require "jtms.rkt")
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
          (create-jtms (list #:jtms-of title)
                       #:node-string view-node)
          (hasheq)
          0
          0
          debugging
          '()
          0)))
    (change-jtms (jtre-jtms j)
                 #:enqueue-procedure
                 (lambda (rule) (enqueue rule j)))))

(define (enqueue rule jtre)
  'TODO)

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

(define (get-dbclass fact [jtre *jtre*])
  'TODO)

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
  'TODO)

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
 rule
 (id           ; Unique ID for easy lookup
  jtre         ; The JTRE it is part of
  dbclass      ; Dbclass of associated pattern
  matcher      ; Procedure that performs the match.
  body         ; Procedure that does the work.
  )
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc this port mode)
     (fprintf port "<Rule ~a>" (rule-id this)))]
  )

(define *file-counter* 0)
(define *file-prefix* "")

(define (rule-file prefix)
  (set! *file-couter* 0)
  (set! *file-prefix* prefix))
