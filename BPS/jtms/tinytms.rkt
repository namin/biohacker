#lang racket

(provide (all-defined-out))

(struct tms
        (
         title
         node-counter                  ;; unique namer for nodes.
         justification-counter         ;; unique namer for justifications.
         nodes                         ;; list of all tms nodes.
         justifications                ;; list of all justifications
         debugging                     ;; debugging flag
         contradictions                ;; list of contradiction nodes.
         assumptions                   ;; list of assumption nodes.
         checking-contradictions       ;; For external systems
         contradiction-handler
         enqueue-procedure
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<tms ~a>" (tms-title this)))])

(define (make-tms
         title
         #:node-counter (node-counter 0)
         #:justification-counter (justification-counter 0)
         #:nodes (nodes '())
         #:justifications (justifications '())
         #:debugging (debugging #f)
         #:contradictions (contradictions '())
         #:assumptions (assumptions '())
         #:checking-contradictions (checking-contradictions #t)
         #:contradiction-handler (contradiction-handler ask-user-handler)
         #:enqueue-procedure (enqueue-procedure #f))
  (tms title node-counter justification-counter nodes justifications debugging
       contradictions assumptions checking-contradictions contradiction-handler
       enqueue-procedure))

(struct tms-node
        (
         index
         datum                  ;; pointer to external problem solver
         label                  ;; :in means believed, :out means disbelieved
         support                ;; Current justification or premise marker
         justifications         ;; Possible justifications
         consequences           ;; Justifications in which it is an antecedent
         mark                   ;; Marker for sweep algorithms
         contradictory?         ;; Flag marking it as contradictory
         assumption?            ;; Flag marking it as an assumption.
         in-rules               ;; Rules that should be triggered when node goes in
         out-rules              ;; Rules that should be triggered when node goes out
         tms                    ;; The TMS in which this node appears.
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<node ~a>" (tms-node-datum this)))]
        )

(define (make-tms-node
         #:index index
         #:datum (datum '())
         #:label (label ':out)
         #:support (support #f)
         #:justifications (justifications '())
         #:consequences (consequences '())
         #:mark (mark #f)
         #:contradictory? (contradictory? #f)
         #:assumption? (assumption? #f)
         #:in-rules (in-rules '())
         #:out-rules (out-rules '())
         #:tms tms)
  (tms-node index datum label support justifications consequences mark
            contradictory? assumption? in-rules out-rules tms))

(struct justification
        (
         index
         informant
         consequence
         antecedents
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<just ~a>" (justification-index this)))]
        )

(define (make-just   #:index index
                     #:informant informant
                     #:consequence consequence
                     #:antecedents antecedents)

  (justification
   index
   informant
   consequence
   antecedents
   ))

(define-syntax push-field!
  (lambda (stx)
    (define syn (syntax->list stx))
    (define getter (syntax->datum (cadr syn)))
    (define setter (string->symbol (format "set-~a!" getter)))
    (define x (caddr syn))
    (define s (cadddr syn))
    (datum->syntax
     #'here
     `(,setter ,s (cons ,x (,getter ,s))))))

(define-syntax-rule (push! val lst)
  (set! lst (cons val lst)))
(define-syntax-rule (pop! lst)
  (cond
    ((null? lst) '())
    (else (let ((popped (car lst))) (set! lst (rest lst)) popped))))

(define-syntax debugging-tms
  (syntax-rules ()
    [(_  tms msg e* ...)
     (let ((args (list e* ...)))
       (when (tms-debugging tms)
         (apply printf msg args)))
     ]))

(define  (in-node? node) (eq? (tms-node-label node) ':in))
(define  (out-node? node) (eq? (tms-node-label node) ':out))

(define (tms-create-node tms datum
                         #:assumption? (assumption? #f)
                         #:contradictory? (contradictory? #f))
  (let ((counter (+ 1 (tms-node-counter tms))))
    (set-tms-node-counter! tms (+ 1 counter))
    (let ((node (make-tms-node #:index counter
                               #:datum datum
                               #:tms tms
                               #:assumption? assumption?
                               #:contradictory? contradictory?
                               )))
      (when assumption? (push-field! tms-assumptions node tms))
      (when contradictory? (push-field! tms-contradictions node tms))
      (push-field! tms-nodes node tms)
      node)))

(define (tms-node-premise? node)
  (let ([support (tms-node-support node)])
    (and support
         (not (eq? support ':enabled-assumptions))
         (null? (justification-antecedents support)))))

(define (check-justification just)
  (and (out-node? (justification-consequence just))
       (justification-satisfied? just)))

(define (justification-satisfied? just)
  (andmap in-node? (justification-antecedents just)))

(define (assume-node node)
  (let ((tms (tms-node-tms node)))
    (unless (or (tms-node-assumption? node) (tms-node-premise? node))
      (debugging-tms tms "\nConverting ~a into an assumption" node)
      (set-tms-node-assumption?! node #t)
      (push-field! tms-assumptions node tms)))
  (enable-assumption node))

(define (make-contradiction node)
  (let ((tms (tms-node-tms node)))
    (unless (tms-node-contradictory? node)
      (set-tms-node-contradictory?! node #t)
      (push-field! tms-contradictions node tms))
    (check-for-contradictions tms)))

(define (justify-node informant consequence antecedents)
  (let* (
         (tms (tms-node-tms consequence))
         (_ (set-tms-justification-counter! tms (+ 1 (tms-justification-counter tms))))
         (just (make-just #:index (tms-justification-counter tms)
                          #:informant informant
                          #:consequence consequence
                          #:antecedents antecedents))
         )
    (push-field! tms-node-justifications just consequence)
    (for ((node antecedents)) (push-field! tms-node-consequences just node))
    (push-field! tms-justifications just tms)
    (debugging-tms tms
                    "\nJustifying ~a by ~a using ~a."
                    consequence
                    informant
                    antecedents)
    (if (or (not (null? antecedents)) (out-node? consequence))
        (when (check-justification just) (install-support consequence just))
        (set-tms-node-support! consequence just))
    (check-for-contradictions tms)))

(define (enable-assumption node)
  (let ((tms (tms-node-tms node)))
    (unless (tms-node-assumption? node)
      (error 'enabled-assumption (format "Can't enable the non-assumption ~a" node)))
    (debugging-tms tms "\n  Enabling assumption ~a." node)
    (cond (
           (out-node? node) (make-node-in node ':enabled-assumption)
           (propagate-inness node))
          ((or (eq? (tms-node-support node) ':enabled-assumption)
               (null? (justification-antecedents (tms-node-support node)))))
          (else (set-tms-node-support! node ':enabled-assumption)))
    (check-for-contradictions tms)))

(define (retract-assumption node)
  (let ((tms (tms-node-tms node)))
    (when (eq? (tms-node-support node) ':enabled-assumption)
      (debugging-tms tms "\n  Retracting assumption ~a." node)
      (make-node-out node)
      (find-alternative-support tms (cons node (propagate-outness node tms))))))

(define (make-node-in conseq reason)
  (let* ((tms (tms-node-tms conseq))
         (enqueuef (tms-enqueue-procedure tms)))
    (debugging-tms tms "\n     Making ~a in via ~a."
                    conseq
                    (if (symbol? reason)
                        reason
                        (cons (justification-informant reason)
                              (justification-antecedents reason))))
    (set-tms-node-label! conseq ':in)
    (set-tms-node-support! conseq reason)
    (when enqueuef
      (for ((in-rule (tms-node-in-rules conseq)))
           (enqueuef in-rule))
      (set-tms-node-in-rules! conseq '()))))

(define (propagate-inness node)
  (let ((tms (tms-node-tms node))
        (q (list node)))
    (do () ((begin (set! node (pop! q)) (null? node)))
      (debugging-tms tms "\n   Propagating belief in ~a." node)
      (for ((justification (tms-node-consequences node)))
           (when (check-justification justification)
             (make-node-in (justification-consequence justification) justification)
             (push! (justification-consequence justification) q))))))

(define (make-node-out node)
  (define tms (tms-node-tms node))
  (define enqueuef (tms-enqueue-procedure tms))
  (debugging-tms tms "\n     retracting belief in ~a." node)
  (set-tms-node-support! node #f)
  (set-tms-node-label! node ':out)
  (when enqueuef (for ((out-rule (tms-node-out-rules node)))
                      (enqueuef out-rule)))
  (set-tms-node-out-rules! node '()))

(define (propagate-outness node tms)
  (let ((out-queue '()))
    (debugging-tms tms "\n   Propagating disbelief in ~a." node)
    (do ((js (tms-node-consequences node) (append (cdr js) newvar))
         (newvar '() '())
         (conseq #f))
        ((null? js) out-queue)
      (set! conseq (justification-consequence (car js)))
      (when (eq? (tms-node-support conseq) (car js))
        (make-node-out conseq)
        (push! conseq out-queue)
        (set! newvar (tms-node-consequences conseq))))))

(define (install-support conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

(define (find-alternative-support tms out-queue)
  (debugging-tms tms "\n   Looking for alternative supports.")
  (with-handlers ([justification? (lambda (x) x)])
    (for ((node out-queue))
         (unless (in-node? node)
           (for ((just (tms-node-justifications node)))
                (when (check-justification just)
                  (install-support (justification-consequence just)
                                   just)
                  (raise just)))))))

(define (check-for-contradictions tms)
  (let ((contradictions '()))
    (when (tms-checking-contradictions tms)
      (for ((cnode (tms-contradictions tms)))
           (when (in-node? cnode) (push! cnode contradictions)))
      (unless (null? contradictions)
        ((tms-contradiction-handler tms) tms contradictions)))))

;;; Well-founded support inqueries
(define (supporting-justification-for-node node) (tms-node-support node))

(define (assumptions-of-node node)
  (let ((assumptions '()) (marker (list ':mark)))
    (do ((nodes (list node) (append (cdr nodes) newvar))
         (newvar '() '()))
        ((null? nodes) assumptions)
      (let ((node (car nodes)))
        (cond ((eq? (tms-node-mark node) marker))
              ((eq? (tms-node-support node) ':enabled-assumption)
               (push! node assumptions))
              ((in-node? node)
               (set! newvar (justification-antecedents (tms-node-support node))))
              )
        (set-tms-node-mark! node marker)))))

(define (enabled-assumptions tms)
  (let ((result '()))
    (for ((assumption (tms-assumptions tms)))
         (when (eq? (tms-node-support assumption) ':enabled-assumption)
           (push! assumption result)))
    result))

;; contradition handler

(define (ask-user-handler jtms contradiction)
  (handle-one-contradiction (car contradiction))
  (check-for-contradictions jtms))

(define (handle-one-contradiction contra-node)
  (define *contra-assumptions* (assumptions-of-node contra-node))
  (when (null? *contra-assumptions*)
      (error 'handle-one-contradiction "\nThere is a flaw in the universe...~a" contra-node))
  (printf  "\nContradiction found: ~a" contra-node)
  (print-contra-list *contra-assumptions*)
  (printf "\nPick the <number> to retract assumption. ")
  (let ((the-answer (read)))
    (when (and (integer? the-answer)
               (> the-answer 0)
               (not (> the-answer (length *contra-assumptions*))))
      (retract-assumption (list-ref *contra-assumptions* (- the-answer 1))))))

(define (print-contra-list nodes)
  (do ((counter 1 (+ 1 counter))
       (nn nodes (cdr nn)))
      ((null? nn))
    (printf "\n~a ~a\n" counter (car nn))))

;; stubs

(define (why-node node)
  (let ((justification (tms-node-support node)))
    (cond
      ((eq?  justification ':enabled-assumption)
       (printf "\n~a is an enabled assumption" node))
      (justification
       (printf "\n~a is IN via ~a on"
               node
               (justification-informant justification))
       (for ((anode (justification-antecedents justification)))
            (printf  "\n  ~a" anode)))
      (else (printf "\n~a is OUT." node)))
    node))

(define (why-nodes tms)
  (for ((node (tms-nodes tms))) (why-node node)))
