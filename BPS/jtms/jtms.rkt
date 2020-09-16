#lang racket

(require "utils.rkt")
(provide (all-defined-out))

(struct jtms
        (
         title ;;nil)
         node-counter ;;0)             ;; unique namer for nodes.
         just-counter ;;0)             ;; unique namer for justifications.
         nodes ;;nil)                  ;; list of all tms nodes.
         justs ;;nil)                  ;; list of all justifications
         debugging ;;nil)              ;; debugging flag
         contradictions ;;nil)         ;; list of contradiction nodes.
         assumptions ;;nil)            ;; list of assumption nodes.
         checking-contradictions ;;#t)  ;; For external systems
         node-string ;;nil)
         contradiction-handler ;;nil)
         enqueue-procedure ;;nil)
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<jtms ~a>" (jtms-title this)))]
        ;;#:transparent
        )

(struct tms-node
        (
         index ;;0)
         datum ;;nil)           ;; pointer to external problem solver
         label ;;:OUT)          ;; :IN means believed, :OUT means disbelieved
         support ;;nil)         ;; Current justification or premise marker
         justs ;;nil)           ;; Possible justifications
         consequences ;;nil)    ;; Justifications in which it is an antecedent
         mark ;;nil)            ;; Marker for sweep algorithms
         contradictory? ;;nil)  ;; Flag marking it as contradictory
         assumption? ;;nil)     ;; Flag marking it as an assumption.
         in-rules ;;nil)	;; Rules that should be triggered when node goes in
         out-rules ;;nil)	;; Rules that should be triggered when node goes out
         jtms ;;nil))           ;; The JTMS in which this node appears.
         )
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<node ~a>" (node-string this)))]
        )

(struct just ;;justification
        (index ;;0)
         informant
         consequence
         antecedents)
        #:mutable
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<just ~a>" (just-index this)))]
        )
(define (make-just   #:index (index 0)
                     #:informant informant
                     #:consequence consequence
                     #:antecedents antecedents)

  (just
   index
   informant
   consequence
   antecedents
   ))

(define (tms-node-premise? node)
  (let ([support (tms-node-support node)])
    (and support ;; other than #f everything is true even '()
         (not (eq? support ':ENABLED-ASSUMPTION)) ;; :ENABLED-ASSUMPTION ?
         (null? (just-antecedents support)))))

;;; Simple utilities:

(define (node-string node)
  ((jtms-node-string (tms-node-jtms node)) node))

(define-syntax debugging-jtms
  (syntax-rules ()
    [(_  jtms msg e* ...)
     (let ((args (list e* ...)))
       (let ((args (if (and (not (null? args)) (tms-node? (car args)))
                       (cons (node-string (car args)) (cdr args))
                       args)))
         (when (jtms-debugging jtms)
           (apply printf msg args))))
     ]))

(define (tms-error proc string node)
  (error proc (format string node)))

(define (default-node-string n)
  (format "~a" (tms-node-datum n)))

(define (create-jtms title
                     #:node-string (node-string default-node-string)
                     #:debugging (debugging #f)
                     #:checking-contradictions (checking-contradictions #t)
                     #:contradiction-handler (contradiction-handler ask-user-handler)
                     #:enqueue-procedure (enqueue-procedure #f))
  (jtms
   title
   0
   0
   '()
   '()
   debugging
   '()
   '()
   checking-contradictions
   node-string
   contradiction-handler
   enqueue-procedure))

(define (change-jtms jtms
                     #:contradiction-handler (contradiction-handler #f)
                     #:node-string (node-string #f)
                     #:enqueue-procedure (enqueue-procedure #f)
                     #:debugging (debugging #f)
                     #:checking-contradictions (checking-contradictions #f)
                     )
  (when node-string (set-jtms-node-string! jtms node-string))
  (when  debugging (set-jtms-debugging! jtms debugging))
  (when checking-contradictions
    (set-jtms-checking-contradictions! jtms
                                       checking-contradictions))
  (when contradiction-handler
    (set-jtms-contradiction-handler! jtms contradiction-handler))
  (when enqueue-procedure
    (set-jtms-enqueue-procedure! jtms enqueue-procedure)))

;;;;; Basic inference-engine interface ;;;;;;

(define  (in-node? node) (eq? (tms-node-label node) ':IN))
(define  (out-node? node) (eq? (tms-node-label node) ':OUT))

(define (tms-create-node jtms datum
                         #:assumptionp (assumptionp #f)
                         #:contradictoryp (contradictoryp #f))
  (let ((counter (+ 1 (jtms-node-counter jtms))))
    (set-jtms-node-counter! jtms (+ counter 1))
    (let ((node (tms-node counter
                          datum
                          ':OUT
                          #f
                          '()
                          '()
                          #f
                          contradictoryp
                          assumptionp
                          '()
                          '()
                          jtms)))
      (when assumptionp (push-jtms-assumptions! node jtms))
      (when contradictoryp (push-jtms-contradictions! node jtms))
      (push-jtms-nodes! node jtms)
      node)))

;;; Converts a regular node to an assumption and enables it.

(define (assume-node node)
  (let ((jtms (tms-node-jtms node)))
    (unless (or (tms-node-assumption? node) (tms-node-premise? node))
      (debugging-jtms jtms "\nConverting ~a into an assumption" node)
      (set-tms-node-assumption?! node #t)
      (push-jtms-assumptions! node jtms)))
  (enable-assumption node))

(define (make-contradiction node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-contradictory? node)
      (set-tms-node-contradictory?! node #t)
      (push-jtms-contradictions! node jtms))
    (check-for-contradictions jtms)))

(define (justify-node informant consequence antecedents)
  (let* (
         (jtms (tms-node-jtms consequence))
         (_ (set-jtms-just-counter! jtms (+ 1 (jtms-just-counter jtms))))
         (just (make-just #:index (jtms-just-counter jtms)
                          #:informant informant
                          #:consequence consequence
                          #:antecedents antecedents))
         )
    (push-tms-node-justs! just consequence)
    (for ((node antecedents)) (push-tms-node-consequences! just node))
    (push-jtms-justs! just jtms)
    (debugging-jtms jtms
                    "\nJustifying ~a by ~a using ~a."
                    consequence
                    informant
                    (map node-string antecedents))
    (if (or (not (null? antecedents)) (out-node? consequence))
        (when (check-justification just) (install-support consequence just))
        (set-tms-node-support! consequence just))
    (check-for-contradictions jtms)))

;;;;;;;;;;;Support for adding justifications;;;;;;;;;;;;;;;;

(define (check-justification just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

(define (justification-satisfied? just)
  (andmap in-node? (just-antecedents just)))

(define (install-support conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

(define (propagate-inness node)
  (let ((jtms (tms-node-jtms node))
        (q (list node)))
    (do () ((begin (set! node (pop! q)) (null? node)))
      (debugging-jtms jtms "\n   Propagating belief in ~a." node)
      (for ((justification (tms-node-consequences node)))
           (when (check-justification justification)
             (make-node-in (just-consequence justification) justification)
             (push! (just-consequence justification) q))))))

(define (make-node-in conseq reason)
  (let* ((jtms (tms-node-jtms conseq))
         (enqueuef (jtms-enqueue-procedure jtms)))
    (debugging-jtms jtms "\n     Making ~a in via ~a."
                    conseq
                    (if (symbol? reason)
                        reason
                        (cons (just-informant reason)
                              (map (jtms-node-string jtms)
                                   (just-antecedents reason)))))
    (set-tms-node-label! conseq ':IN)
    (set-tms-node-support! conseq reason)
    (when enqueuef
      (for ((in-rule (tms-node-in-rules conseq)))
           (enqueuef in-rule))
      (set-tms-node-in-rules! conseq '()))))

;;; Assumption Manipulation
(define (retract-assumption node)
  (let ((jtms #f))
    (when (eq? (tms-node-support node) ':ENABLED-ASSUMPTION)
      (set! jtms (tms-node-jtms node))
      (debugging-jtms jtms "\n  Retracting assumption ~a." node)
      (make-node-out node)
      (find-alternative-support jtms (cons node (propagate-outness node jtms))))))

(define (enable-assumption node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-assumption? node)
      (tms-error 'enabled-assumption "Can't enable the non-assumption ~a" node))
    (debugging-jtms jtms "\n  Enabling assumption ~a." node)
    (cond (
           (out-node? node) (make-node-in node ':ENABLED-ASSUMPTION)
           (propagate-inness node))
          ((or (eq? (tms-node-support node) ':ENABLED-ASSUMPTION)
               (null? (just-antecedents (tms-node-support node)))))
          (else (set-tms-node-support! node ':ENABLED-ASSUMPTION)))
    (check-for-contradictions jtms)))

(define (make-node-out node)
  (define jtms (tms-node-jtms node))
  (define enqueuef (jtms-enqueue-procedure jtms))
  (debugging-jtms jtms "\n     retracting belief in ~a." node)
  (set-tms-node-support! node #f)
  (set-tms-node-label! node ':OUT)
  (when enqueuef (for ((out-rule (tms-node-out-rules node)))
                      (enqueuef out-rule)))
  (set-tms-node-out-rules! node '()))

(define (propagate-outness node jtms)
  (let ((out-queue '()))
    (debugging-jtms jtms "\n   Propagating disbelief in ~a." node)
    (do ((js (tms-node-consequences node) (append (cdr js) newvar))
         (newvar '() '())
         (conseq #f))
        ((null? js) out-queue)
      ;; For each justification using the node, check to see if
      ;; it supports some other node.  If so, forget that node,
      ;; queue up the node to look for other support, and recurse
      (set! conseq (just-consequence (car js)))
      (when (eq? (tms-node-support conseq) (car js))
        (make-node-out conseq)
        (push! conseq out-queue)
        (set! newvar (tms-node-consequences conseq))))))

(define (find-alternative-support jtms out-queue)
  (debugging-jtms jtms "\n   Looking for alternative supports.")
  (with-handlers ([just? (lambda (x) x)])
    (for ((node out-queue))
         (unless (in-node? node)
           (for ((just (tms-node-justs node)))
                (when (check-justification just)
                  (install-support (just-consequence just)
                                   just)
                  (raise just)))))))

;;; Contradiction handling interface
(define (check-for-contradictions jtms)
  (let ((contradictions '()))
    (when (jtms-checking-contradictions jtms)
      (for ((cnode (jtms-contradictions jtms)))
           (when (in-node? cnode) (push! cnode contradictions)))
      (unless (null? contradictions)
        ((jtms-contradiction-handler jtms) jtms contradictions)))))

(define-syntax contradiction-check
  (syntax-rules ()
    [(_ jtms flag body ...)
     (let* ((jtmsv jtms)
            (old-value (jtms-checking-contradictions jtms)))
       (begin
         (set-jtms-checking-contradictions! jtms flag)
         (let ((r (begin body ...)))
           (set-jtms-checking-contradictions! jtms flag)
           r)))]))

(define-syntax without-contradiction-check
  (syntax-rules ()
    [(_ jtms body ...)
     (contradiction-check jtms #f body ...)]))

(define-syntax with-contradiction-check
  (syntax-rules ()
    [(_ jtms body ...)
     (contradiction-check jtms #t body ...)]))

(define-syntax with-contradiction-handler
  (syntax-rules ()
    [(_ jtms handler body ...)
     (let* ((jtmsv jtms)
            (old-handler (jtms-contradiction-handler jtmsv)))
       (begin
         (set-jtms-contradiction-handler! jtmsv handler)
         (let ((r body ...))
         (set-jtms-contradiction-handler! jtmsv old-handler))))]))

(struct contradiction-signal ())

(define (default-assumptions jtms)
  (with-contradiction-check
   jtms
   (with-contradiction-handler
    jtms
    (lambda () (raise (contradiction-signal)))
    (for ((assumption (jtms-assumptions jtms)))
         (cond ((eq? (tms-node-support assumption) ':ENABLED-ASSUMPTION))
               ((not (eq? ':DEFAULT (tms-node-assumption? assumption))))
               ((with-handlers
                 ([contradiction-signal?
                   (lambda (x) (retract-assumption assumption))])
                (enable-assumption assumption))))))))

;;; Well-founded support inqueries
(define (supporting-justification-for-node node) (tms-node-support node))

(define (assumptions-of-node node)
  (let ((assumptions '()) (marker (list ':MARK)))
    (do ((nodes (list node) (append (cdr nodes) newvar))
         (newvar '() '()))
        ((null? nodes) assumptions)
      (let ((node (car nodes)))
        (cond ((eq? (tms-node-mark node) marker))
              ((eq? (tms-node-support node) ':ENABLED-ASSUMPTION)
               (push! node assumptions))
              ((in-node? node)
               (set! newvar (just-antecedents (tms-node-support node))))
              )
        (set-tms-node-mark! node marker)))))

(define (enabled-assumptions jtms)
  (let ((result '()))
    (for ((assumption (jtms-assumptions jtms)))
         (when (eq? (tms-node-support assumption) ':ENABLED-ASSUMPTION)
           (push! assumption result)))
    result))

;; Inference engine stub to allow this JTMS to be used stand alone

(define (why-node node)
  (let ((justification (tms-node-support node)))
    (cond
      ((eq?  justification ':ENABLED-ASSUMPTION)
       (printf "\n~a is an enabled assumption"
               (node-string node)))
      (justification
       (printf "\n~a is IN via ~a on"
               (node-string node)
               (just-informant justification)
               )
       (for ((anode (just-antecedents justification)))
            (printf  "\n  ~a" (node-string anode))))
      (else (printf "\n~a is OUT." (node-string node))))
    node))

(define (why-nodes jtms)
  (for ((node (jtms-nodes jtms))) (why-node node)))

(define *contra-assumptions* '())

(define (ask-user-handler jtms contradiction)
  (handle-one-contradiction (car contradiction))
  (check-for-contradictions jtms))

(define (handle-one-contradiction contra-node)
  (set! *contra-assumptions* (assumptions-of-node contra-node))
  (when (null? *contra-assumptions*)
      (tms-error 'handle-one-contradiction "\nThere is a flaw in the universe...~a" contra-node))
  (printf  "\nContradiction found: ~a" (node-string contra-node))
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
    (printf "\n~a ~a\n" counter (node-string (car nn)))))

(define (tms-answer num)
  (let ((the-answer
         (if (integer? num)
             (if (> num 0)
                 (if (not (> num (length *contra-assumptions*)))
                     num
                     (begin
                       (printf "\nIgnoring answer, too big.")
                       #f))
                 (begin
                   (printf "\nIgnoring answer, too small")
                   #f))
             (begin
               (printf "\nIgnoring answer, must be an integer.")
               #f))))
    (if the-answer
        the-answer
        (tms-answer (read)))))

(define (explore-network node)
  (cond
    ((not (in-node? node))
     (printf "\n Sorry, ~a not believed." (node-string node))
     node)
    (else
     (with-handlers
      ([tms-node? (lambda (x) x)])
      (do ((stack '())
           (current node)
           (options '())
           (olen 0))
          ((null? current))
        (why-node current)
        (set! options (when (just? (tms-node-support current))
                        (just-antecedents (tms-node-support current))))
        (set! olen (length options))
        (do ((good? #f)
             (choice 0))
            (good? (case good?
                     ((q) (raise current))
                     ((0) (if (not (null? stack))
                              (set! current (pop! stack))
                              (raise current)))
                     (else (push! current stack)
                           (set! current (list-ref options (- good? 1))))))
          (printf "\n>>>")
          (set! choice (read))
          (cond ((or (eq? choice 'q)
                     (and (integer? choice)
                          (not (> choice olen))
                          (not (< choice 0))))
                 (set! good? choice))
                (else (printf
                       "\n Must be q or an integer from 0 to ~a."
                       olen)))))))))

(define (push-jtms-assumptions! node jtms)
  (set-jtms-assumptions! jtms (cons node (jtms-assumptions jtms))))
(define (push-jtms-contradictions! node jtms)
  (set-jtms-contradictions! jtms (cons node (jtms-contradictions jtms))))
(define (push-jtms-nodes! node jtms)
  (set-jtms-nodes! jtms (cons node (jtms-nodes jtms))))
(define (push-jtms-justs! just jtms)
  (set-jtms-justs! jtms (cons just (jtms-justs jtms))))

(define (push-tms-node-justs! node justs)
  (set-tms-node-justs! justs (cons node (tms-node-justs justs))))
(define (push-tms-node-consequences! just node)
  (set-tms-node-consequences! node (cons just (tms-node-consequences node))))
