 #lang racket

 (require "logic.rkt")
 (provide (all-defined-out))


 #|
 from https://arxiv.org/pdf/1304.3084.pdf
 [s (A)
    s (A^)]
 s(A) support for A, evidence for A
 s(A^) = evidence for not A, evidence against A
 invariant 0 <= S(A) + s(A^) <= 1

 oplus = orthonalplus
 (a b) oplus (c d) = (
                      (1 - (a^c^ / (1 - (ad + bc))))
                      (1 - (b^d^ / (1 - (ad + bc)))) 
                      )
 |#

 (define (oplus i1 i2)
   (let ((a (interval-s i1))
         (b (interval-p i1))
         (c (interval-s i2))
         (d (interval-p i2)))
     (let ((denom (- 1
                     (+ (* a d)
                        (* b c)))))
       (interval (- 1
                    (/ (* (- 1 a)
                          (- 1 c))
                       denom))
                 (- 1
                    (/ (* (- 1 b)
                          (- 1 d))
                       denom))))))


 (define (ominus i1 i2)
   (let* ((a (interval-s i1))
          (a^ (- 1 a))
          (b (interval-p i1))
          (b^ (- 1 b))
          (c (interval-s i2))
          (c^ (- 1 c))
          (d (interval-p i2))
          (d^ (- 1 d)))
     (let ((denom (- (* c^ d^)
                     (+ (* b^ c c^)
                        (* a^ d d^)))))
       (interval (/ (* c^
                       (- (* a d^)
                          (* b^ c)))
                    denom)
                 (/ (* d^
                       (- (* b c^)
                          (* a^ d)))
                    denom)))))


 (define default-belief
   (interval 0 1))

 (define (combine-beliefs . nodes)
   (if (null? (cdr nodes))
       (tms-node-belief (car nodes))
     (oplus (tms-node-belief (car nodes))
            (apply combine-beliefs
                   (cdr nodes)))))

 (struct jbms
   (title ;;nil)
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
          belief-threshold ;; belief-threshold for true?,....
          tolerance ;; delta tolerance for updating node belief
          belief_diff ;; function for computing difference between beliefs
          )
         #:mutable #:methods
         gen:custom-write [(define (write-proc this port mode)
                             (fprintf port
                                      "<jbms ~a>"
                                      (jbms-title this)))])

 (struct tms-node
   (index ;;0)
    datum ;;nil)           ;; pointer to external problem solver
          label ;;:OUT)          ;; :IN means believed, :OUT means disbelieved
          support ;;nil)         ;; Current justification or premise marker
          justs ;;nil)           ;; Possible justifications
          consequences ;;nil)    ;; Justifications in which it is an antecedent
          mark ;;nil)            ;; Marker for sweep algorithms
          contradictory? ;;nil)  ;; Flag marking it as contradictory
          assumption? ;;nil)     ;; Flag marking it as an assumption.
          belief ;; Dempster-Shafer belief
          in-rules ;;nil)	;; Rules that should be triggered when node goes in
          out-rules ;;nil)	;; Rules that should be triggered when node goes out
          jbms ;;nil))           ;; The JBMS in which this node appears.
          )
         #:mutable #:methods
         gen:custom-write [(define (write-proc this port mode)
                             (fprintf port
                                      "<node ~a>"
                                      (node-string this)))])

 (struct just ;;justification
   (index ;;0)
    informant
    consequence
    antecedents
    belief
    support
    )
         #:mutable #:methods
         gen:custom-write [(define (write-proc this port mode)
                             (fprintf port
                                      "<just ~a>"
                                      (just-index this)))])
 (define (make-just #:index (index 0)
                    #:informant informant
                    #:consequence consequence
                    #:antecedents antecedents
                    #:belief (belief (interval 1 0))
                    #:support (support #f))
   (just index informant consequence antecedents
         belief support))

 (define (tms-node-premise? node)
   (let ([support (tms-node-support node)])
     (and support ;; other than #f everything is true even '()
          (not (equal? support ':ENABLED-ASSUMPTION)) ;; :ENABLED-ASSUMPTION ?
          (or (false? just-antecedents support)
              (empty? (just-antecedents support))))))

 ;;; Simple utilities:

 (define (node-string node)
   ((jbms-node-string (tms-node-jbms node)) node))

 (define-syntax debugging-jbms
   (syntax-rules ()
                 [(_ jbms msg e* ...)
                  (let ((args (list e* ...)))
                    (let ((args (if (and (not (null? args))
                                         (tms-node? (car args)))
                                    (cons (node-string (car args)) (cdr args))
                                  args)))
                      (when (jbms-debugging jbms)
                        (apply printf msg args))))]))

 (define (default-node-string n)
   (format "~a"
           (tms-node-datum n)))

(define (default-belief_diff i1 i2) ;; simple cartesian difference
  (let* ((s_diff (- (interval-s i1) (interval-s i2)))
         (p_diff (- (interval-p i1) (interval-p i2)))
         )
    (sqrt (+ (expt s_diff 2) (expt p_diff 2)))
    )
  )

 (define (create-jbms title
                      #:node-string (node-string default-node-string)
                      #:debugging (debugging #f)
                      #:checking-contradictions (checking-contradictions #t)
                      #:contradiction-handler (contradiction-handler ask-user-handler)
                      #:enqueue-procedure (enqueue-procedure #f)
                      #:belief-threshold (belief-threshold 0.0001)
                      #:tolerance (tolerance 0.0001)
                      #:belief_diff (belief_diff default-belief_diff)
                      )
   (jbms title
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
         enqueue-procedure
         belief-threshold
         tolerance
         belief_diff
         ))

 (define (change-jbms jbms
                      #:contradiction-handler (contradiction-handler #f)
                      #:node-string  (node-string #f)
                      #:enqueue-procedure (enqueue-procedure #f)
                      #:debugging  (debugging #f)
                      #:checking-contradictions (checking-contradictions #f)
                      #:belief-threshold (belief-threshold #f)
                      #:tolerance (tolerance #f)
                      #:belief_diff (belief_diff #f)
                      )
   (when node-string
     (set-jbms-node-string! jbms node-string))
   (when debugging
     (set-jbms-debugging! jbms debugging))
   (when checking-contradictions
     (set-jbms-checking-contradictions! jbms checking-contradictions))
   (when contradiction-handler
     (set-jbms-contradiction-handler! jbms contradiction-handler))
   (when enqueue-procedure
     (set-jbms-enqueue-procedure! jbms enqueue-procedure))
   (when belief-threshold
     (set-jbms-belief-threshold! jbms belief-threshold))
   (when tolerance
     (set-jbms-tolerance! jbms tolerance))
   (when belief_diff
     (set-jbms-belief_diff! jbms belief_diff))
   )

 ;;;;; Basic inference-engine interface ;;;;;;

 (define (in-node? node)
   (equal? (tms-node-label node)
           ':IN))
 (define (out-node? node)
   (equal? (tms-node-label node)
           ':OUT))

 (define (tms-create-node jbms
                          datum
                          #:belief (belief #f)
                          #:assumptionp (assumptionp #f)
                          #:contradictoryp (contradictoryp #f))
   (let ((assumptionp (or assumptionp belief)))
     (let ((counter (+ 1
                       (jbms-node-counter jbms))))
       (set-jbms-node-counter! jbms
                               (+ counter 1))
       (let ((node (tms-node counter
                             datum
                             ':OUT #f'()
                             '()
                             #f
                             contradictoryp
                             assumptionp
                             belief
                             '()
                             '()
                             jbms)))
         (when assumptionp
           (push-jbms-assumptions! node jbms))
         (when contradictoryp
           (push-jbms-contradictions! node jbms))
         (push-jbms-nodes! node jbms)
         node))))

 ;;; Converts a regular node to an assumption and enables it.

 (define (assume-node node)
   (let ((jbms (tms-node-jbms node)))
     (unless (or (tms-node-assumption? node)
                 (tms-node-premise? node))
       (debugging-jbms jbms "\nConverting ~a into an assumption"
                       node)
       (set-tms-node-assumption?! node #t)
       (push-jbms-assumptions! node jbms)))
   (enable-assumption node))

 (define (make-contradiction node)
   (let ((jbms (tms-node-jbms node)))
     (unless (tms-node-contradictory? node)
       (set-tms-node-contradictory?! node #t)
       (push-jbms-contradictions! node jbms))
     (check-for-contradictions jbms)))

 (define (justify-node informant consequence antecedents
                       belief)
   (let* ((jbms (tms-node-jbms consequence))
          (_ (set-jbms-just-counter! jbms
                                     (+ 1
                                        (jbms-just-counter jbms))))
          (just (make-just #:index (jbms-just-counter jbms)
                           #:informant informant
                           #:consequence consequence
                           #:antecedents antecedents
                           #:belief belief)))
     (push-tms-node-justs! just consequence)
     (for ((node antecedents))
          (push-tms-node-consequences! just node))
     (push-jbms-justs! just jbms)
     (debugging-jbms jbms
                     "\nJustifying ~a by ~a using ~a."
                     consequence
                     informant
                     (map node-string antecedents))
     (if (not (null? antecedents))
         (when (check-justification just)(install-support consequence just #:antecedent antecedents))
       (set-tms-node-support! consequence just))
     (check-for-contradictions jbms)))

 ;;;;;;;;;;;Support for adding justifications;;;;;;;;;;;;;;;;

(define (check-justification justification)
  (let* ((initial_belief (tms-node-belief (just-consequence justification)))
        (support_lend (impli (andi (for/list ((ante (just-antecedents justification))) (tms-node-belief ante))) (just-belief justification)))
        (new_belief
         (cond
           ((just-support justification) (oplus support_lend (ominus initial_belief  (just-support justification))))
          (else  oplus initial_belief support_lend))
         )
        (belief-diff (jbms-belief_diff (tms-node-jbms (just-consequence justification))))
        (tolerance (jbms-tolerance (tms-node-jbms (just-consequence justification))))
        (*jbms* (tms-node-jbms (just-consequence justification)))
        )
    
    (debugging-jbms *jbms* "\nDifference is ~A" (belief-diff new_belief initial_belief))
    (>= (belief-diff new_belief initial_belief) tolerance)
    )
  
  )

 (define (justification-satisfied? just)
   (andmap in-node?
           (just-antecedents just)))

 (define (install-support conseq
                          just
                          #:antecedent (antecedents '()))
   (make-node-in conseq just)
   (propagate-inness conseq))

 (define (propagate-inness node)
   (let ((jbms (tms-node-jbms node))
         (q (list node)))
     (do ()
         ((begin (set! node
                       (pop! q))
                 (null? node)))
       (debugging-jbms jbms "\n   Propagating belief in ~a."
                       node)
       (for ((justification (tms-node-consequences node)))
            (when (check-justification justification)
              (make-node-in-modify (just-consequence justification)
                                   justification)
              (push! (just-consequence justification)
                     q))))))

 (define (make-node-in-modify conseq reason)
   (let* ((jbms (tms-node-jbms conseq))
          (enqueuef (jbms-enqueue-procedure jbms)))
     (debugging-jbms jbms
                     "\n     Making ~a in via ~a. (modify version)"
                     conseq
                     (if (symbol? reason)
                         reason
                       (cons (just-informant reason) (map (jbms-node-string jbms)
                                                          (just-antecedents reason)))))
     (unless (symbol? reason)
       (set-tms-node-belief! conseq
                             (ominus (tms-node-belief conseq)
                                     (just-support reason)))
       (set-just-support! reason
                          (impli (andi (for/list ((ante (just-antecedents reason))) (tms-node-belief ante)))
                                 (just-belief reason)))
       (set-tms-node-belief! conseq
                             (oplus (tms-node-belief conseq)
                                          (impli (andi (for/list ((ante (just-antecedents reason))) (tms-node-belief ante)))
                                                 (just-belief reason))))
                             )
     (set-tms-node-label! conseq ':IN)
     (set-tms-node-support! conseq reason)
     (when enqueuef
       (for ((in-rule (tms-node-in-rules conseq)))
            (enqueuef in-rule))
       (set-tms-node-in-rules! conseq
                               '()))))


 (define (make-node-in conseq reason)
   (let* ((jbms (tms-node-jbms conseq))
          (enqueuef (jbms-enqueue-procedure jbms)))
     (debugging-jbms jbms
                     "\n     Making ~a in via ~a."
                     conseq
                     (if (symbol? reason)
                         reason
                       (cons (just-informant reason) (map (jbms-node-string jbms)
                                                          (just-antecedents reason)))))
     (unless (symbol? reason)
       (set-just-support! reason
                          (impli (andi (for/list ((ante (just-antecedents reason))) (tms-node-belief ante)))
                                 (just-belief reason)))
       (set-tms-node-belief! conseq
                             (oplus (tms-node-belief conseq) (just-support reason))
                             ))
     (set-tms-node-label! conseq ':IN)
     (set-tms-node-support! conseq reason)
     (when enqueuef
       (for ((in-rule (tms-node-in-rules conseq)))
            (enqueuef in-rule))
       (set-tms-node-in-rules! conseq
                               '()))))

 ;;; Assumption Manipulation
 (define (retract-assumption node)
   (let ((jbms #f))
     (when (equal? (tms-node-support node)
                   ':ENABLED-ASSUMPTION)
       (set! jbms
             (tms-node-jbms node))
       (debugging-jbms jbms "\n  Retracting assumption ~a."
                       node)
       (make-node-out node)
       (find-alternative-support jbms
                                 (cons node (propagate-outness node jbms))))))

 (define (enable-assumption node)
   (let ((jbms (tms-node-jbms node)))
     (unless (tms-node-assumption? node)
       (error 'enabled-assumption
              (format "Can't enable the non-assumption ~a"
                      node)))
     (debugging-jbms jbms "\n  Enabling assumption ~a."
                     node)
     (cond
      ((out-node? node)
       (make-node-in node ':ENABLED-ASSUMPTION)
       (propagate-inness node))
      ((or (equal? (tms-node-support node)
                   ':ENABLED-ASSUMPTION)
           (null? (just-antecedents (tms-node-support node)))))
      (#t
       (set-tms-node-support! node ':ENABLED-ASSUMPTION)))
     (check-for-contradictions jbms)))

 (define (make-node-out node)
   (define jbms
     (tms-node-jbms node))
   (define enqueuef
     (jbms-enqueue-procedure jbms))
   (debugging-jbms jbms "\n     retracting belief in ~a."
                   node)
   (set-tms-node-support! node #f)
   (set-tms-node-label! node ':OUT)
   (when enqueuef
     (for ((out-rule (tms-node-out-rules node)))
          (enqueuef out-rule)))
   (set-tms-node-out-rules! node #f))

 (define (propagate-outness node jbms)
   (let ((out-queue '()))
     (debugging-jbms jbms "\n   Propagating disbelief in ~a."
                     node)
     (do ((js (tms-node-consequences node)
              (append (cdr js)
                      newvar))
          (newvar '()
                  '())
          (conseq #f))
         ((null? js) out-queue)
       ;; For each justification using the node, check to see if
       ;; it supports some other node.  If so, forget that node,
       ;; queue up the node to look for other support, and recurse
       (set! conseq
             (just-consequence (car js)))
       (when (equal? (tms-node-support conseq)
                     (car js))
         (make-node-out conseq)
         (push! conseq out-queue)
         (set! newvar
               (tms-node-consequences conseq))))))

 (define (find-alternative-support jbms out-queue)
   (debugging-jbms jbms "\n   Looking for alternative supports.")
   (with-handlers ([just? (lambda (x)
                            x)])
                  (for ((node out-queue))
                       (unless (in-node? node)
                         (for ((just (tms-node-justs node)))
                              (when (check-justification just)
                                (install-support (just-consequence just)
                                                 just
                                                 (list node))
                                (raise just)))))))

 ;;; Contradiction handling interface
 (define (check-for-contradictions jbms)
   (let ((contradictions '()))
     (when (jbms-checking-contradictions jbms)
       (for ((cnode (jbms-contradictions jbms)))
            (when (in-node? cnode)
              (push! cnode contradictions)))
       (unless (null? contradictions)
         ((jbms-contradiction-handler jbms) jbms
          contradictions)))))

 (define-syntax contradiction-check
   (syntax-rules ()
                 [(_ jbms flag body ...)
                  (let* ((jbmsv jbms)
                         (old-value (jbms-checking-contradictions jbms)))
                    (begin (set-jbms-checking-contradictions! jbms flag)
                           (let ((r (begin body ...)))
                             (set-jbms-checking-contradictions! jbms flag)
                             r)))]))

 (define-syntax without-contradiction-check
   (syntax-rules ()
                 [(_ jbms body ...)
                  (contradiction-check jbms #f body ...)]))

 (define-syntax with-contradiction-check
   (syntax-rules ()
                 [(_ jbms body ...)
                  (contradiction-check jbms #t body ...)]))

 (define-syntax with-contradiction-handler
   (syntax-rules ()
                 [(_ jbms handler body ...)
                  (let* ((jbmsv jbms)
                         (old-handler (jbms-contradiction-handler jbmsv)))
                    (begin (set-jbms-contradiction-handler! jbmsv handler)
                           (let ((r body ...))
                             (set-jbms-contradiction-handler! jbmsv old-handler))))]))

 (struct contradiction-signal
         ())

 (define (default-assumptions jbms)
   (with-contradiction-check jbms
                             (with-contradiction-handler jbms
                                                         (lambda ()
                                                           (raise (contradiction-signal)))
                                                         (for ((assumption (jbms-assumptions jbms)))
                                                              (cond
                                                               ((eq? (tms-node-support assumption) ':ENABLED-ASSUMPTION))
                                                               ((not (eq? ':DEFAULT (tms-node-assumption? assumption))))
                                                               ((with-handlers ([contradiction-signal? (lambda (x)
                                                                                                         (retract-assumption assumption))])
                                                                               (enable-assumption assumption))))))))

 ;;; Well-founded support inqueries
 (define (supporting-justification-for-node node)
   (tms-node-support node))

 (define (assumptions-of-node node)
   (let ((assumptions '())
         (marker (list ':MARK)))
     (do ((nodes (list node)
                 (append (cdr nodes)
                         newvar))
          (newvar '()
                  '()))
         ((null? nodes) assumptions)
       (let ((node (car nodes)))
         (cond
          ((equal? (tms-node-mark node)
                   marker))
          ((equal? (tms-node-support node)
                   ':ENABLED-ASSUMPTION)
           (push! node assumptions))
          ((in-node? node)
           (set! newvar
                 (just-antecedents (tms-node-support node)))))
         (set-tms-node-mark! node marker)))))

 (define (enabled-assumptions jbms)
   (let ((result '()))
     (for ((assumption (jbms-assumptions jbms)))
          (when (equal? (tms-node-support assumption)
                        ':ENABLED-ASSUMPTION)
            (push! assumption result)))
     result))

 ;; Inference engine stub to allow this JBMS to be used stand alone

 (define (why-node node)
   (let ((justification (tms-node-support node)))
     (cond
      ((equal? justification ':ENABLED-ASSUMPTION)
       (printf "\n~a is an enabled assumption"
               (node-string node)))
      (justification ;; right condition? (printf "\n~a is IN via ~a on" (node-string node) (just-informant justification))
       (for ((anode (just-antecedents justification)))
            (printf "\n  ~a"
                    (node-string anode))))
      (#t
       (printf "\n~a is OUT."
               (node-string node))))
     node))

 (define (why-nodes jbms)
   (for ((node (jbms-nodes jbms)))
        (why-node node)))

 (define *contra-assumptions*
   '())

 (define (ask-user-handler jbms contradiction)
   (handle-one-contradiction (car contradiction))
   (check-for-contradictions jbms))

 (define (handle-one-contradiction contra-node)
   (set! *contra-assumptions*
         (assumptions-of-node contra-node))
   (unless *contra-assumptions*
     (error 'handle-one-contradiction
            (format "\nThere is a flaw in the universe...~a"
                    contra-node)))
   (printf "\nContradiction found: ~a"
           (node-string contra-node))
   (print-contra-list *contra-assumptions*)
   (printf "\nPick the <number> to retract assumption. ")
   (let ((the-answer (read)))
     (when (and (integer? the-answer)
                (> the-answer 0)
                (not (> the-answer (length *contra-assumptions*))))
       (retract-assumption (list-ref *contra-assumptions*
                                     (- the-answer 1))))))

 (define (print-contra-list nodes)
   (do ((counter 1
                 (+ 1 counter))
        (nn nodes
            (cdr nn)))
       ((null? nn))
     (printf "\n~a ~a\n"
             counter
             (node-string (car nn)))))

 (define (tms-answer num)
   (let ((the-answer (if (integer? num)
                         (if (> num 0)
                             (if (not (> num (length *contra-assumptions*)))
                                 num
                               (begin (printf "\nIgnoring answer, too big.")
                                      #f))
                           (begin (printf "\nIgnoring answer, too small")
                                  #f))
                       (begin (printf "\nIgnoring answer, must be an integer.")
                              #f))))
     (if the-answer
         the-answer
       (tms-answer (read)))))

 (define (explore-network node)
   (cond
    ((not (in-node? node))
     (printf "\n Sorry, ~a not believed."
             (node-string node))
     node)
    (else (with-handlers ([tms-node? (lambda (x)
                                       x)])
                         (do ((stack '())
                              (current node)
                              (options '())
                              (olen 0)
                              (done? #f))
                             ((or done?
                                  (null? current)))
                           (why-node current)
                           (set! options
                                 (when (just? (tms-node-support current))
                                   (just-antecedents (tms-node-support current))))
                           (set! olen
                                 (length options))
                           (do ((good? #f)
                                (choice 0))
                               (good? (case good?
                                        ((q)
                                         (raise current))
                                        ((0)
                                         (if (not (null? stack))
                                             (set! current
                                                   (pop! stack))
                                           (raise current)))
                                        ((#t)
                                         (push! current stack)
                                         (set! current
                                               (list-ref options
                                                         (- good? 1))))))
                             (printf "\n>>>")
                             (set! choice
                                   (read))
                             (cond
                              ((or (equal? choice 'q)
                                   (and (integer? choice)
                                        (<= choice olen))
                                   (>= choice 0))
                               (set! good? choice))
                              (#t
                               (printf "\n Must be q or an integer from 0 to ~a."
                                       olen)))))))))

 (define (push-jbms-assumptions! node jbms)
   (set-jbms-assumptions! jbms
                          (cons node (jbms-assumptions jbms))))
 (define (push-jbms-contradictions! node jbms)
   (set-jbms-contradictions! jbms
                             (cons node (jbms-contradictions jbms))))
 (define (push-jbms-nodes! node jbms)
   (set-jbms-nodes! jbms
                    (cons node (jbms-nodes jbms))))
 (define (push-jbms-justs! just jbms)
   (set-jbms-justs! jbms
                    (cons just (jbms-justs jbms))))

 (define (push-tms-node-justs! node justs)
   (set-tms-node-justs! justs
                        (cons node (tms-node-justs justs))))
 (define (push-tms-node-consequences! just node)
   (set-tms-node-consequences! node
                               (cons just (tms-node-consequences node))))

 (define-syntax-rule (push! val lst) ;;pushes val to list lst at first pos
   (set! lst
         (cons val lst)))
 (define-syntax-rule (pop! lst) ;; pops the first position
   (cond
    ((null? lst) '())
    (else (let ((popped (car lst)))
            (set! lst
                  (rest lst))
            popped))))

;;;Queries;;;;;
;;;;;;;;;;;;;;;

(define (true? node)
  (> (interval-s (tms-node-belief node)) (jbms-belief-threshold (tms-node-jbms node))))

(define (false? node)
  (> (interval-p (tms-node-belief node)) (jbms-belief-threshold (tms-node-jbms node))))

(define (unknown? node)
  (and
   (< (interval-s (tms-node-belief node)) (jbms-belief-threshold (tms-node-jbms node)))
   (< (interval-p (tms-node-belief node)) (jbms-belief-threshold (tms-node-jbms node)))
   ))

(define (absolutely-true? node)
  (define *jbms* (tms-node-jbms node))
  (<= (abs (- (interval-s (tms-node-belief node)) 1.0)) (jbms-belief-threshold *jbms*)))

(define (absolutely-false? node)
  (define *jbms* (tms-node-jbms node))
  (<= (abs (- (interval-p (tms-node-belief node)) 1.0)) (jbms-belief-threshold *jbms*)))

(define (absolutely-unknown? node)
  (define *jbms* (tms-node-jbms node))
  (define threshold (jbms-belief-threshold *jbms*))
  (and
   (<= (abs (- (interval-s (tms-node-belief node)) 0.0)) threshold)
   (<= (abs (- (interval-p (tms-node-belief node)) 0.0)) threshold)
   ))

(define (support-for? node)
  (interval-s (tms-node-belief node))
  )

(define (support-against? node)
  (interval-p (tms-node-belief node))
  )

(define (possible-true node)
  (- 1 (support-against? node))
  )

(define (possible-false node)
  (- 1 (support-for? node))
  )

(define (belief-uncertainty node)
  (- 1 (+ (support-against? node) (support-for? node)))
  )

   
