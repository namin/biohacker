#lang racket

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
(define (make-just   #:INDEX (INDEX 0)
                     #:INFORMANT (informant 'NA)
                     #:CONSEQUENCE (consequence 'NA)
                     #:ANTECEDENTS (antecedents 'NA))

  (just
   INDEX
   informant
   consequence
   antecedents
   ))



(define (tms-node-premise? node)
  (let ([support (tms-node-support node)])
    (and support ;; other than #f everything is true even '()
         (not (equal? support ':ENABLED-ASSUMPTION)) ;; :ENABLED-ASSUMPTION ?
         (or (false? just-antecedents support) (empty? (just-antecedents support)))
         )
    )
  )
;;; Simple utilities:

(define (node-string node)
  (apply (eval (jtms-node-string (tms-node-jtms node))) (list node)))

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

(define (tms-error string node)
  (error string (node-string node)))

(define (default-node-string n)
  (format "~a" (tms-node-datum n))
  )


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

(define  (in-node? node) (equal? (tms-node-label node) ':IN))
(define  (out-node? node) (equal? (tms-node-label node) ':OUT))

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
  (enable-assumption node)
  (void))

(define (make-contradiction node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-contradictory? node)
      (set-tms-node-contradictory?! node #t)
      (push-jtms-contradictions! node jtms))
    (check-for-contradictions jtms)
    )
  (void))


(define (justify-node informant consequence antecedents)
  (let* (
         (jtms (tms-node-jtms consequence))
         (_ (set-jtms-just-counter! jtms (+ 1 (jtms-just-counter jtms))))
         (just (make-just #:INDEX (jtms-just-counter jtms)
                          #:INFORMANT informant
                          #:CONSEQUENCE consequence
                          #:ANTECEDENTS antecedents))
         )
    (push-tms-node-justs! just consequence)
    (for ((node antecedents)) (push-tms-node-consequences! just node))
    (push-jtms-justs! just jtms)
    (debugging-jtms jtms
                    "\nJustifying ~a by ~a using ~a."
                    consequence
                    informant
                    (map node-string antecedents))
    (if (or antecedents (out-node? consequence))
        (when (check-justification just) (install-support consequence just))
        (set-tms-node-support! consequence just))
    (check-for-contradictions jtms)
    (void))
  )

;;;;;;;;;;;Support for adding justifications;;;;;;;;;;;;;;;;

(define (check-justification just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

(define (justification-satisfied? just)
  (andmap in-node? (just-antecedents just))
  )
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
             (push! (just-consequence justification) q)))) (void)))

(define (make-node-in conseq reason )
  (let* ((jtms (tms-node-jtms conseq))
         (enqueuef (jtms-enqueue-procedure jtms)))
    (set-tms-node-jtms! conseq jtms)
    (debugging-jtms jtms "\n     Making ~a in via ~a."
                    conseq
                    (if (symbol? reason)
                        reason
                        (cons (just-informant reason)
                              (map (eval (jtms-node-string jtms)) ;; assuming jtms-node-string gives symbols
                                   (just-antecedents reason)))))
    (set-tms-node-label! conseq ':IN)
    (set-tms-node-support! conseq reason)
    (when enqueuef
      (for ((in-rule (tms-node-in-rules conseq)))
           (apply (eval enqueuef) (list in-rule))) ;; verify?
      (set-tms-node-in-rules! conseq #f)) ;; #f or '() lisp both have nil, racket only #f is false
    (void)
    )
  )




;;; Assumption Manipulation
(define (retract-assumption node)
  (let ((jtms #f))
    (when (equal? (tms-node-support node) ':ENABLED-ASSUMPTION)
      (set! jtms (tms-node-jtms node))
      (debugging-jtms jtms "\n  Retracting assumption ~a." node)
      (make-node-out node)
      (find-alternative-support jtms (cons node (propagate-outness node jtms))))
    (void)))

(define (enable-assumption node)
  (let ((jtms (tms-node-jtms node)))
    ;;(unless (tms-node-assumption? node)
    ;;(tms-error "Can't enable the non-assumption ~a" node) tms-error not implemented
    ;;)
    (debugging-jtms jtms "\n  Enabling assumption ~a." node)
    (cond (
           (out-node? node) (make-node-in node ':ENABLED-ASSUMPTION)
           (propagate-inness node))
          ((or (equal? (tms-node-support node) ':ENABLED-ASSUMPTION)
               (null? (just-antecedents (tms-node-support node)))))
          (#t (set-tms-node-support! node ':ENABLED-ASSUMPTION)))
    (check-for-contradictions jtms)
    (void) ))

(define (make-node-out node)
  (let ((jtms 'NA) (enqueuef 'NA))
    (set! jtms (tms-node-jtms node))
    (set! enqueuef (jtms-enqueue-procedure jtms))
    (debugging-jtms jtms "\n     retracting belief in ~a." node)
    (set-tms-node-support! node #f)
    (set-tms-node-label! node ':OUT)
    (when enqueuef (for ((out-rule (tms-node-out-rules node)))
                        (apply (eval enqueuef) (list out-rule)))) ;; map..
    (set-tms-node-out-rules! node #f)
    (void)
    )
  )
(define (propagate-outness node jtms)
  (let ((out-queue 'NA))
    (debugging-jtms jtms "\n   Propagating disbelief in ~a." node)
    (do ((js (tms-node-consequences node) (append (cdr js) newvar))
         (newvar '() '())
         (conseq #f))
        ((null? js) out-queue)
      ;;For each justification using the node, check to see if
      ;;it supports some other node.  If so, forget that node,
      ;;queue up the node to look for other support, and recurse
      (set! conseq (just-consequence (car js)))
      (when (equal? (tms-node-support conseq) (car js))
        (make-node-out conseq)
        (push! conseq out-queue)
        (set! newvar (tms-node-consequences conseq))))
    (void)))

(define (find-alternative-support jtms out-queue)
  (debugging-jtms jtms "\n   Looking for alternative supports.")
  (for ((node out-queue))
       (unless (in-node? node)
         (for ((just (tms-node-justs node)))
              (when (check-justification just)
                (install-support (just-consequence just)
                                 just)

                ;;(return just) ;; return from for((just...
                )))))

;;; Contradiction handling interface
(define (check-for-contradictions jtms)
  (let ((contradictions '()))
    (when (jtms-checking-contradictions jtms)
      (for ((cnode (jtms-contradictions jtms)))
           (when (in-node? cnode) (push! cnode contradictions)))
      (unless (empty? contradictions)
        (apply (eval (jtms-contradiction-handler jtms)) (list jtms contradictions)))
      )
    (void)))

(define-syntax-rule (without-contradiction-check jtms &body body)
  (contradiction-check jtms #f body))

(define-syntax-rule (with-contradiction-check jtms &body body) ;;&body=&rest how in racket?
  (contradiction-check jtms #t body))

#|(define (contradiction-check jtms flag body) ;; '(let *... )?
(let ((jtmsv (gensym)) (old-value (gensym)))
`(let* ((,jtmsv ,jtms) ;; ??
(,old-value (jtms-checking-contradictions ,jtmsv)))
(unwind-protect
(progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
(setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))|#

(define-syntax-rule (with-contradiction-handler jtms handler &body body) ;; ?'
  (let ((jtmsv (gensym)) (old-handler (gensym)))
    `(let* ((,jtmsv ,jtms)
            (,old-handler (jtms-contradiction-handler ,jtmsv)))
       (unwind-protect
        (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
        (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

#|(define (default-assumptions jtms)
(with-contradiction-check jtms  ;; body optional arg not present
(with-contradiction-handler jtms #'(lambda (&rest ignore)
(declare (ignore ignore))
(throw 'CONTRADICTION t))
(for ((assumption (jtms-assumptions jtms)))
(cond ((equal? (tms-node-support assumption) ':ENABLED-ASSUMPTION))
((not (equal? ':DEFAULT (tms-node-assumption? assumption))))
((catch 'CONTRADICTION (enable-assumption assumption))
(retract-assumption assumption))))))

)|#

;;; Well-founded support inqueries
(define (supporting-justification-for-node node) (tms-node-support node))

(define (assumptions-of-node node)
  (let ((assumptions '()) (marker (list ':MARK)))
    (do ((nodes (list node) (append (cdr nodes) newvar))
         (newvar '() '()))
        ((null? nodes) assumptions)
      (let ((node (car nodes)))
        (cond ((equal? (tms-node-mark node) marker))
              ((equal? (tms-node-support node) ':ENABLED-ASSUMPTION)
               (push! node assumptions))
              ((in-node? node)
               (set! newvar (just-antecedents (tms-node-support node))))
              )
        (set-tms-node-mark! node marker)
        (void))
      )
    (void)
    ))

(define (enabled-assumptions jtms)
  (let ((result '()))
    (for ((assumption (jtms-assumptions jtms))) ;; ? last param
         (when (equal? (tms-node-support assumption) ':ENABLED-ASSUMPTION)
           (push! assumption result)))
    result
    )
  )

;;Inference engine stub to allow this JTMS to be used stand alone

(define (why-node node)
  (let ((justification (tms-node-support node)))
    (cond
      ((equal?  justification ':ENABLED-ASSUMPTION)
       (format "\n~a is an enabled assumption"
               (node-string node)))
      (justification ;; right condition?
       (format "\n~a is IN via ~a on"
               (node-string node)
               (just-informant justification)
               )
       (for ((anode (just-antecedents justification)))
            (format  "\n  ~a" (node-string anode))))
      (#t (format "\n~a is OUT." (node-string node))))
    node)
  )

(define (why-nodes jtms)
  (for ((node (jtms-nodes jtms))) (why-node node))
  )

(define *contra-assumptions* '()) ;; rather than proclaim special ?
;; proclaim '(special *contra-assumptions*)

(define (ask-user-handler jtms contradiction)
  (handle-one-contradiction (car contradiction))
  (check-for-contradictions jtms)
  )

(define (handle-one-contradiction contra-node)
  (let  ([*contra-assumptions* (assumptions-of-node contra-node)])
    #|[the-answer (catch 'tms-contradiction-handler
    (break "JTMS contradiction break"))]|#

    (unless *contra-assumptions*
      (tms-error "\nThere is a flaw in the universe...~a" contra-node))
    (format  "\nContradiction found: ~a" (node-string contra-node))
    (print-contra-list *contra-assumptions*)
    (format "\nCall (TMS-ANSWER <number>) to retract assumption.")
    #|(if (and (integer? the-answer)
    (> the-answer 0)
    (not (> the-answer (length *contra-assumptions*))))
    (retract-assumption (nth (1- the-answer)
    *contra-assumptions*)))|#
    )
  (void)
  )

(define (print-contra-list nodes)
  (do ((counter 1 (+ 1 counter))
       (nn nodes (cdr nn)))
      ((null? nn))
    (println (format "\n~a ~a" counter
                     (node-string (car nn)))))
  )

(define (tms-answer num)
  (if (integer? num)
      (if (> num 0)
          (if (not (> num (length *contra-assumptions*)))
              (raise 'tms-contradiction-handler num) ;; verify this
              (format "\nIgnoring answer, too big."))
          (format "\nIgnoring answer, too small"))
      (format "\nIgnoring answer, must be an integer.")))

(define (explore-network node)
  (cond
    ((not (in-node? node))
     (format  "\n Sorry, ~a not believed." (node-string node)) ;; print? before format
     node)
    (else (do ((stack '())
               (current node)
               (options '())
               (olen 0)
               (done? #f))
              ((done? current))
            (why-node current)
            (set! options (when (just? (tms-node-support current)) ;;
                            (just-antecedents (tms-node-support current))))
            (set! olen (length options))
            (do ((good? #f)
                 (choice 0))
                (good? (case (good?)
                         ;;((q) (return-from explore-network current)) ;;return-from?
                         ;;((0) (if stack
                         ;;      (set! current (pop! stack))
                         ;;    (return-from explore-network current)))
                         ((#t) (push! current stack)
                          (set! current (list-ref (- good? 1) options)))))
              (format "\n>>>")
              (set! choice (read))
              (cond ((or (equal? choice 'q)
                         (and (integer? choice)
                              (<= choice olen))
                         (>= choice 0))
                     (set! good? choice))
                    (#t (format
                         "\n Must be q or an integer from 0 to ~a."
                         olen))))))))


(define (push-jtms-assumptions! node jtms)
  (set-jtms-assumptions! jtms (cons node (jtms-assumptions jtms))))

(define (push-jtms-contradictions! node jtms)
  (set-jtms-contradictions! jtms (cons node (jtms-contradictions jtms))))

(define (push-jtms-nodes! node jtms)
  (set-jtms-nodes! jtms (cons node (jtms-nodes jtms))))

(define (push-tms-node-justs! node justs)
  (set-tms-node-justs! justs (cons node (tms-node-justs justs))))
(define (push-tms-node-consequences! just node)
  (set-tms-node-consequences! node (cons just (tms-node-consequences node)))
  )

(define (push-jtms-justs! just jtms)
  (set-jtms-justs! jtms (cons just (jtms-justs jtms)))
  )



(define-syntax-rule (push! val lst) ;;pushes val to list lst at first pos
  (set! lst (cons val lst))
  )
(define-syntax-rule (pop! lst) ;; pops the first position
  (cond
    ((null? lst) '())
    (else (let ((popped (car lst))) (set! lst (rest lst)) popped))
    )
  )
