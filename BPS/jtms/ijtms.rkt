#lang racket

(require racket/set)
(require "utils.rkt")
(provide (all-defined-out))

(struct jtms
        (
         title
         nodes                    ;; list of all tms nodes
         justs                    ;; list of all justifications
         debugging                ;; debugging flag
         contradictions           ;; set of contradiction nodes
         assumptions              ;; set of assumption nodes
         checking-contradictions  ;; for external systems
         contradiction-handler
         node-label               ;; per node label: :IN means believed, :OUT means disbelieved
         node-justs               ;; per node possible justifications
         node-support             ;; per node current justification or premise marker
         node-consequences        ;; per node justifications in which it is an antecedent
         )
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<jtms ~a>" (jtms-title this)))]
        #:transparent
        )

(struct just ;;justification
        (
         informant
         consequence
         antecedents
         )
        #:methods gen:custom-write
        [(define (write-proc this port mode)
           (fprintf port "<just ~a>" (just-informant this)))]
        #:transparent
        )

(define (tms-node-support tms node)
  (hash-ref (jtms-node-support tms) node))

(define (tms-node-label tms node)
  (hash-ref (jtms-node-label tms) node))

(define (tms-node-justs tms node)
  (hash-ref (jtms-node-justs tms) node))

(define (tms-node-consequences tms node)
  (hash-ref (jtms-node-consequences tms) node))

(define (tms-node-contradictory? tms node)
  (set-member? (jtms-contradictions tms) node))

(define (tms-node-assumption? tms node)
  (set-member? (jtms-assumptions tms) node))

(define (tms-node-premise? tms node)
  (let ([support (tms-node-support tms node)])
    (and support ;; other than #f everything is true even '()
         (not (eq? support ':ENABLED-ASSUMPTION)) ;; :ENABLED-ASSUMPTION ?
         (null? (just-antecedents support)))))

;;; Simple utilities:
(define (node-string n)
  (format "~a" n))

(define-syntax debugging-jtms
  (syntax-rules ()
    [(_  jtms msg e* ...)
     (let ((args (list e* ...)))
       (when (jtms-debugging jtms)
         (apply printf msg args)))
     ]))

(define (tms-error proc string node)
  (error proc (format string node)))



(define (create-jtms title
                     #:debugging (debugging #f)
                     #:checking-contradictions (checking-contradictions #t)
                     #:contradiction-handler (contradiction-handler ask-user-handler))
  (jtms
   title
   '()
   '()
   debugging
   (set)
   (set)
   checking-contradictions
   contradiction-handler
   (per-node)
   (per-node)
   (per-node)
   (per-node)))

;;;;; Basic inference-engine interface ;;;;;;

(define  (in-node? tms node) (eq? (tms-node-label tms node) ':IN))
(define  (out-node? tms node) (eq? (tms-node-label tms node) ':OUT))

(define (tms-create-node tms node
                         #:assumptionp (assumptionp #f)
                         #:contradictoryp (contradictoryp #f))
  (if (member node (jtms-nodes tms))
      (tms-error 'tms-create-node "node ~a already in TMS" node)
      (struct-copy jtms tms
                   [nodes (cons node (jtms-nodes tms))]
                   [assumptions (add-if assumptionp node (jtms-assumptions tms))]
                   [contradictions (add-if contradictoryp node (jtms-contradictions tms))]
                   [node-label (hash-set (jtms-node-label tms) node ':OUT)]
                   [node-support (hash-set (jtms-node-support tms) node #f)]
                   [node-justs (hash-set (jtms-node-justs tms) node '())]
                   [node-consequences (hash-set (jtms-node-consequences tms) node '())])))

;;; Converts a regular node to an assumption and enables it.

(define (assume-node tms node)
  (let ((tms
         (if (or (tms-node-assumption? tms node) (tms-node-premise? node))
             tms
             (begin
               (debugging-jtms tms "\nConverting ~a into an assumption" node)
               (struct-copy jtms tms [assumptions (set-add (jtms-assumptions tms) node)])))))
    (enable-assumption tms node)))

(define (make-contradiction tms node)
  (let ((tms
         (if (tms-node-contradictory? tms node)
             tms
             (struct-copy jtms tms [contradictions (set-add (jtms-contradictions tms) node)]))))
    (check-for-contradictions tms)))

(define (justify-node tms informant consequence antecedents)
  (let ((just (just informant consequence antecedents))
        (per-node-consequences (jtms-node-consequences tms)))
    (for ((node antecedents)) (set! per-node-consequences (push-in per-node-consequences just node)))
    (debugging-jtms tms
                    "\nJustifying ~a by ~a using ~a."
                    consequence
                    informant
                    (map node-string antecedents))
    (let ((tms (struct-copy jtms tms
                            [node-consequences per-node-consequences]
                            [node-justs (push-in (jtms-node-justs tms) just consequence)]
                            [justs (cons just (jtms-justs tms))])))
      (let ((tms
             (if (or (not (null? antecedents)) (out-node? tms consequence))
                 (if (check-justification tms just)
                     (install-support tms consequence just)
                     tms)
                 (struct-copy jtms tms [node-support (hash-set (jtms-node-support tms) consequence just)]))))
        (check-for-contradictions tms)))))

;;;;;;;;;;;Support for adding justifications;;;;;;;;;;;;;;;;

(define (check-justification tms just)
  (and (out-node? tms (just-consequence just))
       (justification-satisfied? tms just)))

(define (justification-satisfied? tms just)
  (andmap (lambda (n) (in-node? tms n)) (just-antecedents just)))

(define (install-support tms conseq just)
  (propagate-inness (make-node-in tms conseq just) conseq))

(define (propagate-inness tms node)
  (let ((q (list node)))
    (do () ((begin (set! node (pop! q)) (null? node)))
      (debugging-jtms tms "\n   Propagating belief in ~a." node)
      (for ((justification (tms-node-consequences tms node)))
           (when (check-justification tms justification)
             (set! tms (make-node-in tms (just-consequence justification) justification))
             (push! (just-consequence justification) q)))))
  tms)

(define (make-node-in tms conseq reason)
  (debugging-jtms tms "\n     Making ~a in via ~a."
                  conseq
                  (if (symbol? reason)
                      reason
                      (cons (just-informant reason)
                            (map node-string
                                 (just-antecedents reason)))))
  (struct-copy jtms tms
               [node-label (hash-set (jtms-node-label tms) conseq ':IN)]
               [node-support (hash-set (jtms-node-support tms) conseq reason)]))

;;; Assumption Manipulation
(define (retract-assumption tms node)
  (when (eq? (tms-node-support tms node) ':ENABLED-ASSUMPTION)
    (debugging-jtms tms "\n  Retracting assumption ~a." node)
    (set! tms (make-node-out tms node))
    (let-values (((tms out-queue) (propagate-outness node tms)))
      (find-alternative-support tms (cons node out-queue)))))

(define (enable-assumption tms node)
  (unless (tms-node-assumption? tms node)
    (tms-error 'enabled-assumption "Can't enable the non-assumption ~a" node))
  (debugging-jtms tms "\n  Enabling assumption ~a." node)
  (cond ((out-node? tms node)
         (set! tms (propagate-inness (make-node-in tms node ':ENABLED-ASSUMPTION) node)))
        ((or (eq? (tms-node-support tms node) ':ENABLED-ASSUMPTION)
             (null? (just-antecedents (tms-node-support tms node)))))
        (else (set! tms (struct-copy jtms tms [node-support (hash-set (jtms-node-support tms) node ':ENABLED-ASSUMPTION)]))))
  (check-for-contradictions tms))

(define (make-node-out tms node)
  (debugging-jtms tms "\n     retracting belief in ~a." node)
  (struct-copy jtms tms
               [node-support (hash-set (jtms-node-support tms) node #f)]
               [node-label (hash-set (jtms-node-label tms) node ':OUT)]))

(define (propagate-outness node tms)
  (let ((out-queue '()))
    (debugging-jtms tms "\n   Propagating disbelief in ~a." node)
    (do ((js (tms-node-consequences tms node) (append (cdr js) newvar))
         (newvar '() '())
         (conseq #f))
        ((null? js) (values tms out-queue))
      ;; For each justification using the node, check to see if
      ;; it supports some other node.  If so, forget that node,
      ;; queue up the node to look for other support, and recurse
      (set! conseq (just-consequence (car js)))
      (when (eq? (tms-node-support tms conseq) (car js))
        (set! tms (make-node-out tms conseq))
        (push! conseq out-queue)
        (set! newvar (tms-node-consequences tms conseq))))))

(define (find-alternative-support tms out-queue)
  (debugging-jtms tms "\n   Looking for alternative supports.")
  (with-handlers ([just? (lambda (x) x)])
    (for ((node out-queue))
         (unless (in-node? tms node)
           (for ((just (tms-node-justs tms node)))
                (when (check-justification tms just)
                  (set! tms (install-support tms (just-consequence just) just))
                  (raise just))))))
  tms)

;;; Contradiction handling interface
(define (check-for-contradictions tms)
  (let ((contradictions '()))
    (when (jtms-checking-contradictions tms)
      (for ((cnode (jtms-contradictions tms)))
           (when (in-node? tms cnode) (push! cnode contradictions)))
      (unless (null? contradictions)
        ((jtms-contradiction-handler tms) tms contradictions))))
  tms)

;;; Well-founded support inqueries
(define (supporting-justification-for-node tms node) (tms-node-support tms node))

(define (assumptions-of-node tms node)
  (let ((assumptions '()) (marks (mutable-set)))
    (do ((nodes (list node) (append (cdr nodes) newvar))
         (newvar '() '()))
        ((null? nodes) assumptions)
      (let ((node (car nodes)))
        (cond ((set-member? marks node))
              ((eq? (tms-node-support tms node) ':ENABLED-ASSUMPTION)
               (push! node assumptions))
              ((in-node? tms node)
               (set! newvar (just-antecedents (tms-node-support tms node))))
              )
        (set-add! marks node)))))

(define (enabled-assumptions tms)
  (let ((result '()))
    (for ((assumption (jtms-assumptions tms)))
         (when (eq? (tms-node-support tms assumption) ':ENABLED-ASSUMPTION)
           (push! assumption result)))
    result))

;; Inference engine stub to allow this JTMS to be used stand alone

(define (why-node tms node)
  (let ((justification (tms-node-support tms node)))
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

(define (why-nodes tms)
  (for ((node (jtms-nodes tms))) (why-node tms node)))

(define *contra-assumptions* '())

(define (ask-user-handler tms contradiction)
  (set! tms (handle-one-contradiction tms (car contradiction)))
  (check-for-contradictions tms))

(define (handle-one-contradiction tms contra-node)
  (set! *contra-assumptions* (assumptions-of-node tms contra-node))
  (when (null? *contra-assumptions*)
      (tms-error 'handle-one-contradiction "\nThere is a flaw in the universe...~a" contra-node))
  (printf  "\nContradiction found: ~a" (node-string contra-node))
  (print-contra-list *contra-assumptions*)
  (printf "\nPick the <number> to retract assumption. ")
  (let ((the-answer (read)))
    (when (and (integer? the-answer)
               (> the-answer 0)
               (not (> the-answer (length *contra-assumptions*))))
      (retract-assumption tms (list-ref *contra-assumptions* (- the-answer 1))))))

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

(define (explore-network tms node)
  (cond
    ((not (in-node? tms node))
     (printf "\n Sorry, ~a not believed." (node-string node))
     node)
    (else
     (with-handlers
      ([(lambda (x) #t) (lambda (x) x)])
      (do ((stack '())
           (current node)
           (options '())
           (olen 0))
          ((null? current))
        (why-node tms current)
        (set! options (if (just? (tms-node-support tms current))
                          (just-antecedents (tms-node-support tms current))
                          '()))
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

;;;;; New Meta Facilities ;;;;;;

(define (if-ok tms op)
  (let ((ok #t))
    (let* ((observing-contradiction (lambda (tms contradiction)
                                      (printf "\nDetected contradiction on ~a.\n" contradiction)
                                      (set! ok #f)))
           (cautious-tms (struct-copy jtms tms [contradiction-handler observing-contradiction]))
           (result-tms (op cautious-tms)))
      (if ok
          (struct-copy jtms result-tms [contradiction-handler (jtms-contradiction-handler tms)])
          #f))))


(define (if-ok-justify-node tms informant consequence antecedents)
  (if-ok tms (lambda (tms) (justify-node tms informant consequence antecedents))))

(define (if-ok-enable-assumption tms node)
  (if-ok tms (lambda (tms) (enable-assumption tms node))))
