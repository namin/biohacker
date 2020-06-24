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

(define (default-node-string n)
  (format "~a" (tms-node-datum n)))

(define (ask-user-handler jtms contradiction)
  'TODO)

(define (create-jtms title
		     #:node-string (node-string default-node-string)
		     #:debugging (debugging #f)
		     #:checking-contradictions (checking-contradictions #f)
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

(define (push-jtms-assumptions! node jtms)
  (set-jtms-assumptions! jtms (cons node (jtms-assumptions jtms))))

(define (push-jtms-contradictions! node jtms)
  (set-jtms-contradictions! jtms (cons node (jtms-contradictions jtms))))

(define (push-jtms-nodes! node jtms)
  (set-jtms-nodes! jtms (cons node (jtms-nodes jtms))))

(define (tms-create-node jtms datum
			 #:assumptionp (assumptionp #f)
			 #:contradictoryp (contradictoryp #f))
  (let ((counter (jtms-node-counter jtms)))
    (set-jtms-node-counter! jtms (+ counter 1))
    (let ((node (tms-node counter
			  datum
			  ':OUT
			  '()
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

;;; Simple utilities:

(define (node-string node)
  ((jtms-node-string (tms-node-jtms node)) node))
