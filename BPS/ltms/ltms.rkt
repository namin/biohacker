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
    [(_ clause)
     (= (clause-pvs clause) 0)]))

(define-syntax walk-clauses
  (syntax rules ()
     [(_ ltms f)
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

;;; INSERT THE REST OF CODE HERE

;;; Inquiring about well-founded support

(define (support-for-node node)
  (match-define (list result support) (list '() '()))
  (cond ((empty? (tms-node-support node)) (set! support (tms-node-support node)))
	((equal? support ':ENABLED-ASSUMPTION) ':ENABLED-ASSUMPTION)
	(#t (for ((pair (clause-literals support)))
	     (unless (equal? (car pair) node)
	       (push (car pair) result)))
	   (values result (clause-informant support)))))

(define (assumptions-of-node node)
  (cond ((equal? ':ENABLED-ASSUMPTION (tms-node-support node)) (list node))
	((known-node? node)
	 (assumptions-of-clause (tms-node-support node)))))

(define (assumptions-of-clause in-clause) 
  (do ((clause-queue (list in-clause)
		     (append (cdr clause-queue) new-clauses))
       (mark (list nil)) ;; Doesn't make sense even in ltms.lisp file should be (list nil). To be changes (maybe) after Kat's work
       (node #f) 
       (new-clauses '() '()) 
       (assumptions '()))  
      ((empty? clause-queue) assumptions)
    (for ((term-pair (clause-literals (car clause-queue))))
      (set! node (car term-pair))
      (unless (equal? (tms-node-mark node) mark)
	(unless (equal? (tms-node-label node) (cdr term-pair))
	  (cond ((equal? ':ENABLED-ASSUMPTION (tms-node-support node))
		 (push node assumptions))
		((empty? (tms-node-support node)) (ltms-error "Node is unknown" node))
		(#t (push (tms-node-support node) new-clauses))))
	(set-tms-node-mark! node mark)))))

;;; Simple user interface

(define *contra-assumptions* '())

(define (ask-user-handler contradictions ltms)
  (for ((contradiction contradictions))
    (when (violated-clause? contradiction)
	(handle-one-contradiction contradiction))))

(define (handle-one-contradiction violated-clause) ;; TODO Apoorv
   (let ((*contra-assumptions* (assumptions-of-clause violated-clause))
         (the-answer #f))
      (when (empty? *contra-assumptions*) (ltms-error "Global contradiction"
                                    violated-clause))
      (format "~%Contradiction found:")
      (print-contra-list *contra-assumptions*)
      (format  "~%Call (TMS-ANSWER <number>) to retract assumption.")
      (set! the-answer
         (catch 'tms-contradiction-handler
            (cerror "Continue LTRE processing (after retracting an assumption)"
               "LTMS contradiction break")))
      (when the-answer
        (retract-assumption (list-ref *contra-assumptions* (- the-answer 1))))))


(define (print-contra-list nodes)
  (do ((counter 1 (+ 1 counter))
       (nn nodes (cdr nn)))
      ((empty? nn))
    (format "~%~A ~A" counter
	    (node-string (car nn)))))


(define (tms-answer num) ;; TODO Apoorv 
  (if (integer? num)
      (if (> num 0)
	  (if (not (> num (length *contra-assumptions*)))
	      (throw 'tms-contradiction-handler num)
	      (format  "~%Ignoring answer, too big."))
	  (format  "~%Ignoring answer, too small"))
      (format  "~%Ignoring answer, must be an integer.")))

(define (avoid-all contradictions ignore)
  (match-define (list culprits culprit sign) (list '() #f #f)) ;;
  (for ((contradiction contradictions))
    (when (violated-clause? contradiction)
      (set! culprits (assumptions-of-clause contradiction))
      (when (empty? culprits) 
	(ltms-error "Total contradiction" contradiction))
      (set! culprit (car culprits))
      (set! sign (tms-node-label culprit))
      (retract-assumption culprit)
      (add-nogood culprit sign culprits) ;; to be defined above Kat's 
      #t)))

(define (clause-antecedents clause)
  (let ((result '()))
  (for ((pair (clause-literals clause)))
    (unless (equal? (tms-node-support (car pair)) clause)
      (push (car pair) result)))
  result
  ))

(define (signed-node-string node)
  (if (true-node? node) (node-string node)
      (cond
       ((false-node? node) (format "Not[~A]" (node-string node)))
       (else (format "Unknown[~A]" (node-string node))))))

(define (node-consequences node)
  (match-define (list conseq conseqs) (list #f #f))
  (for ((cl (ecase (tms-node-label node)
		(':TRUE (tms-node-false-clauses node))
		(':FALSE (tms-node-true-clauses node)))))
    (unless (equal? cl (tms-node-support node))
      (set! conseq (clause-consequent cl))
      (when conseq (push conseq conseqs))))
  conseqs)

(define (why-node node)
  (cond ((unknown-node? node)
	 (format  "~A is unknown." (node-string node)))
	((equal? ':ENABLED-ASSUMPTION (tms-node-support node))
	 (format  "~A is ~A <~A>"
		 (node-string node)
		 (tms-node-label node) (tms-node-support node)))
	(else (format "~A is ~A via ~A on"
		   (node-string node)
		   (tms-node-label node)
		   (or (clause-informant (tms-node-support node))
		       (tms-node-support node)))
	   (for ((term-pair (clause-literals (tms-node-support node))))
	     (unless (equal? (tms-node-label (car term-pair))
			    (cdr term-pair))
	       (format "~A is ~A"
		       (node-string (car term-pair))
		       (tms-node-label (car term-pair)))))))
  node)

(define (why-nodes ltms)
  (hash-map  (ltms-nodes ltms) (lambda (ignore n) (why-node n))))

(define *line-count* 0)

(define (explain-node node)
  (define *line-count* #f)
  (unless (equal? (tms-node-label node) ':UNKNOWN)
    (set! *line-count* 0)
    (hash-map (lambda (ignore node) (set-tms-node-mark! node #f))
	   (ltms-nodes (tms-node-ltms node)))
    (explain-1 node)))


(define (explain-1 node)
  (define antecedents '())
  (cond ((tms-node-mark node))
	((equal ':ENABLED-ASSUMPTION (tms-node-support node))
         (if (true-node? node)
             (format "~A  ~A () Assumption"
                     (incf *line-count*) (node-string node))
             (format "~A (:NOT ~A)  () Assumption" (incf *line-count*) (node-string node)))
	 (set-tms-node-mark! node *line-count*))
	(else (set! antecedents
		 (map explain-1 (clause-antecedents (tms-node-support node))))
              (cond
                ((true-node? node) (format "~A ~A ~A" (incf *line-count*) (node-string node) antecedents))
                (else (format "~A (:NOT ~A) ~A" (incf *line-count*) (node-string node) antecedents)))

	   (pretty-print-clause (tms-node-support node))
	   (set-tms-node-mark node *line-count*))))

(define (pretty-print-clauses ltms)
  (walk-clauses ltms (lambda (l)
                       (pretty-print-clause l))))

(define (pretty-print-clause clause)
  (format "(:OR")
  (for ((literal (clause-literals clause)))
    (if (equal? ':TRUE (cdr literal))
        (format " ~A" (node-string (car literal)))
	(format "(:NOT ~A)" (node-string (car literal)))))
  (format  ")"))


(define (show-node-consequences node)
  (let ((conseqs (node-consequences node)))
    (cond ((not (empty? conseqs)) 
	   (printf (format  "~% Consequences of ~A:" (signed-node-string node)))
	   (for ((conseq conseqs))
		   (printf (format  "~%  ~A" (signed-node-string conseq)))))
	  (else (printf (format "~% ~A has no consequences." (node-string node)))))))

(define (node-show-clauses node)
  (printf (format  "For ~A:" (node-string node)))
  (for ((cl (tms-node-true-clauses node)))
    (printf (format T "~%")) (pretty-print-clause cl))
  (for ((cl (tms-node-false-clauses node)))
    (printf (format T "~%")) (pretty-print-clause cl)))


(define (explore-network node)
  (with-handlers ((ret (lambda (x) x)))
  (unless (known-node? node)
	  (printf (format "~% Sorry, ~A not believed." (node-string node)))
	  (raise node))
  (do ((stack '())
       (current node)
       (mode ':ante)
       (options '())
       (olen 0)
       (done? #f))
      (done? current)
      (cond ((equal? mode ':ante)
	     (why-node current)
	     (set! options (if (clause? (tms-node-support current))
			       (clause-antecedents (tms-node-support current))
			     '())))
	    (else ;; Looking at consequences
	     (show-node-consequences current)
	     (set! options (node-consequences current))))
      (set! olen (length options))
      (do ((good? nil)
	   (choice 0))
	  (good? (case good?
		       ((q) (raise current))
		       ((c) (set! mode ':conseq))
		       ((a) (setq mode ':ante))
		       ((0) (unless (empty? stack)
			      (set! current (pop stack))
			      (raise current)))
		       ((#t) (push current stack)
			  (set! current (list-ref options (- good? 1))))))
	  (printf "\n>>>")
	  (set! choice (read))
	  (if (or (equal? choice 'q)
		  (equal? choice 'c)
		  (equal? choice 'a)
		  (and (integer? choice)
		       (not (> choice olen))
		       (not (< choice 0))))
	      (set! good? choice)
	      (printf (format "~% Must be q, a, c or an integer from 0 to ~D."
		        olen)))))))

;;;; Add-on utilities for easier lisp to racket translation;;;;;;;
(define-syntax-rule (incf x)
  (begin
    (set! x (+ 1 x))
  x)
  )

(define (ret x)
  #t)
(define (is-true X)
(define-syntax-rule (push val lst)
  (set! lst (cons val lst))))

(define-syntax-rule (pop lst)
  (begin
    (define popped (car lst))
  (set! lst (cdr lst))
  popped
  )
  )
(define-syntax-rule (pushnew val lst)
  (unless (member? val lst) (set! lst (cons val lst))))