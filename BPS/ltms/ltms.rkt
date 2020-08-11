#|
Code translated from BPS ltms.lisp http://www.qrg.northwestern.edu/BPS/ltms/ltms.lisp
|#
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
         contradiction-handlers ;;nil)
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
           (fprintf port "<Clause ~a>" (clause-index this)))]
        )

(define (make-clause #:index (index 0)
                     #:informant informant
                     #:literals literals
                     #:pvs (pvs 0)
                     #:length (length 0)
                     #:sats (sats 0)
                     #:status (status #f))

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

(define (walk-trie function trie)
  (when trie
    (if (and (not (list? trie)) (not (empty? trie)))
	(function trie)
	(for ((entry trie)) (walk-trie function (cdr entry)))))
  )
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
  (syntax-rules ()
    [(_ clause)
     (= (clause-pvs clause) 0)]))

(define-syntax walk-clauses
  (syntax-rules ()
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
                     #:contradiction-handler (contradiction-handler #f)
                     #:node-string (node-string #f)
                     #:enqueue-procedure (enqueue-procedure #f)
                     #:debugging (debugging #f)
                     #:checking-contradictions (checking-contradictions #f)
                     #:complete (complete #f)
                     #:delay-sat (delay-sat #f)
                     )
  (when node-string (set-ltms-node-string! ltms node-string))
  (when  debugging (set-ltms-debugging! ltms debugging))
  (when checking-contradictions
    (set-ltms-checking-contradictions! ltms
                                       checking-contradictions))
  (when contradiction-handler
    (set-ltms-contradiction-handlers! ltms contradiction-handler))
  (when enqueue-procedure
    (set-ltms-enqueue-procedure! ltms enqueue-procedure))
   (when complete
    (set-ltms-complete! ltms complete))
   (when delay-sat
    (set-ltms-delay-sat! ltms delay-sat)))

(define  (unknown-node? node) (equal? (tms-node-label node) ':UNKNOWN))
(define  (known-node? node) (not (equal? (tms-node-label node) ':UNKNOWN)))
(define  (true-node? node) (equal? (tms-node-label node) ':TRUE))
(define  (false-node? node) (equal? (tms-node-label node) ':FALSE))


;;; TODO Apoorv

(define (tms-create-node ltms datum #:assumptionp (assumptionp #f))
  (when (and (ltms-nodes ltms) (hash-ref  (ltms-nodes ltms) datum #f))
    (ltms-error "Two nodes with same datum:" datum))
  (set-ltms-node-counter! ltms (+ 1 (ltms-node-counter ltms)))
  (let ((node (tms-node  (ltms-node-counter ltms)
                         datum
                         ':UNKNOWN
                         '()
                         '()
                         '()
                         '()
                         assumptionp
                         '()
                         '()
                         ltms
                         '()
                         '())))
    (set-tms-node-true-literal! node (cons node ':TRUE))
    (set-tms-node-false-literal! node (cons node ':FALSE))
    (when (ltms-nodes ltms) ;; Insert if locally caching
       (hash-set! (ltms-nodes ltms) datum node))
    (when (and (ltms-complete ltms)
	       (> (ltms-node-counter ltms) (ltms-cons-size ltms)))
      (set-ltms-conses! ltms '())
      (set-ltms-cons-size! ltms (+ 50 (ltms-cons-size ltms)))
      (for ((i (ltms-cons-size ltms)))
        (define lst (ltms-conses ltms))
	(push '(()) lst) ;; may be (list #f)
        (set-ltms-conses! ltms lst)))
    node))

(define (enable-assumption node label)
  (cond ((not (tms-node-assumption? node))
	 (ltms-error "Can't enable the non-assumption ~A" node))
	((equal? (tms-node-label node) label)
	 (set-tms-node-support! node ':ENABLED-ASSUMPTION))
	((equal? (tms-node-label node) ':UNKNOWN)
	 (top-set-truth node label ':ENABLED-ASSUMPTION))
	(else (ltms-error "Can't set an already set node" node))))

(define (convert-to-assumption node)
  (unless (tms-node-assumption? node)
    (debugging-ltms (tms-node-ltms node)
		    "\nConverting ~A into an assumption" node)
    (set-tms-node-assumption?! node #t)))


(define (retract-assumption node)
  (when (and (known-node? node)
	     (equal? (tms-node-support node) ':ENABLED-ASSUMPTION))
    (find-alternative-support (tms-node-ltms node)
			      (propagate-unknownness node))))

;;; Adding formulas to the LTMS.

(define (add-formula ltms formula (informant #f))
  (cond
    (informant (set! informant (list ':IMPLIED-BY formula informant)))
    (else (set! informant (list ':IMPLIED-BY formula))))
  (for ((clause (normalize ltms formula)))
    (set! clause (simplify-clause clause))
    (unless (equal? ':TRUE clause)
	(add-clause-internal clause informant #t)))
  (check-for-contradictions ltms))

(define (simplify-clause literals)
  (with-handlers ((ret (lambda (x) x)))
    (set! literals (sort-clause literals))
    (do ((tail literals next)
         (next (cdr literals) (cdr next)))
        ((null? next) literals)
      (cond ((not (equal? (caar tail) (caar next))))
            ((not (equal? (cdar tail) (cdar next)))
             (raise ':TRUE))
            (else (rplacd tail (cdr next)))))))


(define (sort-clause literals)
   (sort  literals ;; Avoids shared structure bugs.
        < #:key (lambda (n) (tms-node-index (car n)))))

(define *ltms* #f) 
(define (normalize ltms exp_) (set! *ltms* ltms)(normalize-1 exp_ #f))

(define (normalize-1 exp_ negate_) ;; TODO Apoorv
  (case (and (list? exp_) (car exp_))
    ((:IMPLIES) (if negate_
		  (append (normalize-1 (cadr exp_) #f)
			 (normalize-1 (caddr exp_) #t))
		  (disjoin (normalize-1 (cadr exp_) #t)
			   (normalize-1 (caddr exp_) #f))))
    ((:IFF) (normalize-iff exp_ negate_))
    ((:OR) ( if negate_ (normalize-conjunction exp_ #t)
               (normalize-disjunction exp_ #f)))
    ((:AND) (if negate_ (normalize-disjunction exp_ #t)
	             (normalize-conjunction exp_ #f)))
    ((:NOT) (normalize-1 (cadr exp_) (not negate_)))
    ((:TAXONOMY) (normalize-tax exp_ negate_))
    (else (if negate_ `((,(tms-node-false-literal (find-node *ltms* exp_))))
	          `((,(tms-node-true-literal (find-node *ltms* exp_))))))))


(define (normalize-tax exp_ negate_) ;;TODO Apoorv
  (normalize-1 `(':AND (':OR ,@(cdr exp_)) ;one must be true
                 ;; The list is copied above to prevent very nasty bugs, since
                 ;; the rest of normalize side effects structure continually for
                 ;; efficiency.
                 ,@(do ((firsts (cdr exp_) (cdr firsts))
		       (rests (cddr exp_) (cdr rests))
		       (result '()))
		      ((null? rests) result)
		    (for ((other rests))
		      (push `(':NOT (':AND ,(car firsts) ,other))
			    result))))
	       negate_))

(define (normalize-conjunction exp_ negate_) ;; TODO Apoorv
  (map (lambda (sub) (normalize-1 sub negate_)) (cdr exp_)))


(define (normalize-iff exp_ negate_) ;; TODO Apoorv
  (append (normalize-1 `(':IMPLIES ,(cadr exp_) ,(caddr exp_)) negate_)
	 (normalize-1 `(':IMPLIES ,(caddr exp_) ,(cadr exp_)) negate_)))

(define (normalize-disjunction exp_ negate_) ;; TODO Apoorv
  (with-handlers ((ret (lambda (x) x)))
                  (unless (cdr exp_)
                    (raise (list '())))
                  (do ((result (normalize-1 (cadr exp_) negate_))
                       (rest_ (cddr exp_) (cdr rest_)))
                      ((null? rest_) result)
                    (set! result (disjoin (normalize-1 (car rest_) negate_) result)))))


(define (disjoin conj1 conj2) ;; ? 
  (with-handlers ((ret (lambda (x) x)))
    (unless (or conj1 conj2) (raise disjoin '()))
  (car (map (lambda (disj1)
	    (map (lambda (disj2) (append disj1 disj2))
		    conj2))
	  conj1))))   

(define (find-node ltms name)
  (cond ((tms-node? name) name)
	((when (ltms-nodes ltms) (hash-ref  (ltms-nodes ltms) name #f)) (hash-ref (ltms-nodes ltms) name ) )
	((tms-create-node ltms name))))

(define-syntax compile-formula ;; ?? 
  (syntax-rules ()
    ((_ run-tms e* ...)
     (let* ((args (list e* ...)) (f (car args)) (informant (cdr args)) (ltms #f))
       (set! ltms (create-ltms f))
       (add-formula ltms (expand-formula f))
       (generate-code ltms run-tms (when informant `(':IMPLIED-BY ,f ,informant)))))))


;;;;

(define (generate-code ltms run-tms informant) ;; ?? 
  (match-define (list result bound datum) (#f #f #f))
  (hash-map (ltms-nodes ltms) (lambda (ignore symbol)
	       (when (or (tms-node-true-clauses symbol)
			 (tms-node-false-clauses symbol))
		 (set! datum (tms-node-datum symbol))
		 (when (list? datum)
		   (set-tms-node-mark! symbol datum)
		   (set-tms-node-datum! symbol
			 (string->symbol (format  "~A" (cadr datum))))
		   (push symbol bound))))
	   )
  (walk-clauses ltms
		(lambda (clause)
                  (let ((ps '()) (ns '()))
		    (for ((lit (clause-literals clause)))
		      (if (equal? (cdr lit) ':TRUE)
			  (push (tms-node-datum (car lit)) ps)
			  (push (tms-node-datum (car lit)) ns)))
		    (push (add-clause ps ns informant)
			  result))))
  (map (lambda (s) ((tms-node-datum s) (find-node run-tms (tms-node-mark s)))) bound)
  result)

(define (macroexpand x) ;; ?? 
  (syntax->datum (expand (datum->syntax #f x)))
  )

(define (expand-formula x) ;; ??? 
  (set! x (macroexpand x))
  (cond ((not (list? x)) x)
	((case (macroexpand (car x))
	   ((QUOTE quote) (partial (cadr x)))
	   ((LIST list) (map expand-formula (cdr x)))
	   ((LIST* list*) (if (cddr x)
		      (cons (expand-formula (cadr x))
			    (expand-formula `(LIST* .,(cddr x))))
		      (expand-formula (cadr x))))
	   ((CONS cons) (cons (expand-formula (cadr x))
		       (map expand-formula (caddr x))))))
	(else x)))


(define (partial x)
  (cond ((null? x) x)
	((keyword? x) x)
	((not (list? x)) (string->symbol x)) ;; Convert anything to symbol ? 
	(else (cons (partial (car x)) (partial (cdr x))))))


;;;Misc (cltms.lisp) ;;;;;
(define (remove-subsumed-1 function lits trie)
  void 
 #| (define au #f)
  (cond ((null? lits) (walk-trie function trie) #t)
	((atom trie) nil)
	(#t (setq au (tms-node-index (caar lits)))
	   (do ((subtrie trie)
		(entry nil)
		(previous nil))
	       ((null subtrie))
	     (setq entry (car subtrie))
	     (if (cond ((>= (tms-node-index (caar entry)) au)
			(cond ((eq (car lits) (car entry))
			       (remove-subsumed-1 function (cdr lits) (cdr entry)))
			      ((> (tms-node-index (caar entry)) au) (return nil))))
		       (t (remove-subsumed-1 function lits (cdr entry))))
		 (cond ((null (cdr trie)) (return T))
		       (previous (rplacd previous (cdr subtrie))
				 (setq subtrie (cdr subtrie)))
		       (t (rplaca subtrie (cadr subtrie))
			  (rplacd subtrie (cddr subtrie))))
		 (setq previous subtrie subtrie (cdr subtrie))))))
|#
)

(define (remove-subsumed function lits ltms)
  (when (remove-subsumed-1 function lits (ltms-clauses ltms))
      (set-ltms-clauses! ltms '())))

;;;Processing Clauses (cltms.lisp) ;;;;;
(define (remove-clause old-clause cl)
  void
  ;;
  )
(define (process-clause ltms literals informant internal)
  (define cl #f)
  (set! cl (bcp-add-clause ltms literals informant #f))
  (remove-subsumed (lambda (old-clause)
		       (remove-clause old-clause cl))
		   literals ltms)
  (add-to-trie cl ltms)
  (cond (internal)
	((delay-sat? cl ltms)
	 (index-clause cl ltms))
	(#t (index-clause cl ltms)
         (insert-queue cl ltms))))

;;; Adding clauses
(define (install-clause ltms literals informant) ;; from cltms.lisp
  (unless (subsumed? literals (ltms-clauses ltms))
    (process-clause ltms literals informant '())))

(define (add-clause true-nodes false-nodes (informant #f))
  (add-clause-internal (append (map tms-node-true-literal true-nodes)
			      (map tms-node-false-literal false-nodes))
		       informant
		       #f))


(define (full-add-clause ltms literals informant)
   (when (and (install-clause ltms literals informant)
	     (not (equal? (ltms-complete ltms) ':DELAY)))
    (check-for-contradictions ltms)
    (ipia ltms)) ;; ipia given in cltms
  ;; Given in cltms
  )

(define (add-clause-internal literals informant internal)
  (define ltms #f)
  (set! ltms (tms-node-ltms
	       (or (caar literals)
		   (ltms-error "Total contradiction: Null clause" informant))))
  (if (ltms-complete ltms)
      (full-add-clause ltms literals informant)
      (begin
       (let ((lst (ltms-clauses ltms)))
         (push (bcp-add-clause ltms literals informant) lst)
         (set-ltms-clauses! ltms lst))))
  (unless internal (check-for-contradictions ltms)))

(define (bcp-add-clause ltms literals informant (index #t))
  (match-define (list cl label) (list #f #f))
  (set-ltms-clause-counter! ltms (+ 1 (ltms-clause-counter ltms)))
  (set! cl (make-clause #:index (ltms-clause-counter ltms)
			#:literals literals
			#:informant informant
			#:length (length literals)))
 
  (for ((term literals))
    (set! label (tms-node-label (car term)))
    (when (equal? ':UNKNOWN label)
	(set-clause-pvs! cl (+ 1 (clause-pvs cl))))
    (case (cdr term)
      ((:TRUE)
	(when index (insert-true-clause cl (car term)))
	(when (equal? label ':TRUE)
	  (set-clause-sats! (+ 1 (clause-sats cl)))
          (set-clause-pvs! (+ 1 clause-pvs cl))))
      ((:FALSE)
       (when index (insert-false-clause cl (car term)))
       (when (equal? label ':FALSE)
	 (set-clause-sats! (+ 1 (clause-sats cl)))
         (set-clause-pvs! (+ 1 clause-pvs cl))))))
  (when index (check-clauses ltms (list cl)))
  cl)

(define (insert-true-clause cl node)
  (define lst (tms-node-true-clauses node))
  (push cl lst)
  (set-tms-node-true-clauses! node lst))

(define (insert-false-clause cl node)
 (define lst (tms-node-false-clauses node))
  (push cl lst)
  (set-tms-node-false-clauses! node lst))

(define (add-nogood culprit sign assumptions (informant 'NOGOOD))
  (match-define (list trues falses) (#f #f))
  (for ((a assumptions))
    (case (if (equal? a culprit) sign (tms-node-label a))
      ((:TRUE) (push a falses))
      ((:FALSE) (push a trues)))
    )
    (add-clause trues falses informant)
    )

(define *clauses-to-check* '())

(define (check-clauses ltms clauses-to-check)
  (set! *clauses-to-check* clauses-to-check)
  (debugging-ltms ltms "\n Beginning propagation...")
  (do () ((null? *clauses-to-check*))
    (check-clause ltms (pop *clauses-to-check*))))

(define (check-clause ltms clause)
  (define unknown-pair '())
  (cond ((violated-clause? clause)
         (define lst (ltms-violated-clauses ltms))
	 (pushnew clause lst)
         (set-ltms-violated-clauses! ltms lst))
	((= (clause-pvs clause) 1)
	 ;; Exactly one term of the clause remains that can
	 ;; satisfy the clause, so deduce that term
	 (set! unknown-pair (find-unknown-pair clause))
	 (when (is-true unknown-pair) ;must check, because it might have other
	   (set-truth (car unknown-pair) ; support
		      (cdr unknown-pair) clause)))))


(define (find-unknown-pair clause)
  (with-handlers ((ret (lambda (x) x)))
    (for ((term-pair (clause-literals clause)))
      (when (unknown-node? (car term-pair)) (raise term-pair)))))

(define (top-set-truth node value reason)
  (set! *clauses-to-check* '())
  (set-truth node value reason)
  (check-clauses (tms-node-ltms node) *clauses-to-check*)
  (check-for-contradictions (tms-node-ltms node)))

(define (set-truth node value reason)
  (define ltms  (tms-node-ltms node))
  (define enqueuef (ltms-enqueue-procedure ltms))
  (debugging-ltms
    ltms "\n  Setting ~A to ~A, via ~A." node value reason)
  (set-tms-node-support! node reason)
  (set-tms-node-label! node value)
  (case value ;figure out which set of rules to queue up
    ((:TRUE) (begin
              (when (is-true enqueuef)
	     (for ((rule (tms-node-true-rules node)))
	       (enqueuef rule))
	     (set-tms-node-true-rules! node '()))
	   (for ((clause (tms-node-true-clauses node)))
	     (set-clause-sats! clause (+ 1 (clause-sats clause))))
	   (for ((clause (tms-node-false-clauses node)))
             (set-clause-pvs! clause (- (clause-pvs clause) 1))
	     (when (< (clause-pvs clause) 2)
		 (push clause *clauses-to-check*)))))
    ((:FALSE) (when enqueuef
	      (for ((rule (tms-node-false-rules node)))
		(enqueuef rule)))
	    (set-tms-node-false-rules! node '())
	   (for ((clause (tms-node-false-clauses node)))
	     (set-clause-sats! clause  (+ 1 (clause-sats clause))))
	    (for ((clause (tms-node-true-clauses node)))
              (set-clause-pvs! clause (- (clause-pvs clause) 1))
              (when (< (clause-pvs clause) 2)
		  (push clause *clauses-to-check*))))))

;;; Retracting an assumption ;;;;;;;;
(define-syntax-rule (insert-list2 cl ltms)
  (with-handlers ((ret (lambda (x) x)))
    (do ((cl-count (clause-length cl))
         (previous '() tail)
         (tail (ltms-queue ltms)  (cdr tail)))
        ((or (null? tail) (< cl-count (caar tail)))
         (if (not (empty? previous))
             (rplacd previous (cons (cons cl-count (cons cl '())) tail))
             (set-ltms-queue! ltms
                   (cons (cons cl-count (cons cl '())) tail))))
      (when (= cl-count (caar tail))
        (define car_ (car tail))
        (rplacd car_ (cons cl (cdar tail)))
        (set! tail (cons car_ (cdr tail)))
        (raise '())))))

(define (insert-queue cl ltms)
  (set-clause-status! cl ':QUEUED)
  (insert-list2 cl ltms))

(define (propagate-more-unknownness old-value node ltms)
  (for ((clause (case old-value
                         ((:TRUE) (tms-node-true-clauses node))
                         ((:FALSE) (tms-node-false-clauses node)))))
          (set-clause-sats! (- (clause-sats clause) 1))
          (when (and (= (clause-sats clause) 0)
                     (equal? (clause-status clause) ':DIRTY))
            (insert-queue clause ltms)))
  ;; Given in cltms.lisp
  )

(define (ipia ltms)
  void
  ;; Given in cltms.lisp
  )

(define (propagate-unknownness in-node)
  (match-define (list node old-value node2 unknown-queue ltms) (list #f #f #f '() #f))
  (set! ltms (tms-node-ltms in-node))
  (do ((forget-queue (cons in-node '()) (append forget-queue new_))
       (new_ '() '()))
      ((empty? forget-queue) unknown-queue)
    (set! forget-queue  (cdr forget-queue))
    (rplacd forget-queue unknown-queue)
    (set! unknown-queue forget-queue)
    (set! node (car unknown-queue))
    (debugging-ltms ltms "\n Retracting ~A." node)
    (set! old-value (tms-node-label node))
    (set-tms-node-label! node ':UNKNOWN)
    (set-tms-node-support! node '())
    (for ((clause (case old-value
			((:TRUE) (tms-node-false-clauses node))
			((:FALSE) (tms-node-true-clauses node)))))
      (set-clause-pvs! clause (+ 1 (clause-pvs clause)))
      (when (= (clause-pvs clause) 2)
        (set! node2 (clause-consequent clause))
        (when (is-true node2)
          (push node2 new_))))
    (when (is-true (ltms-complete ltms))
      (propagate-more-unknownness old-value node ltms))))


(define (clause-consequent clause)
  (with-handlers ((ret (lambda (x) x)))
    (for ((term-pair (clause-literals clause)))
    (when (equal? (tms-node-label (car term-pair)) (cdr term-pair))
      (raise (when (equal? clause (tms-node-support (car term-pair)))
		  (car term-pair)))))))

(define (find-alternative-support ltms nodes)
  (for ((node nodes))
    (when (unknown-node? node)
      (check-clauses ltms (tms-node-true-clauses node))
      (check-clauses ltms (tms-node-false-clauses node))))
  (when (equal? #t (ltms-complete ltms)) (ipia ltms))) ;; ipia defined in cltms

;;; Contradiction handling interface.
(define (check-for-contradictions ltms)
  (define violated-clauses #f)
  (set! violated-clauses
	(filter (lambda (c) (violated-clause? c))
		       (ltms-violated-clauses ltms)))
  (set-ltms-violated-clauses! ltms violated-clauses) ;; Cache them.
  (when (is-true violated-clauses) (contradiction-handler ltms violated-clauses))) ;; Is true for giving false for both #f, '()

 (define (contradiction-handler ltms violated-clauses)
   (with-handlers ((ret (lambda (x) x)))
   (cond ((not (ltms-checking-contradictions ltms))
          ;; Update cache of violated clauses
          (set-ltms-pending-contradictions! ltms
                (filter (lambda (c) (violated-clause? c))
                   (ltms-pending-contradictions ltms)))
          (for ((vc violated-clauses))
             (when (violated-clause? vc)
               (define lst (ltms-pending-contradictions ltms))
               (pushnew vc lst)
               (set-ltms-pending-contradictions! ltms lst))))
         (#t (for ((handler (ltms-contradiction-handlers ltms)))
              (when (handler violated-clauses ltms) (raise #t)))))))


 (define-syntax without-contradiction-check
   (syntax-rules ()
                 [(_ ltms body ...)
                  (contradiction-check ltms #f body ...)]))

 (define-syntax with-contradiction-check
   (syntax-rules ()
                 [(_ ltms body ...)
                  (contradiction-check ltms #t body ...)]))



(define-syntax contradiction-check
  (syntax-rules()
    ((_ ltms flag body ...)
     (let* ((.ltms. ltms)
            (.old-value. (ltms-checking-contradictions .ltms.)))
       (begin
         (let ((r (begin body ...)))
           (set-ltms-checking-contradictions! .ltms. flag)
           r
           ))
         (set-ltms-checking-contradictions! .ltms. .old-value.)
           ))))

(define-syntax with-contradiction-handler
  (syntax-rules()
    ((_ ltms handler body ...)
     (let ((.ltms. ltms))
       (begin
         (let ((r (begin body ...)))
           (push-ltms-contradiction-handlers! handler .ltms.)
           r))
       (pop-ltms-contradiction-handlers! .ltms.)))))

(define-syntax with-assumptions
  (syntax-rules ()
    ((_ assumption-values body ...)
     ;; Allows assumptions to be made safely, and retracted properly
     ;; even if non-local exits occur.
     (begin
       (let ((r (begin body ...)))
         (for ((av assumption-values))
           (enable-assumption (car av) (cdr av)))
          (for ((av assumption-values)) (retract-assumption (car av)))
         r))
     )))

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
       (mark (list '())) ;; Doesn't make sense even in ltms.lisp file should be (list nil). To be changes (maybe) after Kat's work
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
      (printf "\nContradiction found:")
      (print-contra-list *contra-assumptions*)
      (printf  "\nCall (TMS-ANSWER <number>) to retract assumption.")
      (let ((the-answer (read)))
        (when the-answer
          (retract-assumption (list-ref *contra-assumptions* (- the-answer 1)))))
      )
)

(define (print-contra-list nodes)
  (do ((counter 1 (+ 1 counter))
       (nn nodes (cdr nn)))
      ((empty? nn))
    (printf (format "\n~A ~A" counter
	    (node-string (car nn))))))


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
  (for ((cl (case (tms-node-label node)
		((:TRUE) (tms-node-false-clauses node))
		((:FALSE) (tms-node-true-clauses node)))))
    (unless (equal? cl (tms-node-support node))
      (set! conseq (clause-consequent cl))
      (when conseq (push conseq conseqs))))
  conseqs)

(define (why-node node)
  (cond ((unknown-node? node)
	 (printf (format  "~A is unknown." (node-string node))))
	((equal? ':ENABLED-ASSUMPTION (tms-node-support node))
	 (printf (format  "~A is ~A <~A>"
		 (node-string node)
		 (tms-node-label node) (tms-node-support node))))
	(else (printf (format "~A is ~A via ~A on"
		   (node-string node)
		   (tms-node-label node)
		   (or (clause-informant (tms-node-support node))
		       (tms-node-support node))))
	   (for ((term-pair (clause-literals (tms-node-support node))))
	     (unless (equal? (tms-node-label (car term-pair))
			    (cdr term-pair))
	       (printf (format "~A is ~A"
		       (node-string (car term-pair))
		       (tms-node-label (car term-pair))))))))
  node)

(define (why-nodes ltms)
  (hash-map  (ltms-nodes ltms) (lambda (ignore n) (why-node n))))

(define *line-count* 0)

(define (explain-node node)
  (set! *line-count* 0)
  (unless (equal? (tms-node-label node) ':UNKNOWN)
    (set! *line-count* 0)
    (hash-map  (ltms-nodes (tms-node-ltms node))  (lambda (_ node) (set-tms-node-mark! node #f)))
    (explain-1 node)))


(define (explain-1 node)
  (define antecedents '())
  (cond ((tms-node-mark node))
	((equal? ':ENABLED-ASSUMPTION (tms-node-support node))
         (if (true-node? node)
             (printf (format "\n~A     ~A     ()     Assumption"
                     (incf *line-count*) (node-string node)))
             (printf (format "\n~A     (:NOT ~A)     ()     Assumption" (incf *line-count*) (node-string node))))
	 (set-tms-node-mark! node *line-count*)
         *line-count*
         )
	(else (set! antecedents
                                (map explain-1 (clause-antecedents (tms-node-support node))))
              (cond
                ((true-node? node) (printf (format "\n~A     ~A       ~A  " (incf *line-count*) (node-string node) antecedents)))
                (else (printf (format "\n~A     (:NOT ~A)     ~A  " (incf *line-count*) (node-string node) antecedents))))
	   (pretty-print-clause (tms-node-support node))
	   (set-tms-node-mark! node *line-count*)
           *line-count*
           )))

(define (pretty-print-clauses ltms)
  (walk-clauses ltms (lambda (l)
                       (pretty-print-clause l))))

(define (pretty-print-clause clause)
  (printf (format "(:OR"))
  (for ((literal (clause-literals clause)))
    (if (equal? ':TRUE (cdr literal))
        (printf (format " ~A " (node-string (car literal))))
	(printf (format " (:NOT ~A) " (node-string (car literal))))))
  (printf (format  ")")))


(define (show-node-consequences node)
  (let ((conseqs (node-consequences node)))
    (cond ((not (empty? conseqs)) 
	   (printf (format  "\n Consequences of ~A:" (signed-node-string node)))
	   (for ((conseq conseqs))
		   (printf (format  "\n  ~A" (signed-node-string conseq)))))
	  (else (printf (format "\n ~A has no consequences." (node-string node)))))))

(define (node-show-clauses node)
  (printf (format  "For ~A:" (node-string node)))
  (for ((cl (tms-node-true-clauses node)))
    (printf (format "\n")) (pretty-print-clause cl))
  (for ((cl (tms-node-false-clauses node)))
    (printf (format "\n")) (pretty-print-clause cl)))


(define (explore-network node)
  (with-handlers ((ret (lambda (x) x)))
  (unless (known-node? node)
	  (printf (format "\n Sorry, ~A not believed." (node-string node)))
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
      (do ((good? #f)
	   (choice 0))
	  (good? (case good?
		       ((q) (raise current))
		       ((c) (set! mode ':conseq))
		       ((a) (set! mode ':ante))
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
	      (printf (format "\n Must be q, a, c or an integer from 0 to ~D."
		        olen)))))))

;;;; Add-on utilities for easier lisp to racket translation;;;;;;;
(define-syntax-rule (incf x)
  (begin
    (set! x (+ 1 x))
  x)
  )

(define-syntax-rule (decf x)
  (begin
    (set! x (- x 1))
  x)
  )


(define (is-true X)
  (and X (not (empty? X))))
  
(define-syntax-rule (rplacd lst cdr-val)
  (set! lst (cons (car lst) cdr-val)))

(define-syntax-rule (push val lst)
  (set! lst (cons val lst)))

(define-syntax-rule (pop lst)
  (begin
    (let ((popped (car lst)))
    (set! lst (cdr lst))
    popped
  )
  ))
(define-syntax-rule (pushnew val lst)
  (unless (member val lst) (set! lst (cons val lst))))

(define-syntax-rule (push-ltms-contradiction-handlers! handler ltms)
  (set-ltms-contradiction-handlers! ltms (cons handler (ltms-contradiction-handler ltms))))

(define-syntax-rule (pop-ltms-contradiction-handlers! ltms)
  (set-ltms-contradiction-handlers! ltms (cdr (ltms-contradiction-handler ltms))))

;;;;;;;;;;;;;CLTMS

(define (subsumed? lits trie)
  void
  )

(define (ret x)
  #t)

(define (add-to-trie cl ltms) ;; TODO 
  void
  )

(define (delay-sat? . op)
  void
  )

(define (index-clause . op)
  void
  )
