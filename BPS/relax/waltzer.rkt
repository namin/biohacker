#lang racket

(provide (all-defined-out))

(define-namespace-anchor waltzer-anchor)

(struct network
  (
   title
   cells
   constraints
   cell-queue
   constraint-queue
   timestamp
   event-list
   debug?
   status
   contradiction-reason
   contradiction-hook
   plist
   )
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc net port mode)
     (fprintf port "<network ~a>" (network-title net)))]
  )

(define-syntax-rule (debug-waltzer net . args)
  (when (network-debug? net)
    (printf . args)))

(define (create-network title)
  (network
   title
   '()
   '()
   '()
   '()
   0
   '()
   #f
   '#:new
   '()
   #f
   (make-hash)))

(struct cell
  (
   name
   network
   value
   constraints
   possible-values
   out-reasons
   plist
   )
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (fprintf port "<cell ~a>" (cell-name c)))]
  )

(define (build-cell name net possible-values)
  (let ((c (cell name net '() '() possible-values '() (make-hash))))
    (set-network-cells! net (cons (cons name c) (network-cells net)))
    c))

(struct constraint
  (
   name
   network
   parts
   update-procedure
   queued?
   plist
   )
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc con port mode)
     (fprintf port "<constraint ~a>" (constraint-name con)))]
  )

(define (build-constraint name net update-procedure)
  (let ((con (constraint name net '() update-procedure #f (make-hash))))
    (set-network-constraints! net (cons (cons name con) (network-constraints net)))
    (queue-constraint con)
    con))

(define (add-constraint-cell c con)
  (set-cell-constraints! c (cons con (cell-constraints c))))

(define (clear-network net)
  (set-network-constraint-queue! net '())
  (set-network-cell-queue! net '())
  (set-network-event-list! net '())
  (set-network-timestamp! net 0)
  (hash-set! (network-plist net) '#:stack '())
  (for-each (lambda (nc) (clear-cell (cdr nc))) (network-cells net))
  (for-each (lambda (ncon)
              (set-constraint-queued?! (cdr ncon) #f)
              (queue-constraint (cdr ncon)))
            (network-constraints net))
  (set-network-status! net '#:new))

(define (clear-cell cell)
  (set-cell-value! cell (cell-possible-values cell))
  (hash-set! (cell-plist cell) '#:stack '())
  (set-cell-out-reasons! cell (map (lambda (pval) (cons pval (box '#:in))) (cell-possible-values cell))))

(define (copy-list xs)
  xs ;; not needed
  ;;(apply list xs)
  )

(define (copy-tree t)
  (cond
    ((pair? t)
     (cons (copy-tree (car t)) (copy-tree (cdr t))))
    ((box? t)
     (box (unbox t)))
    (else t)))

(define (push-network net)
  (debug-waltzer net " Pushing state of ~a at ~a.\n"
                 (network-title net) (network-timestamp net))
  (hash-push (list (network-timestamp net)
                   (network-status net)
                   (network-event-list net))
             (network-plist net)
             '#:stack)
  (for ([cell-entry (network-cells net)])
    (push-cell (cdr cell-entry))))

(define (hash-push v h k)
  (hash-set! h k (cons v (hash-ref h k))))

(define (hash-pop h k)
  (let ((q (hash-ref h k)))
    (hash-set! h k (cdr q))
    (car q)))

(define (push-cell cell)
  (hash-push (cons (copy-list (cell-value cell))
                   (copy-tree (cell-out-reasons cell)))
             (cell-plist cell)
             '#:stack))

(define (pop-network net)
  (when (null? (hash-ref (network-plist net) '#:stack))
    (error "No state saved in ~a." net))
  (set-network-constraint-queue! net '())
  (set-network-cell-queue! net '())
  (let ((state (hash-pop (network-plist net) '#:stack)))
    (set-network-timestamp! net (car state))
    (set-network-status! net (cadr state))
    (set-network-event-list! net (caddr state)))
  (for ([cell-entry (network-cells net)]) (pop-cell (cdr cell-entry)))
  (for ([con-entry (network-constraints net)])
    (set-constraint-queued?! (cdr con-entry) #f))
  (debug-waltzer net "\n ..Popped ~a back to ~a."
                 (network-title net) (network-timestamp net)))

(define (pop-cell cell)
  (let ((state (hash-pop (cell-plist cell) '#:stack)))
    (set-cell-value! cell (car state))
    (set-cell-out-reasons! cell (cdr state))))

(define (exclude cell value [informant '#:user])
  (let ((net (cell-network cell)))
    (debug-waltzer net "\n   Excluding ~a from ~a via ~a"
                   value (cell-name cell) informant)
    (let ((entry (assoc value (cell-out-reasons cell))))
      (cond ((eq? (unbox (cdr entry)) '#:in)
             (set-box! (cdr entry) (cons informant (network-timestamp net)))
             (set-cell-value! cell (remove value (cell-value cell)))
             (when (null? (cell-value cell))
               (signal-contradiction cell 'Overconstrained))
             (for ([con (cell-constraints cell)])
               (queue-constraint con)))
            (else
             (debug-waltzer net " -- already excluded."))))))

(define (pick cell value [informant '#:user])
  (let ((net (cell-network cell)))
    (debug-waltzer net "\n   Selecting ~a for ~a, via ~a."
                   value (cell-name cell) informant)
    (let ((entry (assoc value (cell-out-reasons cell))))
      (cond ((eq? (unbox (cdr entry)) '#:in)
             (for ([other (cell-out-reasons cell)])
               (unless (equal? value (car other))
                 (exclude cell (car other) informant))))
            (else (signal-contradiction cell 'dead-choice))))))

(define (update constraint)
  ((constraint-update-procedure constraint) constraint))

(define (fire-constraints net)
  (if (eq? (network-status net) '#:overconstrained)
      '()
      (begin
        (set-network-status! net '#:in-progress)
        (debug-waltzer net "\n Beginning propagation on ~a.." (network-title net))
        (let loop ((nconstraints 0)
                   (ncells 0)
                   (con '()))
          (cond
            ((or (and (null? (network-cell-queue net))
                      (null? (network-constraint-queue net)))
                 (eq? (network-status net) '#:overconstrained))
             (debug-waltzer net "\n .. finished propagating ~a." (network-title net))
             (unless (eq? (network-status net) '#:overconstrained)
               (set-network-status! net '#:quiescent))
             (values ncells nconstraints))
            ((not (null? (network-cell-queue net)))
             (let ((q (network-cell-queue net)))
               (set-network-cell-queue! net (cdr q))
               (eval (car q) (namespace-anchor->namespace waltzer-anchor)))
             (loop nconstraints (+ 1 ncells) con))
            (else
             (let* ((q (network-constraint-queue net))
                    (con (car q)))
               (set-network-constraint-queue! net (cdr q))
               (set-network-timestamp! net (+ 1 (network-timestamp net)))
               (set-network-event-list! net (cons con (network-event-list net)))
               (set-constraint-queued?! con #f)
               (update con)
               (loop (+ 1 nconstraints) ncells con))))))))

(define (queue-cell cell message value informant)
  (let ((net (cell-network cell)))
    (set-network-cell-queue!
     net
     (cons
      `(,(case message
           ('#:exclude 'exclude)
           ('#:pick 'pick))
        ,cell
        ',value
        ',informant)
      (network-cell-queue net)))))

(define (queue-constraint constraint)
  (unless (constraint-queued? constraint)
    (set-constraint-queued?! constraint #t)
    (let ((net (constraint-network constraint)))
      (set-network-constraint-queue!
       net
       (cons constraint (network-constraint-queue net))))))

(define (check-constraints net)
  (for ([con-entry (network-constraints net)])
    (queue-constraint (cdr con-entry))
    (fire-constraints net)))

(define (signal-contradiction cell message)
  (let ((net (cell-network cell)))
    (debug-waltzer net "\nContradiction concerning cell ~a: ~a" cell message)
    (set-network-status! net '#:overconstrained)
    (set-network-contradiction-reason! net cell)
    (when (network-contradiction-hook net)
      ((network-contradiction-hook net) net))))

(define (lookup-cell name net)
  (cdr (assoc name (network-cells net))))

(define (lookup-constraint name net)
  (cdr (assoc name (network-constraints net))))

(define (what-are net)
  (printf "\n ~a is ~a" net (network-status net))
  (for ([cell-entry (network-cells net)])
    (what-is (cdr cell-entry))))

(define (determined? net)
  (and (eq? (network-status net) '#:quiescent)
       (andmap (lambda (cell-entry) (known? (cdr cell-entry)))
               (network-cells net))))

(define (to-plunk net)
  (filter-map (lambda (cell-entry)
                (and (not (null? (cdr (cell-value (cdr cell-entry)))))
                     (cdr cell-entry)))
              (network-cells net)))

(define (what-is cell)
  (cond ((null? (cell-value cell))
         (printf "\n  ~a is overconstrained." (cell-name cell)))
        ((not (null? (cdr (cell-value cell))))
         (printf "\n  ~a is one of ~a." (cell-name cell) (cell-value cell)))
        (else
         (printf "\n  ~a = ~a." (cell-name cell) (car (cell-value cell))))))

(define (known? cell)
  (and (not (null? (cell-value cell))) (null? (cdr (cell-value cell)))))

(define (value cell)
  (unless (known? cell)
    (error "\n~a not known." (cell-name cell)))
  (car (cell-value cell)))

(define (search-network net consistent-proc contra-proc [prune #f])
  (define (search-thru-plunkable-cells cells indent)
    (let ((cells (if prune (prune (copy-list cells)) cells)))
      (cond ((eq? (network-status net) '#:overconstrained)
             (contra-proc net))
            ((determined? net)
             (consistent-proc net))
            ((null? cells)
             (error "Inconsistent network status -- ~a" net))
            (else
             (for ([val (copy-list (cell-value (car cells)))])
               (printf "\n~a Trying ~a for ~a" indent val (cell-name (car cells)))
               (push-network net)
               (pick (car cells) val '#:search)
               (fire-constraints net)
               (search-thru-plunkable-cells (cdr cells) (string-append indent " "))
               (pop-network net))))))
  (if (determined? net)
      (begin (consistent-proc net) net)
      (search-thru-plunkable-cells (to-plunk net) "")))

(define (say-solution net)
  (unless (determined? net)
    (error "say-solution called with unsolved network ~a." (network-title network)))
  (printf "\n A solution for ~a:" (network-title net))
  (what-are net)
  (raise "Consistent solution"))

(define (say-contradiction net)
  (unless (eq? (network-status net) '#:overconstrained)
    (error "say-contradiction called on okay network ~a." (network-title net)))
  (printf "\n ~a overconstrained, due to ~a."
          (network-title net)
          (cell-name (network-contradiction-reason net))))

(define (show-search net [prune #f])
  (search-network net say-solution say-contradiction prune))
