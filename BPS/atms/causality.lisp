(defstruct (causal (:PRINT-FUNCTION print-causal))
  (title nil)
  (graph nil)
  (priors nil)
  (given nil)
  (intervention nil)
  (find nil))

(defun print-causal (causal stream ignore)
  (declare (ignore ignore))
  (format stream "#<causal: ~A>" (causal-title causal)))

(setq
 *causal*
 (make-causal
  :title "riflemen"
  :graph
  '((U . C)
    (C . A)
    (C . B)
    (A . D)
    (B . D)
    (W . A))
  :priors
  '((U . 0.7)
    (W . 0.6))
  :given 'D
  :intervention '(:NOT A)
  :find '(:NOT D)))

(setq
 *graph*
 (causal-graph *causal*))

(defun graph-reverse (graph)
  (mapcar #'(lambda (p) (cons (cdr p) (car p))) graph))

(defun consolidate-alist (alist &optional (result nil))
  (if (null alist)
      result
      (let* ((key (caar alist))
             (val (cdar alist))
             (p (assoc key result))
             (result (if p
                         (progn (rplacd p (cons val (cdr p))) result)
                         (cons (list key val) result))))
        (consolidate-alist (cdr alist) result))))

;; (graph-reverse *graph*)
;; (consolidate-alist (graph-reverse *graph*))

(defun graph-formula (graph)
  `(:and
    .
    ,(append
      (mapcar #'(lambda (p) `(:implies ,(car p) ,(cdr p))) graph)
      (mapcar #'(lambda (p) `(:implies ,(car p) (:or . ,(cdr p)))) (consolidate-alist (graph-reverse graph))))))

;; (graph-formula *graph*)

(setq *formula* (graph-formula *graph*))

(setq *p* (PLTMS::prime-implicates *formula*))

(defun print-clauses (cs)
  (loop for c in cs do
    (PLTMS::pretty-print-clause c)
    (format t "~%")))

(setq *clauses* (PLTMS::collect *p*))

(print-clauses *clauses*)

(setq *atms* (create-atms (causal-title *causal*) :debugging t))

;; (car (PLTMS::clause-literals (car *clauses*)))

(defun find-node (atms name)
  (find-if #'(lambda (n) (equal name (tms-node-datum n))) (atms-nodes atms)))

(defun name-literal (l)
  (let ((d (PLTMS::tms-node-datum (car l))))
    (ecase (cdr l)
      (:TRUE d)
      (:FALSE `(:not ,d)))))

(defun create-node (atms la)
  (let ((pos (tms-create-node atms (name-literal (cons la :TRUE))))
        (neg (tms-create-node atms (name-literal (cons la :FALSE)))))
    (nogood-nodes 'nogood-complement (list pos neg))
    (assume-node pos)
    (assume-node neg)
    'done))

(defun translate-node (atms literal)
  (or (find-node atms (name-literal literal))
      (progn
        (create-node atms (car literal))
        (translate-node atms literal))))

;; (translate-node *atms* (car (PLTMS::clause-literals (car *clauses*))))
;; (translate-node *atms* (cadr (PLTMS::clause-literals (car *clauses*))))

(defun negate-literal (l)
  (cons
   (car l)
   (ecase (cdr l)
     (:TRUE :FALSE)
     (:FALSE :TRUE))))

;; (negate-literal (car (PLTMS::clause-literals (car *clauses*))))

(defun all-splits (xs &optional (prev nil))
  (if (null xs)
      nil
      (cons
       (cons (car xs) (append (reverse prev) (cdr xs)))
       (all-splits (cdr xs) (cons (car xs) prev)))))

;; (all-splits '(1 2 3 4))

(defun translate-clause (atms clause)
  (mapcar
   #'(lambda (ls)
       (justify-node
        'PI
        (translate-node atms (car ls))
        (mapcar #'(lambda (l) (translate-node atms (negate-literal l))) (cdr ls))))
   (all-splits (PLTMS::clause-literals clause))))

;; (translate-clause *atms* (car *clauses*))

(mapcar #'(lambda (c) (translate-clause *atms* c)) *clauses*)

(why-nodes *atms*)
#|
<The contradiction,{}>
<A,{{U}{W}{D}{B}{C}{A}}>
<(NOT A),{{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT D)}{(NOT A)}}>
<C,{{U}{(NOT W),D}{(NOT W),A}{B}{C}}>
<(NOT C),{{(NOT U)}{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}}>
<B,{{U}{(NOT W),D}{(NOT W),A}{C}{B}}>
<(NOT B),{{(NOT U)}{(NOT D)}{(NOT A)}{(NOT C)}{(NOT B)}}>
<D,{{U}{W}{A}{B}{C}{D}}>
<(NOT D),{{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<W,{{(NOT U),A}{(NOT U),D}{(NOT B),A}{(NOT C),A}{(NOT B),D}{(NOT C),D}{W}}>
<(NOT W),{{(NOT D)}{(NOT A)}{(NOT W)}}>
<U,{{(NOT W),D}{(NOT W),A}{B}{C}{U}}>
<(NOT U),{{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}{(NOT U)}}>
|#

(defun negate-name (name)
  (if (and (listp name) (eq :not (car name)))
      (cadr name)
      `(:not ,name)))

;; (negate-name 'D)
;; (negate-name '(:not D))

(setq
 *ps*
 (append
  (mapcar #'(lambda (p) (cons (find-node *atms* (car p)) (cdr p))) (causal-priors *causal*))
  (mapcar #'(lambda (p) (cons (find-node *atms* (negate-name (car p))) (- 1 (cdr p)))) (causal-priors *causal*))
  ))

(why-prob-nodes *atms* *ps*)
#|
<The contradiction,0.00:{}>
<A,0.88:{0.70:{U}0.60:{W}{D}{B}{C}{A}}>
<(NOT A),0.12:{0.12:{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT D)}{(NOT A)}}>
<C,0.70:{0.70:{U}{(NOT W),D}{(NOT W),A}{B}{C}}>
<(NOT C),0.30:{0.30:{(NOT U)}{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}}>
<B,0.70:{0.70:{U}{(NOT W),D}{(NOT W),A}{C}{B}}>
<(NOT B),0.30:{0.30:{(NOT U)}{(NOT D)}{(NOT A)}{(NOT C)}{(NOT B)}}>
<D,0.88:{0.70:{U}0.60:{W}{A}{B}{C}{D}}>
<(NOT D),0.12:{0.12:{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<W,0.60:{{(NOT U),A}{(NOT U),D}{(NOT B),A}{(NOT C),A}{(NOT B),D}{(NOT C),D}0.60:{W}}>
<(NOT W),0.40:{{(NOT D)}{(NOT A)}0.40:{(NOT W)}}>
<U,0.70:{{(NOT W),D}{(NOT W),A}{B}{C}0.70:{U}}>
<(NOT U),0.30:{{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}0.30:{(NOT U)}}>
|#

(setq *given-node* (find-node *atms* (causal-given *causal*)))
(setq *given-p* (node-prob *given-node* *ps*))
