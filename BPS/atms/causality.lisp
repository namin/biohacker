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
