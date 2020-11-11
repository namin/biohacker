(defstruct (causal (:PRINT-FUNCTION print-causal))
  (title nil)
  (graph nil)
  (priors nil)
  (symbolic-priors nil)
  (given nil)
  (intervention nil)
  (outcome nil)
  (atms nil)
  (atms-ps nil)
  (post-graph nil)
  (post-atms nil)
  (post-atms-ps nil)
  (given-node nil)
  (given-p nil)
  (outcome-node nil)
  (outcome-p nil))

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
  '((U . 0.6)
    (W . 0.7))
  :symbolic-priors
  '((U . p)
    (W . q))
  :given 'D
  :intervention '(:NOT A)
  :outcome '(:NOT D)))

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

(defun graph-formula (graph)
  `(:and
    .
    ,(append
      (mapcar #'(lambda (p) `(:implies ,(car p) ,(cdr p))) graph)
      (mapcar #'(lambda (p) `(:implies ,(car p) (:or . ,(cdr p)))) (consolidate-alist (graph-reverse graph))))))

(defun print-clauses (cs)
  (loop for c in cs do
    (PLTMS::pretty-print-clause c)
    (format t "~%")))

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
    (unless (member (PLTMS::tms-node-datum la) '("given" "outcome") :test #'equal)
      (assume-node pos)
      (assume-node neg))
    'done))

(defun translate-node (atms literal)
  (or (find-node atms (name-literal literal))
      (progn
        (create-node atms (car literal))
        (translate-node atms literal))))

(defun negate-literal (l)
  (cons
   (car l)
   (ecase (cdr l)
     (:TRUE :FALSE)
     (:FALSE :TRUE))))

(defun all-splits (xs &optional (prev nil))
  (if (null xs)
      nil
      (cons
       (cons (car xs) (append (reverse prev) (cdr xs)))
       (all-splits (cdr xs) (cons (car xs) prev)))))

(defun translate-clause (atms clause)
  (mapcar
   #'(lambda (ls)
       (justify-node
        'PI
        (translate-node atms (car ls))
        (mapcar #'(lambda (l) (translate-node atms (negate-literal l))) (cdr ls))))
   (all-splits (PLTMS::clause-literals clause))))

(defun atms-from-graph (causal graph title &aux formula p clauses atms)
  (setq formula (graph-formula graph))
  (setq
   formula
   `(:and
     ,formula
     (:implies ,(causal-given causal) "given")
     (:implies ,(causal-outcome causal) "outcome")))
  (setq p (PLTMS::prime-implicates formula))
  (setq clauses (PLTMS::collect p))
  (setq atms (create-atms title :debugging nil))
  (mapcar #'(lambda (c) (translate-clause atms c)) clauses)
  atms)

(defun negate-name (name)
  (if (and (listp name) (eq :not (car name)))
      (cadr name)
      `(:not ,name)))

(defun probabilities-for (atms ps)
  (remove-if-not
   #'car
   (append
    (mapcar #'(lambda (p) (cons (find-node atms (car p)) (cdr p))) ps)
    (mapcar #'(lambda (p) (cons (find-node atms (negate-name (car p))) (- 1 (cdr p)))) ps))))

(defun symbolic-probabilities-for (atms ps)
  (remove-if-not
   #'car
   (append
    (mapcar #'(lambda (p) (cons (find-node atms (car p)) (cdr p))) ps)
    (mapcar #'(lambda (p) (cons (find-node atms (negate-name (car p))) (symbolic-- 1 (cdr p)))) ps))))

(defun post-graph (causal)
  (let* ((i (causal-intervention causal))
         (names (list i (negate-name i))))
    (remove-if #'(lambda (p) (member (cdr p) names))
               (causal-graph causal))))

(defun causal-crank (causal)
  (setf (causal-atms causal) (atms-from-graph causal (causal-graph causal) (causal-title causal)))
  (setf (causal-atms-ps causal) (probabilities-for (causal-atms causal) (causal-priors causal)))
  (setf (causal-post-graph causal) (post-graph causal))
  (setf (causal-post-atms causal) (atms-from-graph causal (causal-post-graph causal) (format nil "~A (POST)" (causal-title causal))))
  (nogood-nodes 'nogood-not-intervention (list (find-node (causal-post-atms causal) (negate-name (causal-intervention causal)))))
  (setf (causal-given-node causal) (find-node (causal-atms causal) "given"))
  (setf (causal-given-p causal) (node-prob (causal-given-node causal) (causal-atms-ps causal)))
  (setf
   (causal-post-atms-ps causal)
   (probabilities-for
    (causal-post-atms causal)
    (cons
     (cons (causal-intervention causal) 1.0)
     (mapcar #'(lambda (p) (cons (car p) (/ (cdr p) (causal-given-p causal))))
             (causal-priors causal)))))
  (setf (causal-outcome-node causal) (find-node (causal-post-atms causal) "outcome"))
  (setf (causal-outcome-p causal) (node-prob (causal-outcome-node causal) (causal-post-atms-ps causal)))

  (format t "~%~%Before intervention:~%")
  (why-prob-nodes (causal-atms causal) (causal-atms-ps causal))
  (format t "~%~%After intervention:~%")
  (why-prob-nodes (causal-post-atms causal) (causal-post-atms-ps causal)))

(causal-crank *causal*)
#|
Before intervention:

<The contradiction,0.00:{}>
<A,0.88:{0.60:{U}0.70:{W}{D}{B}{C}{A}}>
<(NOT A),0.12:{0.12:{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT D)}{(NOT A)}}>
<C,0.60:{0.60:{U}{(NOT W),D}{(NOT W),A}{B}{C}}>
<(NOT C),0.40:{0.40:{(NOT U)}{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}}>
<B,0.60:{0.60:{U}{(NOT W),D}{(NOT W),A}{C}{B}}>
<(NOT B),0.40:{0.40:{(NOT U)}{(NOT D)}{(NOT A)}{(NOT C)}{(NOT B)}}>
<D,0.88:{0.60:{U}0.70:{W}{A}{B}{C}{D}}>
<(NOT D),0.12:{0.12:{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<W,0.70:{{(NOT U),A}{(NOT U),D}{(NOT B),A}{(NOT C),A}{(NOT B),D}{(NOT C),D}0.70:{W}}>
<(NOT W),0.30:{{(NOT D)}{(NOT A)}0.30:{(NOT W)}}>
<U,0.60:{{(NOT W),D}{(NOT W),A}{B}{C}0.60:{U}}>
<(NOT U),0.40:{{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}0.40:{(NOT U)}}>

After intervention:

<The contradiction,0.00:{}>
<B,0.68:{0.68:{U}{(NOT A),D}{C}{B}}>
<(NOT B),0.32:{0.32:{(NOT U)}{(NOT D)}{(NOT C)}{(NOT B)}}>
<C,0.68:{0.68:{U}{(NOT A),D}{B}{C}}>
<(NOT C),0.32:{0.32:{(NOT U)}{(NOT D)}{(NOT B)}{(NOT C)}}>
<D,0.68:{0.68:{U}{B}{C}{D}}>
<(NOT D),0.32:{0.32:{(NOT A),(NOT U)}{(NOT A),(NOT B)}{(NOT A),(NOT C)}{(NOT D)}}>
<A,0.00:{}>
<(NOT A),1.00:{{(NOT D)}1.00:{(NOT A)}}>
<U,0.68:{{(NOT A),D}{B}{C}0.68:{U}}>
<(NOT U),0.32:{{(NOT D)}{(NOT B)}{(NOT C)}0.32:{(NOT U)}}>
|#

(defun symbolic-causal-crank (causal)
  (setf (causal-atms causal) (atms-from-graph causal (causal-graph causal) (causal-title causal)))
  (setf (causal-atms-ps causal) (symbolic-probabilities-for (causal-atms causal) (causal-symbolic-priors causal)))
  (setf (causal-post-graph causal) (post-graph causal))
  (setf (causal-post-atms causal) (atms-from-graph causal (causal-post-graph causal) (format nil "~A (POST)" (causal-title causal))))
  (nogood-nodes 'nogood-not-intervention (list (find-node (causal-post-atms causal) (negate-name (causal-intervention causal)))))
  (setf (causal-given-node causal) (find-node (causal-atms causal) "given"))
  (setf (causal-given-p causal) (symbolic-node-prob (causal-given-node causal) (causal-atms-ps causal)))
  (setf
   (causal-post-atms-ps causal)
   (symbolic-probabilities-for
    (causal-post-atms causal)
    (cons
     (cons (causal-intervention causal) 1)
     (mapcar #'(lambda (p) (cons (car p) (list '/ (cdr p) (causal-given-p causal))))
             (causal-symbolic-priors causal)))))
  (setf (causal-outcome-node causal) (find-node (causal-post-atms causal) "outcome"))
  (setf (causal-outcome-p causal) (symbolic-node-prob (causal-outcome-node causal) (causal-post-atms-ps causal)))

  (format t "~%~%Before intervention:~%")
  (symbolic-why-prob-nodes (causal-atms causal) (causal-atms-ps causal))
  (format t "~%~%After intervention:~%")
  (symbolic-why-prob-nodes (causal-post-atms causal) (causal-post-atms-ps causal)))

(symbolic-causal-crank *causal*)
#|
Before intervention:

<The contradiction,0.00:{}>
<A,(- (+ P Q) (* P Q)):{P:{U}Q:{W}{D}{B}{C}{A}}>
<(NOT A),(* (- 1 Q) (- 1 P)):{(* (- 1 Q) (- 1 P)):{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT D)}{(NOT A)}}>
<C,P:{P:{U}{(NOT W),D}{(NOT W),A}{B}{C}}>
<(NOT C),(- 1 P):{(- 1 P):{(NOT U)}{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}}>
<B,P:{P:{U}{(NOT W),D}{(NOT W),A}{C}{B}}>
<(NOT B),(- 1 P):{(- 1 P):{(NOT U)}{(NOT D)}{(NOT A)}{(NOT C)}{(NOT B)}}>
<D,(- (+ P Q) (* P Q)):{P:{U}Q:{W}{A}{B}{C}{D}}>
<(NOT D),(* (- 1 Q) (- 1 P)):{(* (- 1 Q) (- 1 P)):{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<W,Q:{{(NOT U),A}{(NOT U),D}{(NOT B),A}{(NOT C),A}{(NOT B),D}{(NOT C),D}Q:{W}}>
<(NOT W),(- 1 Q):{{(NOT D)}{(NOT A)}(- 1 Q):{(NOT W)}}>
<U,P:{{(NOT W),D}{(NOT W),A}{B}{C}P:{U}}>
<(NOT U),(- 1 P):{{(NOT D)}{(NOT B)}{(NOT A)}{(NOT C)}(- 1 P):{(NOT U)}}>

After intervention:

<The contradiction,0.00:{}>
<B,(/ P (- (+ P Q) (* P Q))):{(/ P (- (+ P Q) (* P Q))):{U}{(NOT A),D}{C}{B}}>
<(NOT B),(- 1 (/ P (- (+ P Q) (* P Q)))):{(- 1 (/ P (- (+ P Q) (* P Q)))):{(NOT U)}{(NOT D)}{(NOT C)}{(NOT B)}}>
<C,(/ P (- (+ P Q) (* P Q))):{(/ P (- (+ P Q) (* P Q))):{U}{(NOT A),D}{B}{C}}>
<(NOT C),(- 1 (/ P (- (+ P Q) (* P Q)))):{(- 1 (/ P (- (+ P Q) (* P Q)))):{(NOT U)}{(NOT D)}{(NOT B)}{(NOT C)}}>
<D,(/ P (- (+ P Q) (* P Q))):{(/ P (- (+ P Q) (* P Q))):{U}{B}{C}{D}}>
<(NOT D),(- 1 (/ P (- (+ P Q) (* P Q)))):{(- 1 (/ P (- (+ P Q) (* P Q)))):{(NOT A),(NOT U)}{(NOT A),(NOT B)}{(NOT A),(NOT C)}{(NOT D)}}>
<A,0.00:{}>
<(NOT A),0.00:{{(NOT D)}{(NOT A)}}>
<U,(/ P (- (+ P Q) (* P Q))):{{(NOT A),D}{B}{C}(/ P (- (+ P Q) (* P Q))):{U}}>
<(NOT U),(- 1 (/ P (- (+ P Q) (* P Q)))):{{(NOT D)}{(NOT B)}{(NOT C)}(- 1 (/ P (- (+ P Q) (* P Q)))):{(NOT U)}}>
|#
