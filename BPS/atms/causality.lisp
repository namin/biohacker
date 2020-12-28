;;  Causality structure records these intermediate states of information. This allows us to have a crank and inspect it later.
;;  


(defstruct (causal (:PRINT-FUNCTION print-causal))
  (title nil)
  (graph nil)
  (priors nil)
  (symbolic-priors nil)
  (given nil)
  (intervention nil)
  (outcome nil)
  (exogenous-variables nil)
  (intervention-literals nil)
  (all-issues nil)
  (given-issues nil)
  (post-issues nil)
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

(defun numeric-probabilities-for (n atms ps)
  (remove-if-not
   #'car
   (append
    (mapcar #'(lambda (p) (cons (find-node atms (car p)) (cdr p))) ps)
    (mapcar #'(lambda (p) (cons (find-node atms (negate-name (car p))) (funcall (numeric-- n) 1 (cdr p)))) ps))))

(defun post-graph (causal)
  (let* ((ls (causal-intervention-literals causal))
         (names (apply #'concatenate 'list (mapcar #'(lambda (i) (list i (negate-name i))) ls))))
    (remove-if #'(lambda (p) (member (cdr p) names))
               (causal-graph causal))))

(defun combinations (ls)
  (if (null ls)
      '(())
      (let ((rests (combinations (cdr ls)))
            (a (car ls))
            (na (negate-name (car ls))))
        (append
         (mapcar #'(lambda (x) (cons a x)) rests)
         (mapcar #'(lambda (x) (cons na x)) rests)))))

(defun combination-probability (n atms combination ps)
  (apply (numeric-* n)
         (mapcar #'(lambda (l) (cdr (assoc (find-node atms l) ps))) combination)))

(defun combination-env (atms combination)
  (find-or-make-env (mapcar #'(lambda (l) (find-node atms l)) combination) atms))

(defun make-issue (n atms combination ps)
  (list
   combination
   (combination-env atms combination)
   (combination-probability n atms combination ps)))

(defun issue-combination (issue) (car issue))
(defun issue-env (issue) (cadr issue))
(defun issue-prob (issue) (caddr issue))
(defun issue-update-prob (issue f)
  (list (car issue) (cadr issue) (funcall f (caddr issue))))
(defun port-atms-issue (atms issue extra)
  (list
   (issue-combination issue)
   (combination-env atms (append extra (issue-combination issue)))
   (issue-prob issue)))

(defun make-issues (n atms ls ps)
  (mapcar #'(lambda (combination) (make-issue n atms combination ps)) (combinations ls)))

(defun subset-issues-with (node issues)
  (remove-if-not
   #'(lambda (issue) (in-node? node (issue-env issue)))
   issues))

(defun weight-issues (n issues)
  (apply (numeric-+ n) (mapcar #'issue-prob issues)))

(defun renormalize-issues (n issues)
  (let ((w (weight-issues n issues)))
    (mapcar
     #'(lambda (issue)
         (issue-update-prob
          issue
          #'(lambda (p) (funcall (numeric-/ n) p w))))
     issues)))

(defun weight-of-event (n atms issues v)
  (weight-issues n (subset-issues-with (find-node atms v) issues)))

(defun port-atms-issues (atms issues extra)
  (mapcar #'(lambda (issue) (port-atms-issue atms issue extra)) issues))

(defun intervention-to-literals (intervention)
  (if (and (listp intervention) (eq ':AND (car intervention)))
      (cdr intervention)
      (list intervention)))

;; 
(defun numeric-causal-crank (n causal-numeric-priors causal)
  (setf (causal-exogenous-variables causal) (mapcar #'car (funcall causal-numeric-priors causal)))
  (setf (causal-atms causal) (atms-from-graph causal (causal-graph causal) (causal-title causal)))
  (setf (causal-atms-ps causal) (numeric-probabilities-for n (causal-atms causal) (funcall causal-numeric-priors causal)))
  (setf (causal-all-issues causal) (make-issues n (causal-atms causal) (causal-exogenous-variables causal) (causal-atms-ps causal)))
  (setf (causal-intervention-literals causal) (intervention-to-literals (causal-intervention causal)))
  (setf (causal-post-graph causal) (post-graph causal))
  (setf (causal-post-atms causal) (atms-from-graph causal (causal-post-graph causal) (format nil "~A (POST)" (causal-title causal))))
  (mapc #'(lambda (i) (nogood-nodes 'nogood-not-intervention (list (find-node (causal-post-atms causal) (negate-name i))))) (causal-intervention-literals causal))
  (setf (causal-given-node causal) (find-node (causal-atms causal) "given"))
  (setf (causal-given-issues causal) (subset-issues-with (causal-given-node causal) (causal-all-issues causal)))
  (setf (causal-given-p causal) (weight-issues n (causal-given-issues causal)))
  (setf (causal-given-issues causal) (renormalize-issues n (causal-given-issues causal)))
  (setf (causal-post-issues causal) (port-atms-issues (causal-post-atms causal) (causal-given-issues causal) (causal-intervention-literals causal)))
  (setf
   (causal-post-atms-ps causal)
   (numeric-probabilities-for
    n
    (causal-post-atms causal)
    (cons
     (cons (causal-intervention causal) 1)
     (mapcar #'(lambda (v) (cons v (weight-of-event n (causal-atms causal) (causal-given-issues causal) v)))
             (causal-exogenous-variables causal)))))
  (setf (causal-outcome-node causal) (find-node (causal-post-atms causal) "outcome"))
  (setf (causal-outcome-p causal) (weight-of-event n (causal-post-atms causal) (causal-post-issues causal) "outcome"))

  (format t "~%~%Given probability: ~2$.~%" (causal-given-p causal))
  (format t "Outcome probability: ~2$.~%" (causal-outcome-p causal))
  )

(defun causal-crank (causal)
  (numeric-causal-crank *numeric* #'causal-priors causal))

(defun symbolic-causal-crank (causal)
  (numeric-causal-crank *symbolic* #'causal-symbolic-priors causal))
