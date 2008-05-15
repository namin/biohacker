;; biohacker/trunk/BPS/ltms/explain.lisp

(defun node->fact (node &aux literal)
  (setq literal
	(cond ((true-node? node) (tms-node-true-literal node))
	      ((false-node? node) (tms-node-false-literal node))
	      (t (error (format nil "Node ~A is not known." node)))))
  (literal->fact literal))

(defun node-all-antecedents (node)
  (clear-node-marks)
  (do ((todo (list node))
       (result nil)
       (current)
       (support))
      ((null todo) result)
    (setq current (car todo))
    (setq todo (cdr todo))
    (unless (tms-node-mark current)
      (setf (tms-node-mark current) t)
      (setq support (tms-node-support current))
      (unless (eq :ENABLED-ASSUMPTION support)
	(setq todo (append (clause-antecedents support) todo)))
      (push current result))))

(defun all-antecedents (fact &optional (patterns nil) &aux node antecedents)
  (setq node (get-tms-node fact))
  (when (known-node? node)
    (setq antecedents (mapcar #'node->fact (node-all-antecedents node)))
    (when patterns
      (setq antecedents (remove-if-not (function-matching-patterns patterns) antecedents)))
    antecedents))

