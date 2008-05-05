;; Ex 3, Chapter 10

(defvar *unify-file*
  (make-bps-source-file-name *ltre-path* "unify"))

(load *unify-file*)

(defun unknown-pairs (clause &optional except-nodes)
  (remove-if
   #'(lambda (term-pair) 
       (let ((node (car term-pair)))
	(or (known-node? node)
	    (find node except-nodes))))
   (clause-literals clause)))

(defun opposite-pair (term-pair &aux node fun)
  (setq node (car term-pair))
  (setq fun (ecase (cdr term-pair) (:TRUE #'tms-node-false-literal) (:FALSE #'tms-node-true-literal)))
  (funcall fun node))

(defun opposite-pairs (term-pairs)
  (mapcar #'opposite-pair term-pairs))

(defun node-needs-1 (node label &aux node-list clauses)
  (when (unknown-node? node)
    (setq clauses 
	  (remove-if #'(lambda (clause) (satisfied-clause? clause))
	   (funcall
	    (ecase label (:TRUE #'tms-node-true-clauses) (:FALSE #'tms-node-false-clauses))
	    node)))
    (setq node-list (list node))
    (mapcar #'(lambda (clause)
		(opposite-pairs (unknown-pairs clause node-list)))
	    clauses)))

(defun needs-1 (fact label &aux node)
  (setq node (get-tms-node fact))
  (node-needs-1 node label))

(defun function-circular-need (nodes)
  #'(lambda (literals) 
    (dolist (literal literals nil)
      (when (find (car literal) nodes)
	(return t)))))

(defun label< (x y)
  (cond ((eq x y) nil)
	((eq x :TRUE) t)
	(t nil)))

(defun literal< (x y &aux cx cy)
  (defun counter (x)
    (datum-counter (tms-node-datum (car x))))
  (setq cx (counter x))
  (setq cy (counter y))
  (if (= cx cy)
      (label< (cdr x) (cdr y))
    (< cx cy)))

(defun append-to-all (el sets)
  (if (null sets)
      nil
    (cons (append el (car sets))
	  (append-to-all el (cdr sets)))))

(defun all-variations-on-set (set literal-needs)
  (if (null set)
      (list nil)
    (let ((first-sets (cdr (assoc (car set) literal-needs)))
	  (rest-sets (all-variations-on-set (cdr set) literal-needs)))
      (mapcan #'(lambda (el) (append-to-all el rest-sets)) first-sets))))

(defun function-variations-on-set (literal-needs)
  #'(lambda (set) 
      (mapcar #'remove-duplicates (all-variations-on-set set literal-needs))))

(defun remove-supersets (sets &optional keep todo)
  (cond ((and (null sets) (null todo))
	 keep)
	((null sets)
	 (if (null (car todo))
	     nil
	   (remove-supersets 
	    sets
	    (cons (car todo) keep)
	    (cdr todo))))
	((null todo)
	 (remove-supersets
	  (cdr sets)
	  keep
	  (cons (car sets) todo)))
	((null (car todo))
	 nil)
	((some #'(lambda (set) (subsetp set (car sets)))
	       todo)
	 (remove-supersets
	  (cdr sets)
	  keep
	  todo))
	((some #'(lambda (set) (subsetp (car sets) set))
	       todo)
	 (remove-supersets
	  (cdr sets)
	  keep
	  (cons (car sets) 
		(remove-if #'(lambda (set) (subsetp (car sets) set))
			   todo))))
	(t
	 (remove-supersets
	  (cdr sets)
	  keep
	  (cons (car sets) todo)))))

(defun all-variations-on-sets (sets literal-needs)
  (remove-supersets
   (mapcan (function-variations-on-set literal-needs)
	   sets)))

(defun node-needs (node label &key (nodes nil) &aux sets new-nodes literals literal-needs)
  (when (and (setq new-nodes (cons node nodes))
	     (setq sets 
		   (remove-if 
		    (function-circular-need nodes) 
		    (node-needs-1 node label))))
    (setq literals
	  (remove-duplicates (apply #'append sets)))
    (setq literal-needs
	  (mapcar #'(lambda (literal)
		      (cons literal
			    (cons (list literal) 
				  (node-needs (car literal) (cdr literal) :nodes new-nodes))))
		  literals))
    (all-variations-on-sets
     sets
     literal-needs)))

(defun literal->fact (literal &aux form)
  (setq form (datum-lisp-form (tms-node-datum (car literal))))
  (ecase (cdr literal)
    (:TRUE form)
    (:FALSE (list :NOT form))))

(defun function-matches (a)
  #'(lambda (b) (not (eq :FAIL (unify a b)))))

(defun function-matching-patterns (patterns)
  #'(lambda (set)
      (every #'(lambda (form)
		 (some (function-matches form)
		       patterns))
	     set)))

(defun needs (fact label &optional (patterns nil) &aux node sets)
  (setq node (get-tms-node fact))
  (run-rules)
  (setq sets
	(mapcar #'(lambda (set) (mapcar #'literal->fact set)) 
		(node-needs node label)))
  (when patterns
    (setq sets
	  (remove-if-not (function-matching-patterns patterns)
			 sets)))
  sets)

(defun form-cost (form pattern-cost-list)
  (cdr (assoc-if (function-matches form) pattern-cost-list)))

(defun explanation-cost (exp pattern-cost-list)
  (apply #'+
	 (mapcar #'(lambda (form) (form-cost form pattern-cost-list))
		 exp)))

(defun labduce (fact label pattern-cost-list &aux patterns sets min-cost-exp min-cost cost)
  (setq patterns (mapcar #'car pattern-cost-list))
  (setq sets (needs fact label patterns))
  (when sets
    (setq min-cost-exp (car sets))
    (setq min-cost (explanation-cost min-cost-exp pattern-cost-list))
    (dolist (set (cdr sets) (cons min-cost-exp min-cost))
      (when (< (setq cost (explanation-cost set pattern-cost-list)) 
	       min-cost)
	(setq min-cost-exp set)
	(setq min-cost cost)))))