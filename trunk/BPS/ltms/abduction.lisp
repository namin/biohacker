;; Ex 3, Chapter 10

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

(defun node-literal (node label)
  (funcall
   (ecase label (:TRUE #'tms-node-true-literal) (:FALSE #'tms-node-false-literal))
   node))

(defun node-needs-1 (node label &aux node-list clauses)
  (when (unknown-node? node)
    (setq clauses 
	  (remove-if #'satisfied-clause?
	   (funcall
	    (ecase label (:TRUE #'tms-node-true-clauses) (:FALSE #'tms-node-false-clauses))
	    node)))
    (setq node-list (list node))
    (mapcar #'(lambda (clause)
		(opposite-pairs (unknown-pairs clause node-list)))
	    clauses)))


(defun literal->fact (literal &aux form)
  (setq form (datum-lisp-form (tms-node-datum (car literal))))
  (ecase (cdr literal)
    (:TRUE form)
    (:FALSE (list :NOT form))))

(defun literal-set->fact-set (set)
  (mapcar #'literal->fact set))

(defun literal-sets->fact-sets (sets)
  (mapcar #'literal-set->fact-set sets))

(defun needs-1 (fact label &aux node)
  (setq node (get-tms-node fact))
  (literal-sets->fact-sets (node-needs-1 node label)))

(defun append-to-all (set ssets)
  (do ((ssets ssets (cdr ssets))
       (result nil (cons (append set (car ssets)) result)))
      ((null ssets) result)))

(defun all-combinations (sssets)
  (do ((sssets sssets (cdr sssets))
       (result (list nil)
	       (mapcan 
		#'(lambda (set)
		    (append-to-all set result))
		(car sssets))))
      ((null sssets) result)))

(defun all-variations-on-set (set literal-needs)
  (all-combinations
   (mapcar 
    #'(lambda (literal)
	(cdr (assoc literal literal-needs)))
    set)))

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
	   (remove-supersets sets))))

(defun add-literal-as-set-if (matching-patterns literal sets)
  (if (or (null matching-patterns)
	  (funcall matching-patterns (literal->fact literal)))
      (cons (list literal) sets)
    sets))

(defun add-all-literals-needs (literals matching-patterns literal-needs-list 
					&optional (k #'(lambda (x) x)))
  (if (null literals)
      (funcall k literal-needs-list)
    (add-all-literals-needs
     (cdr literals)
     matching-patterns
     literal-needs-list
     #'(lambda (literal-needs-list)
	 (if (assoc (car literals) literal-needs-list)
	     (funcall k literal-needs-list)
	   (add-literal-needs
	    (car literals)
	    matching-patterns
	    literal-needs-list
	    k))))))

(defun add-literal-needs (literal matching-patterns literal-needs-list 
				  &optional (k #'(lambda (x) x))
				  &aux sets-1 sub-literals sets)
  (setq literal-needs-list
	(acons literal :PENDING literal-needs-list))
  (setq sets-1 (node-needs-1 (car literal) (cdr literal)))
  (setq sub-literals (remove-duplicates (apply #'append sets-1)))
  (add-all-literals-needs
   sub-literals
   matching-patterns
   literal-needs-list
   #'(lambda (literal-needs-list)
       (setq sets-1
	     (remove-if
	      #'(lambda (set)
		  (some 
		   #'(lambda (sub-literal)
		       (eq :PENDING 
			   (cdr (assoc sub-literal literal-needs-list))))
		   set))
	      sets-1))
       (setq sets
	     (all-variations-on-sets
	      sets-1
	      literal-needs-list))
       (setq sets
	     (add-literal-as-set-if
	      matching-patterns
	      literal
	      sets))
       (funcall k (acons literal sets literal-needs-list)))))

(defun node-needs (node label &optional (matching-patterns nil) &aux literal)
  (setq literal (node-literal node label))
  (cdr (assoc literal (add-literal-needs literal matching-patterns nil))))

(defun function-matches (a)
  #'(lambda (b) (not (eq :FAIL (unify a b)))))

(defun function-matching-patterns (patterns)
  #'(lambda (form)
      (some (function-matches form)
	    patterns)))

(defun needs (fact label &optional (patterns nil) &aux matching-patterns node sets)
  (setq node (get-tms-node fact))
  (run-rules)
  (setq matching-patterns
	(when patterns
	  (function-matching-patterns patterns)))
  (setq sets 
	(literal-sets->fact-sets
	 (node-needs node label matching-patterns)))
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

(defun alphalessp (x y)
  (string-lessp
   (format nil "~A" x)
   (format nil "~A" y)))

(defun sort-fact-sets (sets)
  (sort
   (mapcar #'(lambda (set)
	      (sort set #'alphalessp))
	   sets)
   #'alphalessp))

(defun pp-sets (sets &optional (st t))
  (dolist (set sets)
    (format st "~%(")
    (dolist (fact set)
      (format st " ~A " fact))
    (format st ")")))