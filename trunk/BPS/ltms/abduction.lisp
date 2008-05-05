;; Ex 3, Chapter 10

(defun get-tms-node-or-nil (fact &optional (*LTRE* *LTRE*) &aux datum)
  (when (setq datum (referent fact nil)) 
    (datum-tms-node datum)))

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
	  (funcall
	   (ecase label (:TRUE #'tms-node-true-clauses) (:FALSE #'tms-node-false-clauses))
	   node))
    (setq node-list (list node))
    (mapcar #'(lambda (clause)
		(opposite-pairs (unknown-pairs clause node-list)))
	    clauses)))

(defun needs-1 (fact label &aux node)
  (when (setq node (get-tms-node-or-nil fact))
    (node-needs-1 node label)))

(defun circular-need (nodes)
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

(defun adjoin-to-all (el sets)
  (if (null sets)
      nil
    (cons (adjoin el (car sets))
	  (adjoin-to-all el (cdr sets)))))

(defun all-variations-on-set (literal-needs)
  (defun vary (set)
    (if (null set)
	(list nil)
      (let ((first-set (cdr (assoc (car set) literal-needs)))
	    (rest-sets (vary (cdr set))))
	(mapcan #'(lambda (el) (adjoin-to-all el rest-sets)) first-set))))
  #'vary)

(defun remove-supersets (sets)
  (defun helper (keep todo sets &aux sub)
    (cond ((and (null sets) (null todo))
	   keep)
	  ((null sets)
	   (if (null (car todo))
	       nil
	     (helper (cons (car todo) keep)
		     (cdr todo)
		     sets)))
	  ((null todo)
	   (helper keep
		   (cons (car sets) todo)
		   (cdr sets)))
	  ((null (car todo))
	   nil)
	  ((some #'(lambda (set)
		     (subsetp set (car sets)))
		 todo)
	   (helper keep
		   todo
		   (cdr sets)))
	  ((setq sub
		 (find-if #'(lambda (set)
			      (subsetp (car sets) set))
			  todo))
	   (helper keep
		   (cons (car sets) (remove sub todo))
		   (cdr sets)))
	  (t
	   (helper keep
		   (cons (car sets) todo)
		   (cdr sets)))))
  (helper nil nil sets))

(defun all-variations-on-sets (sets literal-needs)
  (remove-supersets 
   (mapcan (all-variations-on-set literal-needs)
	   sets)))

(defun node-needs (node label &key (nodes nil) &aux sets new-nodes literals literal-needs)
  (when (and (setq new-nodes (cons node nodes))
	     (setq sets 
		   (remove-if 
		    (circular-need nodes) 
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

(defun needs (fact label &aux)
  (when (setq node (get-tms-node-or-nil fact))
    (node-needs node label)))




