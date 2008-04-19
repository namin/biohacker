;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;; Complete logic-based truth maintenance system, version 16 of 4/26/92
;;; Requires LTMS version 39.
;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.
 
;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; Adding formulas and clauses.

(defmacro map-over (literals mapf informant)
  `(add-clause-internal
     (sort-clause (mapcar #'(lambda (literal)
			      (if (eq (cdr literal) :TRUE)
				  (tms-node-true (,mapf (car literal)))
				  (tms-node-false (,mapf (car literal)))))
			  ,literals))
     ,informant
     t))

(defun add-formula (ltms formula &optional informant 
		    &aux clauses tltms literals)
  (setq tltms (create-ltms "Temporary for add-formula"
			   :COMPLETE :DELAY :DELAY-SAT nil)
	clauses (normalize ltms formula))
  (unless informant (setq informant (list :IMPLIED-BY formula)))
  (maphash #'(lambda (ignore node)
	       (setf (tms-node-mark node) 0))
	   (ltms-nodes ltms))
  (dolist (clause clauses)
    (dolist (literal clause)
      (if (= (tms-node-mark (car literal)) 0)
	  (push (car literal) literals))
      (incf (tms-node-mark (car literal)))))
  (dolist (literal (sort literals #'< :KEY #'tms-node-mark))
    (setf (tms-node-mark  literal) (tms-create-node tltms literal)))
  (dolist (clause clauses)
    (map-over clause tms-node-mark nil))
  (complete-ltms tltms)
  (walk-trie #'(lambda (clause)
		 (map-over (clause-literals clause) tms-node-datum informant))
	     (ltms-clauses tltms))
  (check-for-contradictions ltms)
  (if (eq (ltms-complete ltms) :COMPLETE) (ipia ltms)))

(defun normalize-disjunction (exp negate &aux result disj ltms nltms)
  (unless (cdr exp)
    (return-from normalize-disjunction (list nil)))
  (setq result (normalize-1 (cadr exp) negate)
	ltms (create-ltms "Normalize disjunction"))
  (dolist (c result) (add-to-trie (make-clause :LITERALS c) ltms))
  (dolist (sub-exp (cddr exp))
    (setq nltms (create-ltms "Normalize disjunction"))
    (dolist (disj1 (normalize-1 sub-exp negate))
      (walk-trie #'(lambda (disj2)
		     (setq disj (disjoin-clauses disj1 (clause-literals disj2)))
		     (when (and (not (eq :FAIL disj))
				(not (subsumed? disj (ltms-clauses nltms))))
		       (remove-subsumed #'null disj nltms)
		       (add-to-trie (make-clause :LITERALS disj) nltms)))
		 (ltms-clauses ltms)))
    (setq ltms nltms))
  (mapcar #'clause-literals (collect ltms)))

(defun normalize-conjunction (exp negate &aux ltms)
  (setq ltms (create-ltms "Normalize conjunction"))
  (dolist (sub-exp (cdr exp))
    (dolist (disjunct (normalize-1 sub-exp negate))
      (when (not (subsumed? disjunct (ltms-clauses ltms)))
	(remove-subsumed #'null disjunct ltms)
	(add-to-trie (make-clause :LITERALS disjunct) ltms))))
  (mapcar #'clause-literals (collect ltms)))

(defun disjoin-clauses (terms1 terms2 &aux result compare)
  (do nil (nil)
    (cond ((null terms1) (return (nreconc result terms2)))
	  ((null terms2) (return (nreconc result terms1)))
	  ((eq (car terms1) (car terms2))
	   (push (pop terms1) result)
	   (pop terms2))
	  ((= 0 (setq compare (- (tms-node-index (caar terms1))
				 (tms-node-index (caar terms2)))))
	   (return :FAIL))
	  ((< compare 0)
	   (push (pop terms1) result))
	  (t (push (pop terms2) result)))))

(defmacro compile-formula (run-tms f &optional informant &aux ltms)
  (setq ltms (create-ltms f :COMPLETE :DELAY :DELAY-SAT nil))
  (dolist (clause (normalize ltms (expand-formula f)))
    (add-clause-internal clause nil t))
  (complete-ltms ltms)
  (generate-code ltms run-tms informant))

;;; Add clause
(defun add-clause (true-nodes false-nodes &optional informant)
  (add-clause-internal
    (sort-clause (nconc (mapcar #'tms-node-true true-nodes)
			(mapcar #'tms-node-false false-nodes)))
    informant
    nil))

;;; Consensus algorithm
(defun simplify-consensus (cl1 cl2 term1 conses &aux result compare)
  (macrolet ((push (a b) `(let ((cons (pop conses)))
			    (rplaca cons ,a)
			    (rplacd cons ,b)
			    (setq ,b cons))))
    (unless (and (clause-informant cl1)
		 (eq (clause-informant cl1)
		     (clause-informant cl2)))
      (do ((terms1 (clause-literals cl1))
	   (terms2 (clause-literals cl2)))
	  (nil)
	(cond ((null terms1) (return (nreconc result terms2)))
	      ((null terms2) (return (nreconc result terms1)))
	      ((eq (car terms1) (car terms2))
	       (push (pop terms1) result)
	       (pop terms2))
	      ((= 0 (setq compare (- (tms-node-index (caar terms1))
				     (tms-node-index (caar terms2)))))
	       (unless (eq (car terms1) term1)
		 (return nil))
	       (pop terms2) (pop terms1))
	      ((< compare 0)
	       (push (pop terms1) result))
	      (t (push (pop terms2) result)))))))

(defun simplify-subsume-consensus (ltms cl1 cl2 p &aux literals)
  (when (and (or (not (clause-informant cl1))
		 (not (eq (clause-informant cl1) (clause-informant cl2))))
	     (setq literals (simplify-consensus cl1 cl2
						p (ltms-conses ltms)))
	     (not (subsumed? literals (ltms-clauses ltms))))
    (process-clause ltms (copy-list literals)
		    `(RESOLVE ,cl1 ,cl2 ,p) t)))

;;; Maintaining the connection graph
(defmacro insert-clause (cl list)
  `(do ((cl-count (clause-length ,cl))
	(previous nil tail)
	(tail ,list (cdr tail)))
       ((or (null tail) (>= cl-count (clause-length (car tail))))
	(if previous
	    (rplacd previous (cons ,cl tail))
	    (setf ,list (cons ,cl tail))))))

(defun insert-true-clause (cl node)
  (insert-clause cl (tms-node-true-clauses node)))

(defun insert-false-clause (cl node)
  (insert-clause cl (tms-node-false-clauses node)))

(defun index-clause (cl ltms)
  (dolist (term (clause-literals cl))
    (ecase (cdr term)
      (:TRUE (insert-true-clause cl (car term)))
      (:FALSE (insert-false-clause cl (car term)))))
  (check-clauses ltms (list cl)))

(defun literal-connections (literal)
  (if (eq (cdr literal) :TRUE)
      (tms-node-false-clauses (car literal))
      (tms-node-true-clauses (car literal))))

;;; LTMS entry points.
(defun propagate-more-unknownness (old-value node ltms)
  (dolist (clause (ecase old-value
		    (:TRUE (tms-node-true-clauses node))
		    (:FALSE (tms-node-false-clauses node))))
    (when (and (= (decf (clause-sats clause)) 0)
	       (eq (clause-status clause) :DIRTY))
      (insert-queue clause ltms))))

(defun full-add-clause (ltms literals informant)
  (when (and (install-clause ltms literals informant)
	     (not (eq (ltms-complete ltms) :DELAY)))
    (check-for-contradictions ltms)
    (ipia ltms)))

;;; Implementing Tison's method
(defmacro insert-list2 (cl list)
  `(do ((cl-count (clause-length ,cl))
	(previous nil tail)
       (tail ,list (cdr tail)))
      ((or (null tail) (< cl-count (caar tail)))
       (if previous
	   (rplacd previous (cons (cons cl-count (cons ,cl nil)) tail))
	   (setf ,list
		 (cons (cons cl-count (cons ,cl nil)) tail))))
    (when (= cl-count (caar tail))
      (rplacd (car tail) (cons ,cl (cdar tail)))
      (return nil))))

(defun insert-queue (cl ltms)
  (setf (clause-status cl) :QUEUED)
  (insert-list2 cl (ltms-queue ltms)))

(defun insert-list-1 (cl list) (insert-list2 cl list) list)

(defmacro insert-list (cl list)
  `(setq ,list (insert-list-1 ,cl ,list)))

(defun complete-ltms (ltms &aux old)
  (setq old (ltms-complete ltms))
  (unwind-protect (progn (setf (ltms-complete ltms) T)
			 (ipia ltms))
    (setf (ltms-complete ltms) old)
    (check-for-contradictions ltms)))

(defmacro delay-sat? (clause ltms)
  `(when (and (ltms-delay-sat ,ltms) (satisfied-clause? ,clause))
    (setf (clause-status ,clause) :DIRTY)))

(defun ipia (ltms &aux px pxs sigma c slot)
  (do nil ((null (setq slot (car (ltms-queue ltms)))))
    (setq c (cadr slot))
    (if (cddr slot) (rplacd slot (cddr slot)) (pop (ltms-queue ltms)))
    (when (and (eq (clause-status c) :QUEUED)
	       (not (delay-sat? c ltms)))
      (setq sigma nil pxs nil)
      (insert-list c sigma)
      (dolist (lit (clause-literals c))
	(setq px nil)
	(dolist (p (literal-connections lit))
	  (cond ((eq (clause-status p) :QUEUED))
		((eq (clause-status p) :SUBSUMED))
		((and (eq (clause-status p) :DIRTY) (delay-sat? p ltms)))
		((not (simplify-consensus c p lit (ltms-conses ltms))))
		((delay-sat? p ltms))
		(t (push p px))))
	(push px pxs))
      (when (setq pxs (nreverse pxs))
	(dolist (lit (clause-literals c))
	  (when (setq px (pop pxs))
	    (dolist (l sigma)
	      (dolist (s (cdr l))
		(cond ((eq (clause-status s) :SUBSUMED))
		      ((not (member lit (clause-literals s))))
		      ((delay-sat? s ltms))
		      (t (setq sigma (ipia-inner ltms sigma px s lit)))))))))
      (if (eq (clause-status c) :QUEUED) (setf (clause-status c) nil))
      (dolist (l sigma)
	(dolist (s (cdr l))
	  (when (eq (clause-status s) :NOT-INDEXED)
	    (index-clause s ltms) (setf (clause-status s) nil)))))
    (check-for-contradictions ltms)))

(defun ipia-inner (ltms sigma px s lit &aux s-children consensus)
  (dolist (p px)
    (cond ((delay-sat? p ltms))
	  ((and (not (eq (clause-status p) :SUBSUMED))
		(setq consensus
		      (simplify-subsume-consensus ltms
						  s p lit)))
	   (setf (clause-status consensus) :NOT-INDEXED)
	   (when (eq (clause-status s) :SUBSUMED)
	     (insert-list consensus sigma)
	     (setq s-children nil)
	     (return nil))
	   (push consensus s-children))))
  (dolist (child s-children)
    (unless (eq (clause-status child) :SUBSUMED)
      (insert-list child sigma)))
  sigma)

;;; Maintaining the trie.
(defun subsumed? (lits trie &aux it slot)
  (dolist (entry trie)
    (unless lits (return nil))
    (when (setq slot (member (car entry) lits))
      (unless (listp (cdr entry)) (return (cdr entry)))
      (if (setq it (subsumed? (cdr slot) (cdr entry)))
	  (return it)))))

(defun add-to-trie (cl ltms &aux lits slot trie index entry)
  (setq lits (if (listp cl) cl (clause-literals cl)))
  (unless (setq trie (ltms-clauses ltms))
    (setf (ltms-clauses ltms) (build-trie lits cl))
    (return-from add-to-trie nil))
  (do ((lits lits (cdr lits))) (nil)
    (setq index (tms-node-index (caar lits)) slot nil)
    (do nil (nil)
      (setq entry (car trie))
      (cond ((eq (car lits) (car entry))
	     (setq slot entry)
	     (return nil))
	    ((> (tms-node-index (caar entry)) index) (return nil))
	    ((null (cdr trie)) (setq entry nil) (return nil)))
      (setq trie (cdr trie)))
    (unless slot
      (setq slot (build-trie lits cl))
      (cond (entry (rplacd slot (cdr trie))
		   (rplacd trie slot)
		   (rplaca trie (car slot))
		   (rplaca slot entry))
	    (t (rplacd trie slot)))
      (return nil))
    (setq trie (cdr slot))))

(defun build-trie (lits cl)
  (if (null lits)
      cl
      (cons (cons (car lits)
		  (build-trie (cdr lits) cl))
	    nil)))


(defun remove-subsumed (function lits ltms)
  (if (remove-subsumed-1 function lits (ltms-clauses ltms))
      (setf (ltms-clauses ltms) nil)))

(defun remove-subsumed-1 (function lits trie &aux au)
  (cond ((null lits) (walk-trie function trie) T)
	((atom trie) nil)
	(t (setq au (tms-node-index (caar lits)))
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
		 (setq previous subtrie subtrie (cdr subtrie)))))))

(defun walk-trie (function trie)
  (when trie
    (if (atom trie)
	(funcall function trie)
	(dolist (entry trie) (walk-trie function (cdr entry))))))

(defun collect (ltms &aux result)
  (walk-trie #'(lambda (cl) (push cl result)) (ltms-clauses ltms))
  result)

(defun remove-clause (old-clause new-clause &aux node)
  (unless (eq (clause-status old-clause) :NOT-INDEXED)
    (dolist (term (clause-literals old-clause))
      (ecase (cdr term)
	(:TRUE (setf (tms-node-true-clauses (car term))
		     (delete old-clause (tms-node-true-clauses (car term)))))
	(:FALSE (setf (tms-node-false-clauses (car term))
		      (delete old-clause (tms-node-false-clauses (car term))))))))
  (setf (clause-status old-clause) :SUBSUMED)
  (cond ((null (setq node (clause-consequent old-clause))))
	((assoc node (clause-literals new-clause))
	 (setf (tms-node-support node) new-clause) )
	;; A contradiction is being introduced.
	(t (find-alternative-support (tms-node-ltms node)
				     (propagate-unknownness node)))))

;;; Processing clauses.
(defun install-clause (ltms literals informant)
  (unless (subsumed? literals (ltms-clauses ltms))
    (process-clause ltms literals informant nil)))

(defun process-clause (ltms literals informant internal &aux cl)
  (setq cl (bcp-add-clause ltms literals informant nil))
  (remove-subsumed #'(lambda (old-clause)
		       (remove-clause old-clause cl))
		   literals ltms)
  (add-to-trie cl ltms)
  (cond (internal)
	((delay-sat? cl ltms)
	 (index-clause cl ltms))
	(t (index-clause cl ltms)
	   (insert-queue cl ltms)))
  cl)

(defun tms-env (node sign &aux label env)
  (if (tms-node-assumption? node)
      (push (list (ecase sign (:TRUE (tms-node-true node))
		              (:FALSE (tms-node-false node))))
	    label))
  (dolist (p (ecase sign (:TRUE (tms-node-true-clauses node))
		         (:FALSE (tms-node-false-clauses node))))
    (unless (dolist (lit (clause-literals p))
	      (or (eq (car lit) node)
		  (tms-node-assumption? (car lit))
		  (return T)))
      (setq env nil)
      (dolist (lit (clause-literals p))
	(unless (eq (car lit) node)
	  (push (if (eq (cdr lit) :TRUE)
		    (tms-node-false (car lit))
		    (tms-node-true (car lit)))
		env)))
      (push env label)))
  label)

(defun pi (formula &aux ltms tltms literals clauses)
  (setq ltms (create-ltms "Prime Implicates")
	tltms (create-ltms "Prime Implicates"
			   :COMPLETE :DELAY :DELAY-SAT nil)
	clauses (normalize ltms formula))
  (maphash #'(lambda (ignore node)
	       (setf (tms-node-mark node) 0)
	       (push node literals))
	   (ltms-nodes ltms))
  (dolist (clause clauses)
    (dolist (literal clause)
      (incf (tms-node-mark (car literal)))))
  (dolist (literal (sort literals #'< :KEY #'tms-node-mark))
    (setf (tms-node-mark  literal)
	  (tms-create-node tltms (tms-node-datum literal))))
  (dolist (clause clauses)
    (map-over clause tms-node-mark nil))
  (complete-ltms tltms)
  (format T "~% There now are ~D clauses" (length (collect tltms)))
  tltms)
