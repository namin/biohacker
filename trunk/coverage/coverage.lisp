;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "coverage-rules"))

(defvar *debugging-coverage* t)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(defun create-coverage-problem (&key (debugging nil))
  (setq *atre* (create-atre "Coverage Problem" :debugging debugging))
  (load *coverage-rules-file*)
  *atre*)

(defmacro reaction (name reactants &rest products)
  `(assert! '(reaction ,name ,reactants ,@products) '(:IS-ENABLED (enabled-reaction ,name))))

(defmacro growth (&rest compounds)
  `(assert! '(growth)
	    '(:SUFFICIENT-FOR-GROWTH ,@ (mapcar #'(lambda (compound)
						    `(compound ,compound))
						compounds))))
(defun env-forms (env)
  (mapcar #'(lambda (node)
	      (datum-lisp-form (tms-node-datum node)))
	  (env-assumptions env)))

(defun env-nutrients (env &aux forms nutrients)
  (setq forms (env-forms env))
  (setq nutrients (remove-if-not #'(lambda (form)
				     (eq (car form)
					 'NUTRIENT))
				 forms))
  (setq nutrients (mapcar #'cadr nutrients))
  (sort nutrients (lambda (a b)
		    (string-lessp (string a)
				  (string b)))))

(defun organism-assumption (organism)
  (if organism
      `(organism ,organism)
    '(UNIVERSAL)))

(defun nutrients-sufficient-for-growth (&key (organism nil) &aux node envs)
  (setq node (get-tms-node (organism-assumption organism)))
  (setq envs (assumptions-of '(GROWTH)))
  (setq envs (remove-if-not #'(lambda (env)
			      (find node (env-assumptions env)))
			    envs))
  (mapcar #'env-nutrients envs))

(defun closed-environment-of (nutrients disabled-reactions &rest extra-forms &aux not-disabled-reactions)
  (setq nutrients (mapcar #'(lambda (nutrient) `(nutrient ,nutrient)) nutrients))
  (setq not-disabled-reactions (remove-if #'(lambda (form)
						  (find (cadadr form) disabled-reactions)) 
					 (fetch '(not (disabled-reaction ?r)))))
  (environment-of (append extra-forms nutrients not-disabled-reactions)))

(defun direct-outcome (nutrients disabled-reactions &key (organism nil) &aux env)
  (setq env (closed-environment-of nutrients disabled-reactions (organism-assumption organism)))
  (if (in? '(GROWTH) env) 'GROWTH 'NO-GROWTH))

(defun forms->env->env-forms (forms)
  #'(lambda (env) 
      (mapcar 
       #'cadr
       (remove-if-not 
	#'(lambda (form) 
	    (in-node? (get-tms-node form) env))
	forms))))

(defun unique-env-form-sets (les forms)
  (remove-duplicates (mapcar (forms->env->env-forms forms) les) :test #'equal))

(defun growth->no-growth (nutrients disabled-reactions &rest extra-forms &aux env les recs recs-of-env)
  (setq env (apply #'closed-environment-of `(,nutrients ,disabled-reactions ,@extra-forms)))
  (setq les (remove-if-not #'(lambda (le) (subset-env? le env))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (fetch '(enabled-reaction ?r)))
  (unique-env-form-sets les recs))

(defun no-growth->growth (nutrients disabled-reactions &rest extra-forms &aux env les)
  (setq env (environment-of extra-forms))
  (unless (consistent-with? '(GROWTH) env)
    (return-from no-growth->growth nil))
  (setq les (remove-if-not #'(lambda (le)
			       (and (subset-env? env le)
				    (subsetp (env-nutrients le) nutrients)))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (mapcar #'(lambda (rec) `(enabled-reaction ,rec)) disabled-reactions))
  (unique-env-form-sets les recs))

(defun flip-outcome (nutrients disabled-reactions &key (organism nil) &aux outcome sets)
  (setq outcome (direct-outcome nutrients disabled-reactions :organism organism))
  (cond ((eq outcome 'growth)
	 (setq sets (growth->no-growth nutrients disabled-reactions (organism-assumption organism)))
	 (debugging-coverage "Outcome is growth.~% For no-growth, disable ONE reaction from EACH set:~% ~A" sets)
	 (values sets
		 outcome))
	(t
	 (setq sets (no-growth->growth nutrients disabled-reactions (organism-assumption organism)))
	 (debugging-coverage "Outcome is no-growth.~% For growth, enable EACH reaction from ONE set:~% ~A" sets)
	 (values sets
		 outcome))))
