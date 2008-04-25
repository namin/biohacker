;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "coverage-rules"))

(defvar *debugging-coverage* t)

(defvar *organism* nil)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(defun create-coverage-problem (&key (debugging nil))
  (setq *atre* (create-atre "Coverage Problem" :debugging debugging))
  (setq *organism* nil)
  (load *coverage-rules-file*)
  (assume! '(UNIVERSAL) :UNIVERSAL)
  (assume! '(NO-GROWTH) :NO-GROWTH)
  *atre*)

(defmacro reaction (name reactants &rest products)
  `(assert! '(reaction ,name ,reactants ,@products) '(:IS-ENABLED (enabled-reaction ,name))))

(defmacro growth (&rest compounds)
  `(assert! '(growth)
	    '(:SUFFICIENT-FOR-GROWTH ,(organism-assumption *organism*)
				     ,@ (mapcar #'(lambda (compound)
						    `(compound ,compound))
						compounds))))

(defmacro organism (name)
  `(progn
    (setq *organism* ',name)
    (assume! '(organism ,name) :ORGANISM)))

(defmacro enzyme (enzyme &rest genes &aux gene-forms organism-assumption statements)
  (setq organism-assumption (organism-assumption *organism*))
  (setq gene-forms
	(mapcar #'(lambda (gene)
		   (let ((gene-form `(gene ,gene))) 
		     (push `(assert! ',gene-form '(:IS-ON ,organism-assumption (not (off ,gene)))) statements)
		     gene-form))
		genes))
  (push `(assert! '(enzyme ,enzyme)
		  '(:FORMED-BY ,organism-assumption ,@gene-forms))
	statements)
  (push 'progn statements)
  statements)

(defmacro catalyze (reaction &rest enzymes &aux enzyme-forms)
  (setq enzyme-forms (mapcar #'(lambda (enzyme) `(enzyme ,enzyme)) enzymes))
  `(assert! '(enabled-reaction ,reaction) '(:CATALYZED-BY ,@enzyme-forms (not (disabled-reaction ,reaction)))))

(defun env-forms (env)
  (mapcar #'(lambda (node)
	      (datum-lisp-form (tms-node-datum node)))
	  (env-assumptions env)))

(defun env-not-bits (env class &aux forms bits)
  (setq forms (env-forms env))
  (setq bits (remove-if-not #'(lambda (form)
				(and (not (null (cdr form))) (listp (cadr form)) 
				     (eq (caadr form)
					 class)))
				 forms))
  (setq bits (mapcar #'cadadr bits))
  (sort bits (lambda (a b)
		    (string-lessp (string a)
				  (string b)))))

(defun env-bits (env class &aux forms bits)
  (setq forms (env-forms env))
  (setq bits (remove-if-not #'(lambda (form)
				(eq (car form)
				    class))
				 forms))
  (setq bits (mapcar #'cadr bits))
  (sort bits (lambda (a b)
		    (string-lessp (string a)
				  (string b)))))

(defun env-nutrients (env &aux forms nutrients)
  (env-bits env 'nutrient))

(defun organism-assumption (organism)
  (if organism
      `(organism ,organism)
    '(UNIVERSAL)))

(defun nutrients-sufficient-for-growth (&key (organism *organism*) &aux organism-env envs)
  (setq organism-env (environment-cons (organism-assumption organism)
				       (just-not-disabled-reactions-environment)))
  (setq envs (remove-if #'(lambda (env)
			    (env-nogood? (union-env env organism-env)))
			(assumptions-of '(GROWTH))))
  (mapcar #'env-nutrients envs))

(defun negative-forms-except (lst class)
  (remove-if #'(lambda (form)
		 (find (cadadr form) lst)) 
	     (fetch `(not (,class ?x)))))

(defun just-nutrients-environment-of (nutrients)
  (setq nutrients (mapcar #'(lambda (nutrient) `(nutrient ,nutrient)) nutrients))
  (environment-of nutrients))

(defun just-not-disabled-reactions-environment (&optional disabled-reactions)
  (environment-of (negative-forms-except disabled-reactions 'disabled-reaction)))

(defun closed-reaction-environment-of (nutrients disabled-reactions &rest extra-forms)
  (union-env (environment-of extra-forms) 
	     (union-env (just-nutrients-environment-of nutrients) 
			(just-not-disabled-reactions-environment disabled-reactions))))

(defun env-outcome (env)
  (if (in? '(GROWTH) env) 'GROWTH 'NO-GROWTH))

(defun direct-outcome (nutrients disabled-reactions &key (organism *organism*) &aux env)
  (setq env (closed-reaction-environment-of nutrients disabled-reactions (organism-assumption organism)))
  (env-outcome env))

(defun forms->env->env-forms (forms)
  #'(lambda (env) 
      (mapcar 
       #'cadr
       (remove-if-not 
	#'(lambda (form) 
	    (in-node? (get-tms-node form) env))
	forms))))

(defun unique-sets (sets)
  (remove-duplicates sets :test #'equal))

(defun unique-env-form-sets (les forms)
  (unique-sets (mapcar (forms->env->env-forms forms) les)))

(defun growth->no-growth (nutrients disabled-reactions &key (organism *organism*) &aux env les recs recs-of-env)
  (setq env (closed-reaction-environment-of nutrients disabled-reactions (organism-assumption organism)))
  (setq les (remove-if-not #'(lambda (le) (subset-env? le env))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (fetch '(enabled-reaction ?r)))
  (unique-env-form-sets les recs))

(defun organism-environment (organism)
  (environment-of (list (organism-assumption organism))))

(defun no-growth->growth (nutrients disabled-reactions &key (organism *organism*) &aux env les)
  (setq env (organism-environment organism))
  (unless (consistent-with? '(GROWTH) env)
    (return-from no-growth->growth nil))
  (setq les (remove-if-not #'(lambda (le)
			       (and (subset-env? env le)
				    (subsetp (env-nutrients le) nutrients)))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (mapcar #'(lambda (rec) `(enabled-reaction ,rec)) disabled-reactions))
  (unique-env-form-sets les recs))

(defun flip-outcome (nutrients disabled-reactions &key (organism *organism*) &aux outcome sets)
  (setq outcome (direct-outcome nutrients disabled-reactions :organism organism))
  (cond ((eq outcome 'growth)
	 (setq sets (growth->no-growth nutrients disabled-reactions :organism organism))
	 (debugging-coverage "~%Outcome is growth.~% For no-growth, disable ONE reaction from EACH set:~% ~A" sets)
	 (values sets
		 outcome))
	(t
	 (setq sets (no-growth->growth nutrients disabled-reactions :organism organism))
	 (debugging-coverage "~%Outcome is no-growth.~% For growth, enable EACH reaction from ONE set:~% ~A" sets)
	 (values sets
		 outcome))))

(defun just-genetic-environment-of (off-genes &aux not-off-genes)
  (setq not-off-genes (negative-forms-except off-genes 'off))
  (environment-of not-off-genes))

(defun closed-genetic-environment-of (nutrients off-genes &rest extra-forms &aux env)
  (setq env (apply #'closed-reaction-environment-of `(,nutrients ,nil ,@extra-forms)))
  (union-env env (just-genetic-environment-of off-genes)))

(defun experiment-outcome (nutrients off-genes &key (organism *organism*))
  (env-outcome (closed-genetic-environment-of nutrients off-genes (organism-assumption organism))))

(defun remove-reactions-enabled-in (env)
  #'(lambda (set) 
    (remove-if
    #'(lambda (name)
	(in-node? (get-tms-node `(enabled-reaction ,name)) env))
    set)))

(defun experiment-no-growth->growth (nutrients off-genes env &key (organism *organism*) &aux organism-env off-genes-env sets)
  ;; see if we can get growth by assuming some reactions
  (setq off-genes-env (just-genetic-environment-of off-genes))
  (setq organism-env (organism-environment organism))
  (setq les (remove-if-not #'(lambda (le)
			       (and (not (env-nogood? (union-env le off-genes-env))) 
				    (subset-env? organism-env le)
				    (subsetp (env-nutrients le) nutrients)
				    ))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq sets (mapcar #'(lambda (le) (env-bits le 'assumed-reaction)) les))
  (setq sets (mapcar (remove-reactions-enabled-in env) sets))
  (setq sets (remove nil sets))
  (setq sets (unique-sets sets))
  (debugging-coverage "~% Enable EACH reaction from ONE set: ~% ~A" sets)
  sets)

(defun experiment-growth->no-growth (nutrients off-genes env &key (organism *organism*) &aux les recs sets)
  (setq les (remove-if-not #'(lambda (le) (subset-env? le env))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (fetch '(enabled-reaction ?r)))
  (setq sets (unique-env-form-sets les recs))
  (debugging-coverage "~% Disable ONE reaction from EACH set: ~% ~A" sets)
  sets)

(defun why-reaction (reaction &key (organism *organism*) &aux reaction-node env les sets)
  (setq reaction-node (get-tms-node `(enabled-reaction ,reaction)))
  (setq env (environment-cons (organism-assumption organism) 
			      (just-not-disabled-reactions-environment)))
  (setq les
       (remove-if #'(lambda (le)
		      (env-nogood? (union-env le env)))
	(tms-node-label reaction-node)))
  (setq sets (mapcar #'(lambda (le) (env-not-bits le 'off)) les))
  (debugging-coverage "~% Each set of genes is sufficient for reaction ~A: ~A" reaction sets)
  sets)

(defun	fix-organism-network-from-experiment (nutrients off-genes actual-outcome calculated-outcome env &key (organism *organism*))
  (if (eq actual-outcome 'growth)
      (experiment-no-growth->growth nutrients off-genes env :organism organism)
    (experiment-growth->no-growth nutrients off-genes env :organism organism)))

(defun ensure-experiment-coherent (actual-outcome nutrients off-genes &key (organism *organism*) &aux calculated-outcome env)
  (setq env (closed-genetic-environment-of nutrients off-genes (organism-assumption organism)))
  (setq calculated-outcome (env-outcome env))
  (cond ((eq actual-outcome calculated-outcome)
	 (debugging-coverage "~%Experiment is coherent. Expected and calculated outcome ~A." actual-outcome)
	 t)
	(t
	 (debugging-coverage "~%Experiment is not coherent. Expected outcome ~A from experiment but calculated outcome ~A." actual-outcome calculated-outcome)
	 (fix-organism-network-from-experiment nutrients off-genes
					       actual-outcome calculated-outcome
					       env
					       :organism organism))))

(defun experiment-support (outcome nutrients off-genes &key (organism *organism*))
  (ensure-experiment-coherent outcome nutrients off-genes))

(defmacro experiment (outcome &key (nutrients nil) (off nil))
  `(experiment-support ',outcome ',nutrients ',off))
