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

(defun nutrients (env &aux forms nutrients)
  (setq forms (env-forms env))
  (setq nutrients (remove-if-not #'(lambda (form)
				     (eq (car form)
					 'NUTRIENT))
				 forms))
  (setq nutrients (mapcar #'cadr nutrients))
  (sort nutrients (lambda (a b)
		    (string-lessp (string a)
				  (string b)))))

(defun nutrients-sufficient-for-growth (&aux universal-node envs)
  (setq universal-node (get-tms-node '(UNIVERSAL)))
  (setq envs (assumptions-of '(GROWTH)))
  (setq envs (remove-if-not #'(lambda (env)
			      (find universal-node (env-assumptions env)))
			    envs))
  (mapcar #'nutrients envs))

(defun closed-environment-of (nutrients disabled-reactions &rest extra-forms &aux not-disabled-reactions)
  (setq nutrients (mapcar #'(lambda (nutrient) `(nutrient ,nutrient)) nutrients))
  (setq not-disabled-reactions (remove-if #'(lambda (form)
						  (find (cadadr form) disabled-reactions)) 
					 (fetch '(not (disabled-reaction ?r)))))
  (environment-of (append extra-forms nutrients not-disabled-reactions)))

(defun direct-outcome (nutrients disabled-reactions &aux env)
  (setq env (closed-environment-of nutrients disabled-reactions '(UNIVERSAL)))
  (if (in? '(GROWTH) env) 'GROWTH 'NO-GROWTH))

(defun recs-of-env (recs)
  #'(lambda (le) 
      (mapcar 
       #'cadr
       (remove-if-not 
	#'(lambda (rec) 
	    (in-node? (get-tms-node rec) le))
	recs))))

(defun growth->no-growth (nutrients disabled-reactions extra-forms &aux env les recs recs-of-env)
  (setq env (closed-environment-of nutrients disabled-reactions '(UNIVERSAL)))
  (setq les (remove-if-not #'(lambda (le) (subset-env? le env))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (fetch '(enabled-reaction ?r)))
  (remove-duplicates (mapcar (recs-of-env recs) les) :test #'equal))

(defun just-nutrients (env)
  (mapcar #'cadr
   (remove-if-not
    #'(lambda (form)
	(eq (car form) 'nutrient))
    (mapcar #'(lambda (node)
	       (datum-lisp-form (tms-node-datum node))) 
	    (env-assumptions env)))))

(defun no-growth->growth (nutrients disabled-reactions extra-forms &aux env les)
  (setq env (environment-of extra-forms))
  (unless (consistent-with? '(GROWTH) env)
    (return-from no-growth->growth nil))
  (setq les (remove-if-not #'(lambda (le)
			       (and (subset-env? env le)
				    (subsetp (just-nutrients le) nutrients)))
			   (tms-node-label (get-tms-node '(GROWTH)))))
  (setq recs (mapcar #'(lambda (rec) `(enabled-reaction ,rec)) disabled-reactions))
  (remove-duplicates (mapcar (recs-of-env recs) les) :test #'equal))

(defun flip-outcome (nutrients disabled-reactions &aux outcome)
  (setq outcome (direct-outcome nutrients disabled-reactions))
  (cond ((eq outcome 'growth)
	 (format t "Outcome is growth.~% For no-growth, disable ONE reaction from EACH set:")
	 (growth->no-growth nutrients disabled-reactions '((UNIVERSAL))))
	(t
	 (format t "Outcome is no-growth.~% For growth, enable EACH reaction from ONE set:")
	 (no-growth->growth nutrients disabled-reactions '((UNIVERSAL))))))
