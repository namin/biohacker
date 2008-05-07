(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "lcoverage-rules"))

(defvar *debug-rules-file*
  (make-bps-source-file-name *coverage-path* "lcoverage-debug-rules"))

(defvar *debugging-coverage* t)

(defvar *organism* nil)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(defun coverage-node-string (node)
  (let ((form (datum-lisp-form (tms-node-datum node))))
    (cond ((and (listp form)
		(eq (car form) 'experiment))
	   "(experiment ...)")
	  (t (format nil "~A" form)))))

(defun create-coverage-problem (&key (debugging nil))
  (setq *ltre* (create-ltre "Coverage Problem" :debugging debugging))
  (change-ltms (ltre-ltms *ltre*) :node-string 'coverage-node-string)
  (setq *organism* nil)
  (load *coverage-rules-file*)
  *ltre*)

(defmacro network-debugger (name &key (debugging nil))
  `(create-coverage-problem :debugging ,debugging))

(defmacro reaction (name
		    &key
		    reactants
		    products
		    (reversible? :UNKNOWN)
		    (enzymes nil))
  `(progn
     (reaction-old ,name ,reactants ,@products)
     ,@(dolist (enzyme enzymes)
	 `(catalyze-old ,name ,enzyme))))

(defmacro reaction-old (name reactants &rest products)
  `(assert! '(:IMPLIES (reaction-enabled ,name) (reaction ,name ,reactants ,@products)) ':REACTION))

(defmacro growth-old (&rest compounds)
  `(assert! '(sufficient-for-growth
	      ,@ (mapcar #'(lambda (compound)
			     `(compound ,compound))
			 compounds))
	    :GROWTH))

(defmacro organism (name))

(defmacro enzyme (enzyme &rest genes &aux gene-forms statements)
  `(assert! '(sufficient-for-enzyme 
	      ,enzyme 
	      ,@(mapcar #'(lambda (gene)
			    `(gene-on ,gene)) 
			genes))
	    :ENZYME))

(defmacro catalyze-old (reaction &rest enzymes &aux enzyme-forms)
  `(assert! '(sufficient-for-reaction 
	      ,reaction 
	      ,@(mapcar #'(lambda (enzyme)
			    `(enzyme ,enzyme)) 
			enzymes))
	    :CATALYZE))

(defun retract-all-experiments ()
  (dolist (form (fetch '(experiment . ?x)))
    (when (known? form)
      (retract! form ':EXPERIMENT))))

(defmacro experiment (name 
		      nutrients
		      &key
		      growth?
		      (knock-outs nil)
		      (knock-ins nil)
		      (toxins nil)
		      (bootstrap-compounds nil)
		      essential-compounds)
  `(progn
     (experiment-old ,(if growth? 'growth 'no-growth) :nutrients ,nutrients :off ,knock-outs)
     (growth-old ,@essential-compounds)))

(defmacro experiment-old (outcome &key (nutrients nil) (off nil))
  `(progn
     (stop-investigating-experiment)
     (retract-all-experiments)
     (let ((exp-form '(experiment ,outcome ,nutrients ,@off)))
       (assume! exp-form ':EXPERIMENT)
       (run-rules)
       (true? 'growth))))

(defun load-debug-rules ()
  (load *debug-rules-file*)
  (run-rules))

(defun reactions-for-product (compound)
  (mapcar #'(lambda (form) (caddr form)) 
	  (fetch `(product ,compound ?r))))

(defun reactants-for-reaction (reaction)
  (mapcar #'(lambda (form) (cadr form)) 
	  (fetch `(reactant ?r ,reaction))))

(defun sort-symbols (symbols)
  (sort symbols 
	#'(lambda (x y) (string-lessp (string x) (string y)))))

(defun why-not? (compound &optional &aux or-and-list)
  (if (true? `(compound ,compound))
      (progn
	(format t "~%Compound ~A is produced." compound)
	t)
    (progn
      (dolist (reaction (reactions-for-product compound))
      (let ((missing-reactants 
	     (sort-symbols
	      (remove-if 
	       #'(lambda (reactant) (true? `(compound ,reactant))) 
	       (reactants-for-reaction reaction)))))
	(format t "~%Reaction ~A missing reactants ~A" 
		reaction 
		missing-reactants)
	(push (cons reaction missing-reactants) or-and-list)))
    (unless or-and-list
      (format t "~%Compound ~A is not a product." compound))
    or-and-list)))

(defun start-investigating-experiment ()
  (assume! 'cwa :CWA)
  (run-rules)
  (when (false? 'investigating-experiment)
    (retract! '(:NOT investigating-experiment) :CHECK))
  (when (unknown? 'investigating-experiment)
    (assume! 'investigating-experiment :CHECK)))

(defun stop-investigating-experiment ()
  (when (true? 'cwa) 
    (retract! 'cwa :CWA))
  (when (true? 'investigating-experiment)
    (retract! 'investigating-experiment :CHECK))
  (when (unknown? 'investigating-experiment)
    (assume! '(:NOT investigating-experiment) :CHECK)))