(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "jcoverage-rules"))

(defvar *debug-rules-file*
  (make-bps-source-file-name *coverage-path* "jcoverage-debug-rules"))

(defvar *debugging-coverage* t)

(defvar *organism* nil)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(defun create-coverage-problem (&key (debugging nil))
  (setq *jtre* (create-jtre "Coverage Problem" :debugging debugging))
  (setq *organism* nil)
  (load *coverage-rules-file*)
  *jtre*)

(defmacro reaction (name reactants &rest products)
  `(assert! '(reaction ,name ,reactants ,@products) '(:IS-ENABLED (reaction-enabled ,name))))

(defmacro growth (&rest compounds)
  `(assert! '(growth)
	    '(:SUFFICIENT-FOR-GROWTH ,@ (mapcar #'(lambda (compound)
						    `(compound ,compound))
						compounds))))

(defmacro organism (name))

(defmacro enzyme (enzyme &rest genes &aux gene-forms statements)
  (setq gene-forms
	(mapcar #'(lambda (gene)
		   (let ((gene-form `(gene ,gene))) 
		     (push `(assert! ',gene-form '(:IS-ON (gene-on ,gene))) statements)
		     gene-form))
		genes))
  (push `(assert! '(enzyme ,enzyme)
		  '(:FORMED-BY ,@gene-forms))
	statements)
  (push 'progn statements)
  statements)

(defmacro catalyze (reaction &rest enzymes &aux enzyme-forms)
  (setq enzyme-forms (mapcar #'(lambda (enzyme) `(enzyme ,enzyme)) enzymes))
  `(assert! '(reaction-enabled ,reaction) '(:CATALYZED-BY ,@enzyme-forms)))

(defun retract-all-experiments ()
  (dolist (node (enabled-assumptions (jtre-jtms *jtre*)))
    (let ((form (view-node node)))
      (when (and (listp form)
		 (eq (car form) 'experiment))
	(retract! form ':EXPERIMENT)))))

(defmacro experiment (outcome &key (nutrients nil) (off nil))
  `(progn
     (retract-all-experiments)
     (let ((exp-form '(experiment ,outcome ,nutrients ,@off)))
       (assume! exp-form ':EXPERIMENT)
       (run-rules)
       (in? '(growth)))))

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
  (if (in? `(compound ,compound))
      (progn
	(format t "~%Compound ~A is produced." compound)
	t)
    (progn
      (dolist (reaction (reactions-for-product compound))
      (let ((missing-reactants 
	     (sort-symbols
	      (remove-if 
	       #'(lambda (reactant) (in? `(compound ,reactant))) 
	       (reactants-for-reaction reaction)))))
	(format t "~%Reaction ~A missing reactants ~A" 
		reaction 
		missing-reactants)
	(push (cons reaction missing-reactants) or-and-list)))
    (unless or-and-list
      (format t "~%Compound ~A is not a product." compound))
    or-and-list)))

(defun coverage-why-node (node &optional (include-genes nil) &aux skip-list justification)
  (setq skip-list '(gene gene-on reaction nutrient))
  (when (not include-genes)
    (setq skip-list (append '(reaction-enabled enzyme) skip-list)))
  (setq justification (tms-node-support node))
  (cond ((eq justification :ENABLED-ASSUMPTION)
	 (unless (and (listp (view-node node)) (eq (car (view-node node)) 'experiment)) 
	   (format t "~%~A is an enabled assumption"
		   (node-string node))))
	(justification
	 (unless (and (listp (view-node node)) 
		      (find (car (view-node node)) skip-list))
	     (if (eq ':NUTRIENT-IS-COMPOUND (just-informant justification))
	      (format t "~%~A" (node-string (car (just-antecedents justification))))
	    (progn
	      (format t "~%~A is IN via ~A"
		      (node-string node)
		      (just-informant justification))
	      (unless (find (just-informant justification) (list :EXPERIMENT-SETUP ':IS-ON))
		(format t " on ")
		(dolist (anode (just-antecedents justification))
		  (format t "~%  ~A" (node-string anode))))))))
	(T (format t "~%~A is OUT." (node-string node))))
  node)

(defun coverage-wfs (fact &optional (include-genes nil) (*JTRE* *JTRE*))
  ;; Displays well-founded support for a fact
  (cond ((out? fact) (format t "~% ~A is OUT." fact))
	(t (do ((queue (list (get-tms-node fact))
		       (nconc (cdr queue) new-antes))
		(so-far (list (get-tms-node fact)))
		(new-antes nil nil))
	       ((null queue) (format t "~%--------") fact)
	     (coverage-why-node (car queue) include-genes)
	     (unless (or (out-node? (car queue))
			 (tms-node-assumption? (car queue)))
	       ;; Go down the support
	       (dolist (ante (just-antecedents
			      (tms-node-support (car queue))))
		 (unless (member ante so-far)
		   (push ante so-far)
		   (push ante new-antes))))))))