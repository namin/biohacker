;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "coverage2-rules"))

(defvar *debug-rules-file*
  (make-bps-source-file-name *coverage-path* "coverage2-debug-rules"))

(defvar *debugging-coverage* t)

(defvar *organism* nil)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(defun create-coverage-problem (&key (debugging nil))
  (setq *atre* (create-atre "Coverage Problem" :debugging debugging))
  (setq *organism* nil)
  (load *coverage-rules-file*)
  *atre*)

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

(defmacro experiment (outcome &key (nutrients nil) (off nil))
  `(let ((exp-form '(experiment ,outcome ,nutrients ,@off)))
     (assume! exp-form '(:EXPERIMENT))
     (let ((env (environment-of (list exp-form))))
       (change-focus env)
       (run-rules)
       (in? '(growth) env))))

(defun load-debug-rules ()
  (load *debug-rules-file*))

(defun reactions-for-product (compound)
  (mapcar #'(lambda (form) (caddr form)) 
	  (fetch `(product ,compound ?r))))

(defun reactants-for-reaction (reaction)
  (mapcar #'(lambda (form) (cadr form)) 
	  (fetch `(reactant ?r ,reaction))))

(defun why-not? (compound &optional (env *env*) &aux or-and-list)
  (if (in? `(compound ,compound) env)
      (progn
	(format t "~%Compound ~A is produced." compound)
	t)
    (dolist (reaction (reactions-for-product compound) or-and-list)
      (let ((missing-reactants 
	     (remove-if #'(lambda (reactant) (in? `(compound ,reactant) env)) 
			(reactants-for-reaction reaction))))
	(format t "~% Reaction ~A missing reactants ~A" 
		reaction 
		missing-reactants)
	(push (cons reaction missing-reactants) or-and-list)))))