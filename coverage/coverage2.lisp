;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "coverage2-rules"))

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
  `(assume! '(experiment ,outcome ,nutrients ,@off) :EXPERIMENT))