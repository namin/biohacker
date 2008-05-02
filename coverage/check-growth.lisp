;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *coverage-rules-file*
  (make-bps-source-file-name *coverage-path* "check-growth-rules"))

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
  `(assert! '(reaction ,name ,reactants ,@products) '(:REACTION)))

(defmacro growth (&rest compounds)
  `(assert! '(growth ,@compounds) '(:GROWTH)))

(defmacro organism (name))

(defmacro enzyme (enzyme &rest genes &aux gene-forms statements))

(defmacro catalyze (reaction &rest enzymes &aux enzyme-forms))

(defmacro experiment (outcome &key (nutrients nil) (off nil)))

(defun check-essential-compounds ()
  (defun print-from-forms (forms)
    (if forms
	(dolist (compound 
		 (sort (mapcar #'cadr forms) 
		       #'(lambda (x y) (string-lessp (string x) (string y)))))
	  (format t "~%  ~A" compound))
      (format t     "~%  (None)")))
  (format t "~%These compounds are absent from reactions:")
  (print-from-forms (fetch '(absent ?x)))
  (format t "~%")
  (format t "~%These compounds are present only as reactants:")
  (print-from-forms (fetch '(present-reactant ?x)))
  (format t "~%")
  (format t "~%These compounds are present only as products:")
  (print-from-forms (fetch '(present-product ?x)))
  (format t "~%")
  (format t "~%These compounds are present as both reactants and products:")
  (print-from-forms (fetch '(present ?x)))
  (format t "~%")
  (if (= (list-length (fetch '(essential ?x)))
	 (+ (list-length (fetch '(absent ?x)))
	    (list-length (fetch '(present ?x)))
	    (list-length (fetch '(present-reactant ?x)))
	    (list-length (fetch '(present-product ?x)))))
      (format t "~%All essential compounds covered.")
    (format t "~%BUG :(")))

