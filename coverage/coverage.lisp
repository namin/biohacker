;; must load ../BPS/utils/init.lisp
;; must load ../BPS/jtms/jtre.lisp & evaluate (compie-jtre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *debugging-coverage* t)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(setq *jtre* (create-jtre "test" :debugging t))

(rule ((:IN (nutrient ?x) :var ?def))
      (rassert! (compound ?x)
		(:NUTRIENT-IS-COMPOUND ?def)))

(rule ((:IN (reaction ?reaction ?reactants . ?products) :var ?def))
      (let ((inputs (mapcar #'(lambda (reactant)
				`(compound ,reactant))
			    ?reactants)))
	(dolist (product ?products)
	  (assert! `(compound ,product)
		   `(:PRODUCT-OF-REACTION ,?def ,@inputs)))))

(defmacro reaction (name reactants &rest products)
  (assert! `(quote (reaction ,name ,reactants ,@products)) '(:BELIEF (UNIVERSAL))))

(assert! '(nutrient A) '(:BELIEF (UNIVERSAL)))
(assert! '(nutrient B) '(:BELIEF (UNIVERSAL)))
(assert! '(reaction R1 (A B) C G) '(:BELIEF (UNIVERSAL)))
(assume! '(UNIVERSAL) :UNIVERSAL)
(run-rules)
(retract! '(UNIVERSAL) :UNIVERSAL)
(run-rules)

(defun check-universal-compounds (compounds &aux ok not-ok fetched)
  (assume! '(UNIVERSAL) :UNIVERSAL)
  (run-rules)
  (dolist (compound compounds)
    (cond ((null (setq fetched (fetch `(compound ,compound))))
	   (debugging-coverage "~% Compound ~A missing." compound)
	   (push compound not-ok))
	  ((in-node? (get-tms-node (car fetched)))
	   (debugging-coverage "~% Compound ~A in." compound)
	   (push compound ok))
	  (t
	   (debugging-coverage "% Compound ~A out." compound)
	   (push compound not-ok))))
  (retract! '(UNIVERSAL) :UNIVERSAL)
  (run-rules)
  (values ok not-ok))

