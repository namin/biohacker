;; must load ../BPS/utils/init.lisp
;; must load ../BPS/atms/atre.lisp & evaluate (compie-atre)

(defvar *coverage-path*
  (make-path *trunk-home* "coverage"))

(defvar *debugging-coverage* t)

(defmacro debugging-coverage (msg &rest args)
  `(when *debugging-coverage* (format t ,msg ,@ args)))

(setq *atre* (create-atre "test" :debugging t))

(rule :INTERN ((nutrient ?x) :var ?def)
      (rassert! (compound ?x)
		(:NUTRIENT-IS-COMPOUND ?def)))

(rule :INTERN ((reaction ?reaction ?reactants . ?products) :var ?def)
      (let ((inputs (mapcar #'(lambda (reactant)
				`(compound ,reactant))
			    ?reactants)))
	(dolist (product ?products)
	  (assert! `(compound ,product)
		   `(:PRODUCT-OF-REACTION ,?def ,@inputs)))))

(rule :INTERN ((growth))
      (rnogood! :OUTCOME (growth) (no-growth)))

(rule :INTERN ((reaction ?name ?reactants . ?products))
      (dolist (?compound (append ?reactants ?products))
	(rassume! (nutrient ?compound) :POTENTIAL-NUTRIENT))
      (rassume! (assumed-reaction ?name) :POTENTIAL-REACTION)
      (rassume! (disabled-reaction ?name) :DISABLED-REACTION)
      (rassume! (not (disabled-reaction ?name)) :DEFAULT-REACTION)
      (rassert! (enabled-reaction ?name) (:ADD (assumed-reaction ?name)))
      (rassert! (enabled-reaction ?name) (:BELIEF (UNIVERSAL) (not (disabled-reaction ?name)))))


(rule :INTERN ((disabled-reaction ?name) :var ?def)
      (rnogood! :NEG (not ?def) ?def)
      (rnogood! :ANTI (assumed-reaction ?name) ?def))

(rule :INTERN ((assumed-reaction ?name) :var ?def)
      (rnogood! :REDUNDANT ?def (UNIVERSAL)))

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

(assume! '(UNIVERSAL) :UNIVERSAL)
(assume! '(NO-GROWTH) :NO-GROWTH)

;; Example
(reaction R1 (A B) C G)
(reaction R2 (B C) D)
(reaction R3 (D G) E)
(reaction R4 (B F) E)
(growth E)

(run-rules)

(nutrients-sufficient-for-growth)
;((A B) (B C G) (D G) (B F) (E))

(consistent-with? '(NO-GROWTH) (environment-of '((UNIVERSAL) (NUTRIENT A))))
; T

(consistent-with? '(NO-GROWTH) (environment-of '((UNIVERSAL) (NUTRIENT A) (NUTRIENT B))))
; NIL

(consistent-with? 
 '(NO-GROWTH) 
 (environment-of '((ASSUMED-REACTION R1) (ASSUMED-REACTION R2) (ASSUMED-REACTION R3) (NUTRIENT A) (NUTRIENT B))))
; NIL

(consistent-with? 
 '(NO-GROWTH) 
 (environment-of '((ASSUMED-REACTION R1) (ASSUMED-REACTION R2) (NUTRIENT A) (NUTRIENT B))))
; T
