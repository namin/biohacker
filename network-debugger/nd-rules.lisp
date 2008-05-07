(defun list-of (name els)
  (mapcar
   #'(lambda (el)
     (list name el))
   els))

(rule ((:INTERN (enzyme ?enzyme . ?genes)))
      (dolist (?gene ?genes)
	(rassert! (gene ?gene) :NETWORK))
      (assert! `(:IMPLIES
		 (:AND ,@(list-of 'gene-on ?genes))
		 (enzyme-present ,?enzyme))
	       :ENZYME-FORMED)
      (assert! `(:IMPLIES 
		 (:NOT (:OR ,@(list-of 'gene-on ?genes)))
		 (:NOT (enzyme-present ,?enzyme)))
	       :ENZYME-NOT-FORMED))

;; ignoring reversible? for now
;; ignoring unknown enzyme for now
(rule ((:INTERN (reaction ?reaction ?reactants ?products ?reversible? ?enzymes)))
      (dolist (?reactant ?reactants)
	(rassert! (compound ?reactant) :COMPOUND-OF-REACTION)
	(rassert! (reactant ?reactant ?reaction) :REACTANT-OF-REACTION))
      (dolist (?product ?products)
	(rassert! (compound ?product) :COMPOUND-OF-REACTION)
	(rassert! (product ?product ?reaction) :PRODUCT-OF-REACTION))
      (cond ((eq ?enzymes :SPONTANEOUS)
	     (rassert! (reaction-enabled ?reaction) :REACTION-SPONTANEOUS))
	    (t
	     (assert! `(:IMPLIES 
			(:OR ,@(list-of 'enzyme-present ?enzymes))
			(reaction-enabled ,?reaction))
		       :REACTION-CATALYZED)
	     (assert! `(:IMPLIES 
			(:NOT (:OR ,@(list-of 'enzyme-present ?enzymes)))
			(:NOT (reaction-enabled ,?reaction)))
		       :REACTION-NOT-CATALYZED)))
      (assert! `(:IMPLIES
		 (:AND (reaction-enabled ,?reaction)
		       ,@(list-of 'compound-present ?reactants))
		 (:AND (reaction-fired ,?reaction)
		       ,@(list-of 'compound-present ?products)))
	       :REACTION-FIRED)
      (assert! `(:IMPLIES
		 (:NOT (:AND ,@(list-of 'compound-present ?reactants)))
		 (:NOT (reaction-fired ,?reaction)))
	       :REACTION-NOT-FIRED)
      (rassert! (:IMPLIES
		 (:NOT (reaction-enabled ?reaction))
		 (:NOT (reaction-fired ?reaction)))
		:INCLUSION))

(rule ((:INTERN (nutrient ?nutrient) :var ?def))
      (rassert! (:IMPLIES ?def (compound-present ?nutrient))
		:NUTRIENT-PRESENT))

(rule ((:INTERN (experiment ?experiment ?growth? ?nutrients ?essential-compounds ?bootstrap-compounds ?toxins ?knock-ins ?knock-outs)))
      (assert! `(:IMPLIES 
		 (experiment-in-focus ,?experiment)
		 (:AND ,@(list-of 'nutrient ?nutrients)
		       ,@(list-of 'gene-on ?knock-ins)
		       (:NOT (:OR ,@(list-of 'gene-on ?knock-outs)))))
	       :EXPERIMENT-SETUP)
      (let ((or-any-toxin-present
	     `(:OR ,@(list-of 'compound-present ?toxins)))) 
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    ,or-any-toxin-present)
		  (:NOT growth))
		:EXPERIMENT-TOXIC-PREDICTION)
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    (:NOT ,or-any-toxin-present)
		    ,@(list-of 'compound-present ?essential-compounds))
		   growth)
		 :EXPERIMENT-GROWTH-PREDICTION))
      	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    (:NOT (:AND ,@(list-of 'compound-present ?essential-compounds))))
		   (:NOT growth))
		 :EXPERIMENT-NO-GROWTH-PREDICTION)
      (let ((and-all-bootstraps-present
	     `(:AND ,@(list-of 'compound-present ?bootstrap-compounds))))
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    (:NOT ,and-all-bootstraps-present))
		   (:NOT experiment-coherent))
		 :EXPERIMENT-PREDICTION-MISSING-BOOTSTRAP-COMPOUND)
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    ,and-all-bootstraps-present
		    ,(ecase ?growth?
		       ((t) 'growth)
		       ((nil) '(:NOT growth))))
		   experiment-coherent)
		 :EXPERIMENT-PREDICTION-OK))
      (assert! `(:IMPLIES
		 (:AND
		  (experiment-in-focus ,?experiment)
		  ,(ecase ?growth?
		     ((nil) 'growth)
		     ((t) '(:NOT growth))))
		 (:NOT experiment-coherent))
	       :EXPERIMENT-PREDICTION-WRONG-OUTCOME)
      (when (not ?growth?)
	(rule ((:INTERN (compound ?compound)))
	      (unless (find ?compound ?nutrients) 
		(rassert! (:IMPLIES
			   (:AND (experiment-in-focus ?experiment)
				 simplify-needs)
			   (:NOT (nutrient ?compound)))
			  :NO-GROWTH-EXPERIMENT-SIMPLIFICATION))))
      (when ?growth?
	(rule ((:INTERN (gene ?gene)))
	      (unless (or (find ?gene ?knock-outs) (find ?gene ?knock-ins)) 
		(rassert! (:IMPLIES 
			   (:AND (experiment-in-focus ?experiment)
				 simplify-needs)
			   (gene-on ?gene))
			  :GROWTH-EXPERIMENT-SIMPLIFICATION)))))

(rule ((:INTERN (experiment-in-focus ?e1) :var ?focus1)
       (:INTERN (experiment-in-focus ?e2) :var ?focus2))
      (when (form< ?focus1 ?focus2)
	(rassert! (:NOT (:AND ?focus1 ?focus2))
		  :FOCUS-UNIQUENESS)))

(rule ((:TRUE network-closed))
      (rule ((:INTERN (compound ?compound)))
	    (let* ((product-forms (fetch `(product ,?compound ?reaction)))
		   (reaction-forms (mapcar #'(lambda (form) `(reaction-fired ,(caddr form))) product-forms)))
	      (assert! `(:IMPLIES 
			 (:AND 
			  (:NOT (nutrient ,?compound))
			  (:NOT (:OR ,@reaction-forms)))
			 (:NOT (compound-present ,?compound)))
		       :NETWORK-CLOSED))))

