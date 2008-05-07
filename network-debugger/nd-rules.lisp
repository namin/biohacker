(rule ((:INTERN (enzyme ?enzyme . ?genes)))
      (dolist (?gene ?genes)
	(rassert! (gene ?gene) :NETWORK))
      (assert! `(:IMPLIES
		 (:AND 
		  ,@(mapcar
		     (lambda (?gene)
		       `(gene-on ,?gene))
		     ?genes))
		 (enzyme-present ,?enzyme))
	       :ENZYME-FORMATION))

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
			(:OR 
			 ,@(mapcar 
			    (lambda (?enzyme) `(enzyme-present ,?enzyme))
			    ?enzymes))
			(reaction-enabled ,?reaction))
		       :REACTION-CATALYZED)))
      (assert! `(:IMPLIES
		 (:AND (reaction-enabled ,?reaction)
		       ,@(mapcar 
			  (lambda (?reactant)
			    `(compound-present ,?reactant))
			  ?reactants))
		 (:AND (reaction-fired ,?reaction)
		       ,@(mapcar
			  (lambda (?product)
			    `(compound-present ,?product))
			  ?products)))
	       :REACTION-FIRED))

(rule ((:INTERN (nutrient ?nutrient) :var ?def))
      (rassert! (:IMPLIES ?def (compound-present ?nutrient))
		:NUTRIENT-PRESENT))

(defun compound-present-list (compounds)
  (mapcar 
   (lambda (?compound)
     `(compound-present ,?compound)) 
   compounds))

(rule ((:INTERN (experiment ?experiment ?growth? ?nutrients ?essential-compounds ?bootstrap-compounds ?toxins ?knock-ins ?knock-outs)))
      (assert! `(:IMPLIES 
		 (experiment-in-focus ,?experiment)
		 (:AND ,@(mapcar 
			  (lambda (?nutrient) 
			    `(nutrient ,?nutrient))
			  ?nutrients)
		       ,@(mapcar
			  (lambda (?gene)
			    `(gene-on ,?gene))
			  ?knock-ins)
		       ,@(mapcar
			  (lambda (?gene)
			    `(:NOT (gene-on ,?gene)))
			  ?knock-outs)))
	       :EXPERIMENT-SETUP)
      (let ((or-any-toxin-present
	     `(:OR ,@(compound-present-list ?toxins)))) 
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    ,or-any-toxin-present)
		  (:NOT growth))
		:EXPERIMENT-GROWTH-PREDICTION)
	(assert! `(:IMPLIES
		   (:AND
		    (experiment-in-focus ,?experiment)
		    (:NOT ,or-any-toxin-present)
		    ,@(compound-present-list ?essential-compounds))
		   growth)
		 :EXPERIMENT-NO-GROWTH-PREDICTION))
      (let ((and-all-bootstraps-present
	     `(:AND ,@(compound-present-list ?bootstrap-compounds))))
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
	       :EXPERIMENT-PREDICTION-WRONG-OUTCOME))

(rule ((:INTERN (experiment-in-focus ?e1) :var ?focus1)
       (:INTERN (experiment-in-focus ?e2) :var ?focus2))
      (when (form< ?focus1 ?focus2)
	(rassert! (:NOT (:AND ?focus1 ?focus2))
		  :FOCUS-UNIQUENESS)))