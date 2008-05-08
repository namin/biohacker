(rule ((:INTERN (enzyme ?enzyme . ?genes)))
      (dolist (?gene ?genes)
	(rassert! (gene ?gene) :NETWORK))
      (assert! `(:IMPLIES
		 (:AND ,@(list-of 'gene-on ?genes))
		 (enzyme-present ,?enzyme))
	       :ENZYME-FORMED)
      (assert! `(:IMPLIES 
		 (:NOT (:AND ,@(list-of 'gene-on ?genes)))
		 (:NOT (enzyme-present ,?enzyme)))
	       :ENZYME-NOT-FORMED))

(rule ((:INTERN (pathway ?pathway ?reactants ?products ?reversible? ?enzymes ?reactions)))
      (dolist (?reactant ?reactants)
	(rassert! (compound ?reactant) :COMPOUND-OF-PATHWAY)
	(rassert! (pathway-reactant ?reactant ?pathway) :REACTANT-OF-PATHWAY))
      (dolist (?product ?products)
	(rassert! (compound ?product) :COMPOUND-OF-PATHWAY)
	(rassert! (pathway-product ?product ?pathway) :PRODUCT-OF-PATHWAY))
      (cond ((eq ?enzymes :SPONTANEOUS)
	     (rassert! (pathway-enabled ?pathway) :PATHWAY-SPONTANEOUS))
	    (t
	     (assert! `(:IMPLIES 
			(:AND ,@(list-of 'enzyme-present ?enzymes))
			(pathway-enabled ,?pathway))
		       :PATHWAY-CATALYZED)
	     (assert! `(:IMPLIES 
			(:AND
			 (:NOT (:AND ,@(list-of 'enzyme-present ?enzymes)))
			 (:NOT experiment-growth) ; keep uncatalyzed pathways open for growth experiment abduction
			 )
			(:NOT (pathway-enabled ,?pathway)))
		       :PATHWAY-NOT-CATALYZED)))
      (assert! `(:IMPLIES
		 (:AND (pathway-enabled ,?pathway)
		       ,@(list-of 'compound-present ?reactants))
		 (:AND (pathway-fired ,?pathway)
		       ,@(list-of 'compound-present ?products)))
	       :PATHWAY-FIRED)
      (assert! `(:IMPLIES
		 (:NOT (:AND ,@(list-of 'compound-present ?reactants)))
		 (:NOT (pathway-fired ,?pathway)))
	       :PATHWAY-NOT-FIRED)
      (rassert! (:IMPLIES
		 (:NOT (pathway-enabled ?pathway))
		 (:NOT (pathway-fired ?pathway)))
		:INCLUSION))

(rule ((:INTERN (nutrient ?nutrient) :var ?def))
      (rassert! (:IMPLIES ?def (compound-present ?nutrient))
		:NUTRIENT-PRESENT))

(rule ((:INTERN (experiment ?experiment ?growth? ?nutrients ?essential-compounds ?bootstrap-compounds ?toxins ?knock-ins ?knock-outs)))
      (assert! `(:IMPLIES 
		 (focus-experiment ,?experiment)
		 (:AND ,@(list-of 'nutrient ?nutrients)
		       ,@(list-of 'gene-on ?knock-ins)
		       (:NOT (:OR ,@(list-of 'gene-on ?knock-outs)))
		       ,(ecase ?growth?
			  ((t) 'experiment-growth)
			  ((nil) '(:NOT experiment-growth)))))
	       :EXPERIMENT-SETUP)
      (let ((or-any-toxin-present
	     `(:OR ,@(list-of 'compound-present ?toxins)))) 
	(assert! `(:IMPLIES
		   (:AND
		    (focus-experiment ,?experiment)
		    ,or-any-toxin-present)
		  (:NOT growth))
		:EXPERIMENT-TOXIC-PREDICTION)
	(assert! `(:IMPLIES
		   (:AND
		    (focus-experiment ,?experiment)
		    (:NOT ,or-any-toxin-present)
		    ,@(list-of 'compound-present ?essential-compounds))
		   growth)
		 :EXPERIMENT-GROWTH-PREDICTION))
      	(assert! `(:IMPLIES
		   (:AND
		    (focus-experiment ,?experiment)
		    (:NOT (:AND ,@(list-of 'compound-present ?essential-compounds))))
		   (:NOT growth))
		 :EXPERIMENT-NO-GROWTH-PREDICTION)
      (let ((and-all-bootstraps-present
	     `(:AND ,@(list-of 'compound-present ?bootstrap-compounds))))
	(assert! `(:IMPLIES
		   (:AND
		    (focus-experiment ,?experiment)
		    (:NOT ,and-all-bootstraps-present))
		   (:NOT experiment-coherent))
		 :EXPERIMENT-PREDICTION-MISSING-BOOTSTRAP-COMPOUND)
	(assert! `(:IMPLIES
		   (:AND
		    (focus-experiment ,?experiment)
		    ,and-all-bootstraps-present
		    ,(ecase ?growth?
		       ((t) 'growth)
		       ((nil) '(:NOT growth))))
		   experiment-coherent)
		 :EXPERIMENT-PREDICTION-OK))
      (assert! `(:IMPLIES
		 (:AND
		  (focus-experiment ,?experiment)
		  ,(ecase ?growth?
		     ((nil) 'growth)
		     ((t) '(:NOT growth))))
		 (:NOT experiment-coherent))
	       :EXPERIMENT-PREDICTION-WRONG-OUTCOME)
      (when (not ?growth?)
	(rule ((:INTERN (compound ?compound)))
	      (unless (find ?compound ?nutrients) 
		(rassert! (:IMPLIES
			   (:AND (focus-experiment ?experiment)
				 simplify-investigations)
			   (:NOT (nutrient ?compound)))
			  :NO-GROWTH-EXPERIMENT-SIMPLIFICATION))))
      (when ?growth?
	(rule ((:INTERN (gene ?gene)))
	      (unless (or (find ?gene ?knock-outs) (find ?gene ?knock-ins)) 
		(rassert! (:IMPLIES 
			   (:AND (focus-experiment ?experiment)
				 simplify-investigations)
			   (gene-on ?gene))
			  :GROWTH-EXPERIMENT-SIMPLIFICATION)))))

(rule ((:INTERN (focus-experiment ?e1) :var ?focus1)
       (:INTERN (focus-experiment ?e2) :var ?focus2))
      (when (form< ?focus1 ?focus2)
	(rassert! (:NOT (:AND ?focus1 ?focus2))
		  :FOCUS-UNIQUENESS)))

(rule ((:TRUE network-closed))
      (rule ((:INTERN (compound ?compound)))
	    (let* ((product-facts (fetch `(pathway-product ,?compound ?pathway)))
		   (pathway-fired-facts (mapcar #'(lambda (fact) `(pathway-fired ,(caddr fact))) product-facts)))
	      (assert! `(:IMPLIES 
			 (:AND 
			  (:NOT (nutrient ,?compound))
			  (:NOT (:OR ,@pathway-fired-facts)))
			 (:NOT (compound-present ,?compound)))
		       :NETWORK-CLOSED))))

