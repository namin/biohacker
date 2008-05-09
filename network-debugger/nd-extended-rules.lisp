(rule ((:INTERN (enzyme ?enzyme . ?genes)))
      (let* ((actual-genes (remove :UNKNOWN ?genes))
	     (unknown? (find :UNKNOWN ?genes))
	     (and-genes-on
	      `(:AND 
	       ,@(list-of 'gene-on actual-genes)
	       (:OR (:NOT (unknown-gene-for ,?enzyme)) (unknown-gene-on-for ,?enzyme)))))
	(dolist (?gene actual-genes)
	  (rassert! (gene ?gene) :GENE-OF-ENZYME))
	(if unknown?
	    (progn
	      (rassert! (unknown-gene-for ?enzyme) :NETWORK-EXTENSION)
	      (rassert!
	       (:IMPLIES
		 (:AND 
		  assume-unknowns-as-convenient
		  experiment-growth)
		 (unknown-gene-on-for ?enzyme))
	       :GROWTH-EXPERIMENT-CONVENIENT-ASSUMPTION)
	      (rassert!
	       (:IMPLIES
		 (:AND 
		  assume-unknowns-as-convenient
		  (:NOT experiment-growth))
		 (:NOT (unknown-gene-on-for ?enzyme)))
	       :NO-GROWTH-EXPERIMENT-CONVENIENT-ASSUMPTION))
	  (rassert! (:NOT (unknown-gene-for ?enzyme)) :NETWORK-EXTENSION))
	(assert! `(:IMPLIES
		   ,and-genes-on
		   (enzyme-present ,?enzyme))
		 :ENZYME-FORMED)
	(assert! `(:IMPLIES 
		   (:NOT ,and-genes-on)
		   (:NOT (enzyme-present ,?enzyme)))
		 :ENZYME-NOT-FORMED)))

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
			(:AND
			 (:NOT (:OR ,@(list-of 'enzyme-present ?enzymes)))
			 (:NOT experiment-growth) ; keep uncatalyzed reactions open for growth experiment abduction
			 )
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
      (assert! `(:IMPLIES
		 (:AND (reaction-reversible ,?reaction) 
		       (reaction-enabled ,?reaction)
		       ,@(list-of 'compound-present ?products))
		 (:AND (reverse-reaction-fired ,?reaction)
		       ,@(list-of 'compound-present ?reactants)))
	       :REVERSE-REACTION-FIRED)
      (assert! `(:IMPLIES
		 (:OR (:NOT (:AND ,@(list-of 'compound-present ?products)))
		      (:NOT (reaction-reversible ,?reaction)))
		 (:NOT (reverse-reaction-fired ,?reaction)))
	       :REVERSE-REACTION-NOT-FIRED)
      (rassert! (:IMPLIES
		 (:NOT (reaction-enabled ?reaction))
		 (:AND (:NOT (reaction-fired ?reaction))
		       (:NOT (reverse-reaction-fired ?reaction))))
		:INCLUSION)
      (rassert! (:IMPLIES
		 (:NOT (reaction-reversible ?reaction))
		 (:NOT (reverse-reaction-fired ?reaction)))
		:INCLUSION)
      (ecase ?reversible?
	((t) 
	 (rassert! (reaction-reversible ?reaction) :NETWORK-EXTENSION))
	((nil) 
	 (rassert! (:NOT (reaction-reversible ?reaction))) :NETWORK-EXTENSION)
	((:UNKNOWN)
	 (rassert!
	  (:IMPLIES
	   (:AND
	    assume-unknowns-as-convenient
	    (:NOT experiment-growth))
	   (:NOT (reaction-reversible ?reaction)))
	  :NO-GROWTH-EXPERIMENT-CONVENIENT-ASSUMPTION)
	 (rassert!
	  (:IMPLIES
	   (:AND
	    assume-unknowns-as-convenient
	    experiment-growth)
	   (reaction-reversible ?reaction))
	  :GROWTH-EXPERIMENT-CONVENIENT-ASSUMPTION))))

(rule ((:INTERN (nutrient ?nutrient) :var ?def))
      (rassert! (:IMPLIES ?def (compound-present ?nutrient))
		:NUTRIENT-PRESENT))

(rule ((:INTERN (experiment ?experiment ?growth? ?nutrients ?essential-compounds ?bootstrap-compounds ?toxins ?knock-ins ?knock-outs)))
      (dolist (?compound (append ?essential-compounds ?bootstrap-compounds ?toxins))
	(rassert! (compound ?compound)
		  :COMPOUND-OF-EXPERIMENT))
      (dolist (?gene (append ?knock-ins ?knock-outs))
	(rassert! (gene ?gene) :GENE-OF-EXPERIMENT))
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
	    (let* ((product-facts (fetch `(product ,?compound ?reaction)))
		   (reactant-facts (fetch `(reactant ,?compound ?reaction)))
		   (reaction-fired-facts (mapcar #'(lambda (fact) `(reaction-fired ,(caddr fact))) product-facts))
		   (reverse-reaction-fired-facts (mapcar #'(lambda (fact) `(reverse-reaction-fired ,(caddr fact))) reactant-facts)))
	      (assert! `(:IMPLIES 
			 (:AND 
			  (:NOT (nutrient ,?compound))
			  (:NOT (:OR ,@reaction-fired-facts ,@reverse-reaction-fired-facts)))
			 (:NOT (compound-present ,?compound)))
		       :NETWORK-CLOSED))))

