(rule ((:INTERN (enzyme ?enzyme . ?genes) :var ?def))
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
(rule ((:INTERN (reaction ?reaction ?reactants ?products ?reversible? ?enzymes) :var ?def))
      (dolist (?reactant ?reactants)
	(rassert! (compound ?reactant) :COMPOUND-OF-REACTION)
	(rassert! (reactant ?reactant ?reaction) :REACTANT-OF-REACTION))
      (dolist (?product ?products)
	(rassert! (compound ?product) :COMPOUND-OF-REACTION)
	(rassert! (product ?product ?reaction) :PRODUCT-OF-REACTION))
      (cond ((eq ?enzymes :SPONTANEOUS)
	     (rassert! (reaction-enabled ?reaction) :REACTION-SPONTANEOUS))
	    (t
	     (dolist (?enzyme ?enzymes)
	       (rassert! (:IMPLIES 
			  (enzyme-present ?enzyme)
			  (reaction-enabled ?reaction))
			 :REACTION-CATALYZED))))
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

