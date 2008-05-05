(rule ((:INTERN (nutrient ?x) :var ?def))
      (rassert! (:IMPLIES ?def (compound ?x))
		:NUTRIENT-IS-COMPOUND))

(rule ((:INTERN (reaction ?reaction ?reactants . ?products) :var ?def))
      (let ((inputs (mapcar #'(lambda (reactant)
				`(compound ,reactant))
			    ?reactants)))
	(dolist (?product ?products)
	  (assert! `(:IMPLIES (:AND ,?def ,@inputs) (compound ,?product))
		   ':PRODUCT-OF-REACTION))))

(rule ((:INTERN (experiment ?outcome ?nutrients . ?genes-off) :var ?def))
      (dolist (?nutrient ?nutrients)
	(rassert! (:IMPLIES ?def (nutrient ?nutrient))
		 :EXPERIMENT-SETUP))
      (rule ((:INTERN (gene ?gene)))
	    (unless (find ?gene ?genes-off)
	      (rassert! (:IMPLIES ?def (gene-on ?gene))
			:EXPERIMENT-SETUP))))

(rule ((:INTERN (reaction-enabled ?name) :var ?def))
      (rassert! (:IMPLIES (:AND (reaction-catalyzed ?name)
				(:NOT (reaction-disabled ?name)))
			  (reaction-enabled ?name))
		:REACTION-ENABLED))

(rule ((:TRUE no-reaction-disabled :var ?def)
       (:INTERN (reaction-enabled ?name)))
      (rassert! (:IMPLIES ?def (:NOT (reaction-disabled ?name)))))


