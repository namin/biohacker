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

(rule ((:INTERN (reaction ?reaction ?reactants . ?products) :var ?def))
      (dolist (?reactant ?reactants)
	(rassert! (mentioned-compound ?reactant) :MENTIONED-COMPOUND)
	(rassert! (:IMPLIES ?def (reactant ?reactant ?reaction))
		  :REACTANT-OF-REACTION))
      (dolist (?product ?products)
	(rassert! (mentioned-compound ?product) :MENTIONED-COMPOUND)
	(rassert! (:IMPLIES ?def (product ?product ?reaction))
		  :PRODUCT-OF-REACTION)))

(rule ((:TRUE (sufficient-for-enzyme ?enzyme . ?conditions) :var ?def))
      (assert! `(:IMPLIES (:AND ,@?conditions)
			  (enzyme ,?enzyme))
	       ':ENZYME))

(rule ((:TRUE (sufficient-for-reaction ?reaction . ?conditions) :var ?def))
      (assert! `(:IMPLIES (:AND ,@?conditions)
			  (reaction-enabled ,?reaction))
	       :CATALYZE))

(rule ((:TRUE (experiment ?outcome ?nutrients . ?genes-off) :var ?def))
      (dolist (?nutrient ?nutrients)
	(rassert! (:IMPLIES ?def (nutrient ?nutrient))
		 :EXPERIMENT-SETUP))
      (ecase ?outcome
	('no-growth
	 (rule ((:INTERN (mentioned-compound ?compound)))
	       (unless (find ?compound ?nutrients)
		 (rassert! (:IMPLIES ?def (:NOT (nutrient ?compound)))
			   :NO-GROWTH-EXPERIMENT-SETUP)))
	 (rule ((:INTERN (gene-on ?gene) :var ?gene-on-def))
	       (if (find ?gene ?genes-off)
		   (rassert! (:IMPLIES ?def 
				       (:NOT (gene-on ?gene)))
			     :NO-GROWTH-EXPERIMENT-SETUP)
		 (rassert! (:IMPLIES (:AND ?def 
					   (:NOT investigating-experiment)) 
				     ?gene-on-def)
			   :NO-GROWTH-EXPERIMENT-SETUP))))
	('growth
	 (rule ((:INTERN (gene-on ?gene) :var ?gene-on-def))
	       (if (find ?gene ?genes-off)
		   (rassert! (:IMPLIES (:AND ?def (:NOT investigating-experiment)) 
				       (:NOT (gene-on ?gene)))
			     :GROWTH-EXPERIMENT-SETUP)
		 (rassert! (:IMPLIES ?def 
				     ?gene-on-def)
			   :GROWTH-EXPERIMENT-SETUP))))))

(rule ((:TRUE (sufficient-for-growth . ?conditions) :var ?def))
      (assert! `(:IMPLIES (:AND ,@?conditions) ; don't include def to allow needs for growth
			  growth)
	       ':GROWTH))

(rule ((:TRUE cwa :var ?def))
      (let* ((sufficient-for-growth-forms (fetch `(sufficient-for-growth . ?conditions)))
	     (condition-set-forms (mapcar #'(lambda (form) `(:AND ,@(cdr form))) sufficient-for-growth-forms)))
	(assert! `(:IMPLIES (:AND ,?def
				  (:NOT (:AND ,@condition-set-forms)))
			    (:NOT growth))
		 :CWA))
      (rule ((:INTERN (enzyme ?enzyme) :var ?enzyme-def))      
	    (let* ((sufficient-for-enzyme-forms (fetch `(sufficient-for-enzyme ,?enzyme . ?conditions)))
		   (condition-set-forms (mapcar #'(lambda (form) `(:AND ,@(cddr form))) sufficient-for-enzyme-forms)))
	      (assert! `(:IMPLIES (:AND ,?def
					(:NOT (:AND ,@condition-set-forms)))
				  (:NOT ,?enzyme-def))
		       :CWA)))
      (rule ((:INTERN (reaction-enabled ?reaction) :var ?reaction-enabled-def))
	    (let* ((sufficient-for-reaction-forms (fetch `(sufficient-for-reaction ,?reaction . ?conditions)))
		   (condition-set-forms (mapcar #'(lambda (form) `(:AND ,@(cddr form))) sufficient-for-reaction-forms)))
	      (assert! `(:IMPLIES (:AND ,?def
					(:NOT (:AND ,@condition-set-forms)))
				  (:NOT ,?reaction-enabled-def))
		       :CWA))
	    (rassert! (:IMPLIES (:AND ?def 
				      (:NOT ?reaction-enabled-def))
				(:NOT (reaction-active ?reaction)))
		      :CWA)
	    (let* ((reactant-forms (fetch `(reactant ?reactant ,?reaction)))
		   (compound-forms (mapcar #'(lambda (form) `(compound ,(cadr form))) reactant-forms)))
	      (assert! `(:IMPLIES (:AND ,?def
					,?reaction-enabled-def 
					,@compound-forms)
				  (reaction-active ,?reaction))
		       :CWA)
	      (assert! `(:IMPLIES (:AND ,?def 
					(:NOT (:AND ,@compound-forms)))
				  (:NOT (reaction-active ,?reaction)))
		       :CWA)))
      (rule ((:INTERN (mentioned-compound ?compound)))
	    (let* ((product-forms (fetch `(product ,?compound ?reaction)))
		   (reaction-forms (mapcar #'(lambda (form) `(reaction-active ,(caddr form))) product-forms)))
	      (assert! `(:IMPLIES (:AND ,?def
					(:NOT (nutrient ,?compound))
					(:NOT (:OR ,@reaction-forms)))
				  (:NOT (compound ,?compound)))
		       :CWA))))

