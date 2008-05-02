(rule :INTERN ((reaction ?reaction ?reactants . ?products) :var ?def)
      (dolist (reactant ?reactants)
	(assert! `(reactant ,reactant)
		 `(:REACTANT-OF-REACTION ,?def)))
      (dolist (product ?products)
	(assert! `(product ,product)
		 `(:PRODUCT-OF-REACTION ,?def))))

(rule :INTERN ((growth . ?compounds) :var ?def)
      (dolist (compound ?compounds)
	(assert! `(essential ,compound)
		 `(:ESSENTIAL-FOR-GROWTH ,?def))))

(rule :INTERN ((essential ?compound) :var ?def)
      (let ((reactant (fetch `(reactant ,?compound)))
	    (product (fetch `(product ,?compound))))
	(cond ((and reactant product)
	       (assert! `(present ,?compound)
			`(:PRESENT-IN ,@reactant ,@product)))
	      (reactant
	       (assert! `(present-reactant ,?compound)
			`(:PRESENT-IN ,@reactant)))
	      (product
	       (assert! `(present-product ,?compound)
			`(:PRESENT-IN ,@product)))
	      (t
	       (assert! `(absent ,?compound)
			'(:ABSENT))))))
