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
      (rnogood! :ANTI (assumed-reaction ?name) ?def)
      (rnogood! :ANTI (assumed-reaction ?name) (not ?def)))

(rule :INTERN ((assumed-reaction ?name) :var ?def)
      (rnogood! :REDUNDANT ?def (UNIVERSAL)))

(rule :INTERN ((gene ?g))
      (rassume! (off ?g) :GENE-STATE)
      (rassume! (not (off ?g)) :GENE-STATE))

(rule :INTERN ((off ?g) :var ?def)
      (rnogood! :NEG (not ?def) ?def))

(rule :INTERN ((organism ?g1) :var ?def)
      (rnogood! :ORGANISM-XOR-UNIVERSAL (UNIVERSAL) ?def))

(rule :INTERN ((organism ?g1) :var ?def1
	       (organism ?g2) :var ?def2
	       :TEST (not (equal ?g1 ?g2)))
      (rnogood! :UNIQUE-ORGANISM ?def1 ?def2))

