(network-debugger simple :debugging t :abducting t :log "~/src/lisp/biohacker/trunk/network-debugger/tests/simple-stats.log" )

;; Network
(reaction 
 r1 
 :reactants (a b) 
 :products (c g)
 :enzymes (e1)
 :reversible? nil)

(reaction 
 r2
 :reactants (b c)
 :products (d)
 :reversible? t)

(reaction 
 r3
 :reactants (d g)
 :products (e)
 :enzymes (e3))

(reaction 
 r4
 :reactants (b f)
 :products (e)
 :enzymes (e4))

(enzyme e1 g1)
(enzyme e3 g3 g3p)
(enzyme e4 g4)

;; Experiments

(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))

(experiment
 false-positive
 (a b f)
 :growth? nil
 :essential-compounds (a e)
 :knock-outs (g1))

(experiment
 positive
 (a b d)
 :growth? t
 :essential-compounds (a e))

(experiment
 negative
 (a)
 :growth? nil
 :essential-compounds (a e))

(summarize-findings)

#|
Network Debugger SIMPLE
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E3.
Adding enzyme E4.
Closing network for EXPERIMENT.
Adding experiment FALSE-NEGATIVE
Assuming simplify-investigations.
Focusing on experiment FALSE-NEGATIVE.
Experiment FALSE-NEGATIVE is not consistent with model. Needs:
( (NUTRIENT E) )
( (NUTRIENT D) )
( (REACTION-ENABLED R2) )
( (NUTRIENT F) )
Adding experiment FALSE-POSITIVE
Retracting focus on experiment FALSE-NEGATIVE.
Focusing on experiment FALSE-POSITIVE.
Experiment FALSE-POSITIVE is not consistent with model. Needs:
( (NOT (GENE-ON G4)) )
Adding experiment POSITIVE
Retracting focus on experiment FALSE-POSITIVE.
Focusing on experiment POSITIVE.
Experiment POSITIVE is consistent with model.
Adding experiment NEGATIVE
Retracting focus on experiment POSITIVE.
Focusing on experiment NEGATIVE.
Experiment NEGATIVE is consistent with model.
1 positive findings: (POSITIVE)
1 negative findings: (NEGATIVE)
1 false-negative findings: (FALSE-NEGATIVE)
1 false-positive findings: (FALSE-POSITIVE)
|#

#|
(explain 'growth)
  1 (FOCUS-EXPERIMENT POSITIVE)  ()   Assumption
  2    (NUTRIENT A)             (1)  (:OR (NUTRIENT A) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
  3 (COMPOUND-PRESENT A)        (2)  (:OR (COMPOUND-PRESENT A) (:NOT (NUTRIENT A)))
  4 SIMPLIFY-INVESTIGATIONS      ()   Assumption
  5    (GENE-ON G1)           (1 4)  (:OR (GENE-ON G1) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
  6 (ENZYME-PRESENT E1)         (5)  (:OR (:NOT (GENE-ON G1)) (ENZYME-PRESENT E1))
  7 (REACTION-ENABLED R1)       (6)  (:OR (:NOT (ENZYME-PRESENT E1)) (REACTION-ENABLED R1))
  8    (NUTRIENT B)             (1)  (:OR (NUTRIENT B) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
  9 (COMPOUND-PRESENT B)        (8)  (:OR (COMPOUND-PRESENT B) (:NOT (NUTRIENT B)))
 10 (COMPOUND-PRESENT G)    (3 7 9)  (:OR (:NOT (COMPOUND-PRESENT B)) (COMPOUND-PRESENT G) (:NOT (REACTION-ENABLED R1)) (:NOT (COMPOUND-PRESENT A)))
 11    (NUTRIENT D)             (1)  (:OR (NUTRIENT D) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
 12 (COMPOUND-PRESENT D)       (11)  (:OR (COMPOUND-PRESENT D) (:NOT (NUTRIENT D)))
 13   (GENE-ON G3P)           (1 4)  (:OR (GENE-ON G3P) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
 14    (GENE-ON G3)           (1 4)  (:OR (GENE-ON G3) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT POSITIVE)))
 15 (ENZYME-PRESENT E3)     (13 14)  (:OR (:NOT (GENE-ON G3)) (:NOT (GENE-ON G3P)) (ENZYME-PRESENT E3))
 16 (REACTION-ENABLED R3)      (15)  (:OR (:NOT (ENZYME-PRESENT E3)) (REACTION-ENABLED R3))
 17 (COMPOUND-PRESENT E) (10 12 16)  (:OR (COMPOUND-PRESENT E) (:NOT (REACTION-ENABLED R3)) (:NOT (COMPOUND-PRESENT D)) (:NOT (COMPOUND-PRESENT G)))
 18          GROWTH        (1 3 17)  (:OR (:NOT (COMPOUND-PRESENT E)) (:NOT (COMPOUND-PRESENT A)) GROWTH (:NOT (FOCUS-EXPERIMENT POSITIVE)))

  1 (FOCUS-EXPERIMENT NEGATIVE)      ()   Assumption
  2 SIMPLIFY-INVESTIGATIONS          ()   Assumption
  3 (:NOT (NUTRIENT E))           (1 2)  (:OR (:NOT (NUTRIENT E)) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT NEGATIVE)))
  4 (:NOT (NUTRIENT D))           (1 2)  (:OR (:NOT (NUTRIENT D)) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT NEGATIVE)))
  5 (:NOT EXPERIMENT-GROWTH)        (1)  (:OR (:NOT EXPERIMENT-GROWTH) (:NOT (FOCUS-EXPERIMENT NEGATIVE)))
  6 (:NOT (REACTION-ENABLED R2))    (5)  (:OR EXPERIMENT-GROWTH (:NOT (REACTION-ENABLED R2)))
  7 (:NOT (REACTION-FIRED R2))      (6)  (:OR (REACTION-ENABLED R2) (:NOT (REACTION-FIRED R2)))
  8 (:NOT (COMPOUND-PRESENT D))   (4 7)  (:OR (:NOT (COMPOUND-PRESENT D)) (REACTION-FIRED R2) (NUTRIENT D))
  9 (:NOT (REACTION-FIRED R3))      (8)  (:OR (COMPOUND-PRESENT D) (:NOT (REACTION-FIRED R3)))
 10 (:NOT (NUTRIENT B))           (1 2)  (:OR (:NOT (NUTRIENT B)) (:NOT SIMPLIFY-INVESTIGATIONS) (:NOT (FOCUS-EXPERIMENT NEGATIVE)))
 11 (:NOT (COMPOUND-PRESENT B))    (10)  (:OR (:NOT (COMPOUND-PRESENT B)) (NUTRIENT B))
 12 (:NOT (REACTION-FIRED R4))     (11)  (:OR (COMPOUND-PRESENT B) (:NOT (REACTION-FIRED R4)))
 13 (:NOT (COMPOUND-PRESENT E))(3 9 12)  (:OR (REACTION-FIRED R4) (:NOT (COMPOUND-PRESENT E)) (REACTION-FIRED R3) (NUTRIENT E))
 14   (:NOT GROWTH)              (1 13)  (:OR (COMPOUND-PRESENT E) (:NOT GROWTH) (:NOT (FOCUS-EXPERIMENT NEGATIVE)))
|#