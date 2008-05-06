(create-coverage-problem :debugging t)

(reaction R1 (A B) C G)
(reaction R2 (B C) D)
(reaction R3 (D G) E)
(reaction R4 (B F) E)
;(growth E)

(run-rules)

(organism the-bug)
(growth A E)

(enzyme E1 G1)
(enzyme E2 G2 G2p)
;missing (enzyme E3 G3)
(enzyme E4 G4)

(catalyze R1 E1)
(catalyze R2 E2)
;missing (catalyze R3 E3)
(catalyze R4 E4)

(run-rules)

;(nutrients-sufficient-for-growth)
;((A B F) (A E))

(experiment 
 growth 
 :nutrients (a b))
;Experiment is not coherent. Expected outcome GROWTH from experiment but calculated outcome NO-GROWTH.
; Enable EACH reaction from ONE set: 
; ((R3))

(experiment 
 no-growth 
 :nutrients (a b f) 
 :off (g1))
;Experiment is not coherent. Expected outcome NO-GROWTH from experiment but calculated outcome GROWTH.
; Disable ONE reaction from EACH set: 
; ((R4))

;(why-reaction 'r4)
;  Each set of genes is sufficient for reaction R4: ((G4))

;; these should be coherent
(experiment 
 no-growth 
 :nutrients (a b f) 
 :off (g1 g4))

(experiment 
 growth 
 :nutrients (a b f) 
 :off (g1))

(explain 'growth)
#|
  1 (experiment ...)             ()   Assumption
  2    (NUTRIENT A)             (1)  (:OR (NUTRIENT A) (:NOT (experiment ...)))
  3    (COMPOUND A)             (2)  (:OR (COMPOUND A) (:NOT (NUTRIENT A)))
  4    (NUTRIENT F)             (1)  (:OR (NUTRIENT F) (:NOT (experiment ...)))
  5    (COMPOUND F)             (4)  (:OR (COMPOUND F) (:NOT (NUTRIENT F)))
  6    (NUTRIENT B)             (1)  (:OR (NUTRIENT B) (:NOT (experiment ...)))
  7    (COMPOUND B)             (6)  (:OR (COMPOUND B) (:NOT (NUTRIENT B)))
  8    (GENE-ON G4)             (1)  (:OR (GENE-ON G4) (:NOT (experiment ...)))
  9     (ENZYME E4)             (8)  (:OR (ENZYME E4) (:NOT (GENE-ON G4)))
 10 (REACTION-ENABLED R4)             (9)  (:OR (REACTION-ENABLED R4) (:NOT (ENZYME E4)))
 11 (REACTION R4 (B F) E)            (10)  (:OR (:NOT (REACTION-ENABLED R4)) (REACTION R4 (B F) E))
 12    (COMPOUND E)        (5 7 11)  (:OR (:NOT (REACTION R4 (B F) E)) (:NOT (COMPOUND B)) (:NOT (COMPOUND F)) (COMPOUND E))
 13          GROWTH          (3 12)  (:OR (:NOT (COMPOUND E)) (:NOT (COMPOUND A)) GROWTH)
|#
