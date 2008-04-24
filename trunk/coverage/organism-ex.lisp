(create-coverage-problem :debugging t)

(reaction R1 (A B) C G)
(reaction R2 (B C) D)
(reaction R3 (D G) E)
(reaction R4 (B F) E)
(growth E)

(organism the-bug)
(growth A E)

(enzyme E1 G1)
(enzyme E2 G2 G2p)
;(enzyme E3 G3)
(enzyme E4 G4)

(catalyze R1 E1)
(catalyze R2 E2)
;(catalyze R3 E3)
(catalyze R4 E4)

(run-rules)

(ensure-experiment-coherent 'growth '(a b) nil)
;Experiment is not coherent. Expected outcome GROWTH from experiment but calculated outcome NO-GROWTH.
; Enable EACH reaction from ONE set: 
; ((R3))




