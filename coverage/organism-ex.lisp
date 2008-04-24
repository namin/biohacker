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
;missing (enzyme E3 G3)
(enzyme E4 G4)

(catalyze R1 E1)
(catalyze R2 E2)
;missing (catalyze R3 E3)
(catalyze R4 E4)

(run-rules)

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

(why-reaction 'r4)
;  Each set of genes is sufficient for reaction R4: ((G4))