(create-coverage-problem :debugging t)

;; Over all Organisms

;; Universal Beliefs

; A B -R1-> C G
; B C -R2-> D
; D G -R3-> E
; B F -R4-> E
(reaction R1 (A B) C G)
(reaction R2 (B C) D)
(reaction R3 (D G) E)
(reaction R4 (B F) E)

;; Per Organism
(organism the-bug)

;; Organism's Assumptions

(enzyme E1 G1)
(enzyme E2 G2 G2p)
;missing (enzyme E3 G3)
(enzyme E4 G4)

(catalyze R1 E1)
(catalyze R2 E2)
;missing (catalyze R3 E3)
(catalyze R4 E4)

;; Organism's Beliefs
(growth A E)

(run-rules)

(experiment 
 growth
 :nutrients (A B)
 :off (G4))

;; contradiction with experiment:
;; no growth because no essential compound E
;; no essential compound E because missing catalyst for reaction R3
;; suggestion: add (catalyze R3 Ex), add (enzyme Ex Gx)

(experiment
 no-growth
 :nutrients (A B F)
 :off (G1))

;; contradiction with experiment:
;; growth because essential compounds A & E
;; (essential compound A because it's a nutrient)
;; essential compound E via reaction R4 (reactants via nutrients B & F)
;; reaction R4 via enzyme E4
;; enzyme E4 via gene G4
;; suggestion: retract (enzyme E4 G4) which justifies (catalyze R4 E4) which justifies E formation