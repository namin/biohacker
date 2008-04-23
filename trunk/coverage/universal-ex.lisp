(create-coverage-problem :debugging t)

(assume! '(UNIVERSAL) :UNIVERSAL)
(assume! '(NO-GROWTH) :NO-GROWTH)

(reaction R1 (A B) C G)
(reaction R2 (B C) D)
(reaction R3 (D G) E)
(reaction R4 (B F) E)
(growth E)

(run-rules)

(nutrients-sufficient-for-growth)
;((A B) (B C G) (D G) (B F) (E))

(consistent-with? '(NO-GROWTH) (environment-of '((UNIVERSAL) (NUTRIENT A))))
; T

(consistent-with? '(NO-GROWTH) (environment-of '((UNIVERSAL) (NUTRIENT A) (NUTRIENT B) (NOT (DISABLED-REACTION R1)) (NOT (DISABLED-REACTION R2)) (NOT (DISABLED-REACTION R3)))))
; NIL

(consistent-with? 
 '(NO-GROWTH) 
 (environment-of '((ASSUMED-REACTION R1) (ASSUMED-REACTION R2) (ASSUMED-REACTION R3) (NUTRIENT A) (NUTRIENT B))))
; NIL

(consistent-with? 
 '(NO-GROWTH) 
 (environment-of '((ASSUMED-REACTION R1) (ASSUMED-REACTION R2) (NUTRIENT A) (NUTRIENT B))))
; T
