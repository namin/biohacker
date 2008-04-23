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

(direct-outcome '(a) nil)
;NO-GROWTH

(direct-outcome '(a b) nil)
;GROWTH

(direct-outcome '(a b) '(r3))
;NO-GROWTH

