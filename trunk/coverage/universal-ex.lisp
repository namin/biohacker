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

(flip-outcome '(a b) nil)
;Outcome is growth.
; For no-growth, disable ONE reaction from EACH set:
; ((R1 R2 R3))

(direct-outcome '(a b) '(r3))
;NO-GROWTH

(flip-outcome '(a b) '(r3))
;Outcome is no-growth.
; For growth, enable EACH reaction from ONE set:
; ((R3))

(flip-outcome '(a b f) '(r3 r4))
;Outcome is no-growth.
; For growth, enable EACH reaction from ONE set:
; ((R3) (R4))
