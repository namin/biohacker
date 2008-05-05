;; run this after running organism-ex.lisp

;; NO-GROWTH EXPERIMENT
(experiment 
 no-growth 
 :nutrients (a b f) 
 :off (g1))

(true? 'growth)
;T ;; NOT COHERENT!

;; It's tricky to correct no-growth experiments
(retract! 'no-reaction-disabled :NO-REACTION-DISABLED)
(assume! 'cwa :CWA)
(run-rules)
(needs 'growth :FALSE '((reaction-disabled ?r)))
;(((REACTION-DISABLED R4) (REACTION-DISABLED R1))
; ((REACTION-DISABLED R4) (REACTION-DISABLED R2))
; ((REACTION-DISABLED R4) (REACTION-DISABLED R3)))

;; Checking if that works...
(assume! '(reaction-disabled R4) :USER)
(assume! '(reaction-disabled R1) :USER)
(known? 'growth)
;T
(false? 'growth)
;T
;; ... indeed
;; cleaning up
(retract! '(reaction-disabled R4) :USER)
(retract! '(reaction-disabled R1) :USER)
(retract! 'cwa :CWA)

;; GROWTH EXPERIMENT
(experiment 
 growth 
 :nutrients (a b))

(unknown? 'growth)
;T ;; NOT COHERENT -- because growth wasn't derived

;; It's much easier to correct growth experiments.
(needs 'growth :TRUE '((reaction-enabled ?r) (nutrient ?c)))
;(((NUTRIENT E)) 
; ((REACTION-ENABLED R3)) 
; ((NUTRIENT F)))
