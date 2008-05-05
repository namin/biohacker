;; run this after running organism-ex.lisp

;; NO-GROWTH EXPERIMENT
(experiment 
 no-growth 
 :nutrients (a b f) 
 :off (g1))

(true? 'growth)
;T ;; NOT COHERENT!

;; It's tricky to correct no-growth experiments
(start-investigating-experiment)
(setq correction-no-growth 
      (sort-fact-sets
       (needs 'growth :FALSE '((:NOT (gene-on ?g))))))
;(((:NOT (GENE-ON G4))))
;; Checking if that works...
(assume! '(:NOT (GENE-ON G4)) :USER)
(known? 'growth)
;T
(false? 'growth)
;T
;; ... indeed
;; ... cleaning up
(retract! '(:NOT (GENE-ON G4)) :USER)
(stop-investigating-experiment)

;; GROWTH EXPERIMENT
(experiment 
 growth 
 :nutrients (a b))

(unknown? 'growth)
;T ;; NOT COHERENT -- because growth wasn't derived

;; It's much easier to correct growth experiments.
(start-investigating-experiment)
(setq correction-growth
      (sort-fact-sets
       (needs 'growth :TRUE '((reaction-enabled ?r) (nutrient ?c)))))
; (((NUTRIENT E)) 
;  ((NUTRIENT F)) 
;  ((REACTION-ENABLED R3)))
(stop-investigating-experiment)
