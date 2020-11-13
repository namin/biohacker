;;  Query we would like is :given '(Xray Dys)
;;    :intervention '((:NOT Cancer) (:NOT TB))  ;; expected sufficiency of Bronchitis

;;  Given an observed EHR, how many positive symptoms are expected to go away due to treating the disease?
(setq *expected-disablement*
      (make-causal
       :title "How would the symptoms change be if we knew that Bronchitis was not the disease"
       :graph
       '((TB . Dys)
         (TB . Xray)
	 (Cancer . Dys)
	 (Cancer . Xray)
	 (Bronchitis . Dys))
       :priors
       '((TB . 0.02)
	 (Cancer . 0.5)
	 (Bronchitis . 0.3))
       :symbolic-priors
       '((TB . t)
	 (Cancer . c)
	 (Bronchitis . b))
       :given '(:AND Dys (:NOT Xray))  ;; Observed Symptoms  (S)  S+ = {Dys}
       :intervention '(:NOT Bronchitis)  ;; Do(Bronchitis = F)
       :outcome '(:AND Dys (:NOT Xray))))  ;; Counterfactual Symptoms (S')  S'+ = {Dys'  Nausea'}
;; expectation:  If Bronchitis is the disease then {Dys', Xray'}|{Dys = T, Dys' = F}|P( {Dys'=T, Xray'=F}  | do(Bronchitis = F), {Dys=T, Xray=F})
;;                                               < P( {Dys'=T, Xray'= F} | do(Bronchitis = T), {Dys=T, Xray=F})

;; TODO: is the null outcome a bug?

(causal-crank *expected-disablement*)
#|
Given probability: 0.15.
Outcome probability: 0.00.
|#

(symbolic-causal-crank *expected-disablement*)
#|
Given probability: (* (- 1 T) (- 1 C) B).
Outcome probability: NIL.
|#

;;  given an observed EHR, how many symptoms would remain after treating every disease but one?


(setq *expected-sufficiency*
      (make-causal
       :title "How would the symptoms change if we treated the patient for Cancer and TB?"
       :graph
       '((TB . Dys)
         (TB . Xray)
	 (Cancer . Dys)
	 (Cancer . Xray)
	 (Bronchitis . Dys))
       :priors
       '((TB . 0.02)
	 (Cancer . 0.5)
	 (Bronchitis . 0.3))
       :symbolic-priors
       '((TB . t)
	 (Cancer . c)
	 (Bronchitis . b))
       :given '(:AND Dys (:NOT Xray))   ;; S
       :intervention '(:AND (:NOT TB) (:NOT Cancer))   ;; do(treat TB and Cancer)
       :outcome '(:AND Dys (:NOT Xray))))  ;; S'

(causal-crank *expected-sufficiency*)
#|
Given probability: 0.15.
Outcome probability: 1.00.
|#

(symbolic-causal-crank *expected-sufficiency*)
#|
Given probability: (* (- 1 T) (- 1 C) B).
Outcome probability: 1.00
|#

(setq *necessary-causation*
      (make-causal
       :title "P({No Dys}_{treated Bronchitis} | Bronchitis, Dys)"
       :graph
       '((TB . Dys)
         (TB . Xray)
	 (Cancer . Dys)
	 (Cancer . Xray)
	 (Bronchitis . Dys))
       :priors
       '((TB . 0.02)
	 (Cancer . 0.5)
	 (Bronchitis . 0.3))
       :symbolic-priors
       '((TB . t)
	 (Cancer . c)
	 (Bronchitis . b))
       :given '(:AND Dys Bronchitis)
       :intervention '(:NOT Bronchitis)
       :outcome '(:NOT Dys)))

(causal-crank *necessary-causation*)
#|
Given probability: 0.30.
Outcome probability: 0.49.
|#

(symbolic-causal-crank *necessary-causation*)
#|
Given probability: (+ (* T C B) (* T (- 1 C) B) (* (- 1 T) C B) (* (- 1 T) (- 1 C) B)).
Outcome probability: (/ (* (- 1 T) (- 1 C) B) (+ (* T C B) (* T (- 1 C) B) (* (- 1 T) C B) (* (- 1 T) (- 1 C) B))).
|#

 (setq *sufficient-causation*
      (make-causal
       :title "P({Dys}_{infect with Bronchitis} | no_Bronchitis, no_Dys) %% P(Bronchtis) = .30 %% P(Dys | Bronchitis) = ? %% P(Bronchitis | Dys)"
       :graph
       '((TB . Dys)
         (TB . Xray)
	 (Cancer . Dys)
	 (Cancer . Xray)
	 (Bronchitis . Dys))
       :priors
       '((TB . 0.02)
	 (Cancer . 0.5)
	 (Bronchitis . 0.3))
       :symbolic-priors
       '((TB . t)
	 (Cancer . c)
	 (Bronchitis . b))
       :given '(:AND (:NOT Dys) (:NOT Bronchitis))
       :intervention 'Bronchitis
       :outcome 'Dys))

(causal-crank *sufficient-causation*)
#|
Given probability: 0.34.
Outcome probability: 1.00.
|#

(symbolic-causal-crank *sufficient-causation*)
#|
Given probability: (* (- 1 T) (- 1 C) (- 1 B)).
Outcome probability: 1.00
|#
