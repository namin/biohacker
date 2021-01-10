(setq
 *causal*
 (make-causal
  :title "riflemen"
  :graph
  '((U . C)
    (C . A)
    (C . B)
    (A . D)
    (B . D)
    (W . A))
  :priors
  '((U . 0.6)
    (W . 0.7))
  :symbolic-priors
  '((U . p)
    (W . q))
  :given 'D
  :intervention '(:NOT A)
  :outcome '(:NOT D)))

(causal-crank *causal*)
#|
Given probability: 0.88.
Outcome probability: 0.32.
|#

(setq *print-right-margin* 1000)
(symbolic-causal-crank *causal*)
#|
Given probability: (+ (* P Q) (* P (- 1 Q)) (* (- 1 P) Q)).
Outcome probability: (/ (* (- 1 P) Q) (+ (* P Q) (* P (- 1 Q)) (* (- 1 P) Q))).

The textbook answer for the outcome probability is:
q(1-p)
-----------------
1 - (1 - q)(1 - p)

Rewriting...
(1 - q)(1 - p) = 1 - q - p + pq
1-(1 - q)(1 - p) = pq + q + p

while here we have
pq + p(1 - q) + (1 - p)q =
pq + p - pq + q - pq =
pq + p + q

So, YES, it's the same.
Maybe we need a smarter simplifer. :P
|#


(why-nodes (causal-atms *causal*))
#|
<The contradiction,{}>
<D,{{U}{W}{B}{A}{C}{D}}>
<(NOT D),{{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<C,{{U}{(NOT W),A}{(NOT W),D}{B}{C}}>
<(NOT C),{{(NOT U)}{(NOT B)}{(NOT A)}{(NOT D)}{(NOT C)}}>
<A,{{U}{W}{B}{D}{C}{A}}>
<(NOT A),{{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT D)}{(NOT A)}}>
<B,{{U}{(NOT W),A}{(NOT W),D}{C}{B}}>
<(NOT B),{{(NOT U)}{(NOT A)}{(NOT D)}{(NOT C)}{(NOT B)}}>
<W,{{(NOT U),D}{(NOT U),A}{(NOT B),D}{(NOT C),D}{(NOT B),A}{(NOT C),A}{W}}>
<(NOT W),{{(NOT A)}{(NOT D)}{(NOT W)}}>
<U,{{(NOT W),A}{(NOT W),D}{B}{C}{U}}>
<(NOT U),{{(NOT B)}{(NOT A)}{(NOT D)}{(NOT C)}{(NOT U)}}>
<outcome,{{(NOT U),(NOT W)}{(NOT B),(NOT W)}{(NOT C),(NOT W)}{(NOT A)}{(NOT D)}}>
<(NOT outcome),{}>
<given,{{W}{A}{D}{U}{B}{C}}>
<(NOT given),{}>
|#

(why-nodes (causal-post-atms *causal*))
#|
<The contradiction,{}>
<C,{{U}{(NOT A),D}{B}{C}}>
<(NOT C),{{(NOT U)}{(NOT B)}{(NOT D)}{(NOT C)}}>
<D,{{U}{B}{C}{D}}>
<(NOT D),{{(NOT A),(NOT U)}{(NOT A),(NOT B)}{(NOT A),(NOT C)}{(NOT D)}}>
<B,{{U}{(NOT A),D}{C}{B}}>
<(NOT B),{{(NOT U)}{(NOT C)}{(NOT D)}{(NOT B)}}>
<A,{}>
<(NOT A),{{(NOT D)}{(NOT A)}}>
<U,{{(NOT A),D}{B}{C}{U}}>
<(NOT U),{{(NOT B)}{(NOT C)}{(NOT D)}{(NOT U)}}>
<outcome,{{(NOT A),(NOT U)}{(NOT A),(NOT B)}{(NOT A),(NOT C)}{(NOT D)}}>
<(NOT outcome),{}>
<given,{{U}{B}{C}{D}}>
<(NOT given),{}>
|#

;;  Query we would like is :given '(Xray Dys)
;;    :intervention '((:NOT Cancer) (:NOT TB))  ;; expected sufficiency of Bronchitis

(setq
 *medical-graph*
 '((TB . Dys)
   (TB . Xray)
   (Cancer . Dys)
   (Cancer . Xray)
   (Bronchitis . Dys)))

(setq
 *medical-priors*
 '((TB . 0.02)
   (Cancer . 0.5)
   (Bronchitis . 0.3)))

(setq
 *medical-symbolic-priors*
 '((TB . t)
   (Cancer . c)
   (Bronchitis . b)))

;;  Given an observed EHR, how many positive symptoms are expected to go away due to treating the disease?
(setq *expected-disablement*
      (make-causal
       :title "How would the symptoms change be if we knew that Bronchitis was not the disease"
       :graph *medical-graph*
       :priors *medical-priors*
       :symbolic-priors *medical-symbolic-priors*
       :given '(:AND Dys (:NOT Xray))  ;; Observed Symptoms  (S)  S+ = {Dys}
       :intervention '(:NOT Bronchitis)  ;; Do(Bronchitis = F)
       :outcome '(:AND Dys (:NOT Xray))))  ;; Counterfactual Symptoms (S')  S'+ = {Dys'  Nausea'}
;; expectation:  If Bronchitis is the disease then {Dys', Xray'}|{Dys = T, Dys' = F}|P( {Dys'=T, Xray'=F}  | do(Bronchitis = F), {Dys=T, Xray=F})
;;                                               < P( {Dys'=T, Xray'= F} | do(Bronchitis = T), {Dys=T, Xray=F})

(causal-crank *expected-disablement*)
#|
Given probability: 0.15.
Outcome probability: 0.00.
|#

(symbolic-causal-crank *expected-disablement*)
#|
Given probability: (* (- 1 T) (- 1 C) B).
Outcome probability: 0.00.
|#

;;  given an observed EHR, how many symptoms would remain after treating every disease but one?


(setq *expected-sufficiency*
      (make-causal
       :title "How would the symptoms change if we treated the patient for Cancer and TB?"
       :graph *medical-graph*
       :priors *medical-priors*
       :symbolic-priors *medical-symbolic-priors*
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
       :graph *medical-graph*
       :priors *medical-priors*
       :symbolic-priors *medical-symbolic-priors*
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
       :graph *medical-graph*
       :priors *medical-priors*
       :symbolic-priors *medical-symbolic-priors*
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
