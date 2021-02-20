;; The Problem of which this is a subproblem
;; Find the distinguishable events (outcome space)
;; These distinguishable events are logically connected.  We need to count the possible worlds where things are true.
;; The LTMS gives us the ability to infer logical facts and the ATMS gives us the ability to count facts.  We augment an ATMS to be an LTMS by embedding an LTMS inside an ATMS. This enables us to perform logical inference and count worlds.  In order to perform counterfactual reasoning, we connect two LATMS's to each other, one for the indiciative world, and the other for the subjunctive world.
;;
;; We show one way to build a causal crank out of these parts. We envision many causal cranks that can bee implented depending on the use cases.
;; We also envision different ways to reason quantitatively over a causal crank (mini-Turing test a la Pearl)

;; Audience is programming language researchers who want to combine logical and probabilistic reasoning.
;; PPLs start with probability and try to add logic. We start with logic and add probability.
;; Advertise techniques for building problem solvers by connecting inference engines to truth maintenance systems, as advocated in BPS.
;; We follow this advice by connecting a causal inference engine to a truth maintenance system.
;; Doyle says TMSs are far more versatile than as implemented in BPS. He envisioned TMSs as a way to combine multiple reasoning engines into one coherent framework.
;; For example, two TMSs arguing with each other like philosophers (or lawyers).  As a small step towards this vision, we describe some examples in precision medicine.


;; Individual vs average treatment effect (Precision medicine)
;; In order to predict how a drug works on...
;; ....


(setq *causal* (make-causal
  :title "riflemen"
  :graph '((Court-orders . Captain-signals)
           (Captain-signals . Rifleman-A-shoots)
           (Captain-signals . Rifleman-B-shoots)
           (Rifleman-A-shoots . Prisoner-dies)
           (Rifleman-B-shoots . Prisoner-dies)
           (Rifleman-A-is-nervous . Rifleman-A-shoots))
  :priors '((Court-orders . 0.6)
            (Rifleman-A-is-nervous . 0.7))
  :symbolic-priors '((Court-orders . p)
                     (Rifleman-A-is-nervous . q))
  :given 'Prisoner-dies
  :intervention '(:NOT Rifleman-A-shoots)
  :outcome '(:NOT Prisoner-dies)))

;; (setq
;;  *causal*
;;  (make-causal
;;   :title "riflemen"
;;   :graph
;;   '((U . C)
;;     (C . A)
;;     (C . B)
;;     (A . D)
;;     (B . D)
;;     (W . A))
;;   :priors
;;   '((U . 0.6)
;;     (W . 0.7))
;;   :symbolic-priors
;;   '((U . p)
;;     (W . q))
;;   :given 'D
;;   :intervention '(:NOT A)
;;   :outcome '(:NOT D)))

;; U=0,W=0: 0.1
;; U=1,W=0: 0.2
;; U=0,W=1: 0.3
;; U=1,W=1: 0.4
(defun riflemen-jointf (c)
  (cond
    ((equal c '((:not Court-orders) (:not Rifleman-A-is-nervous))) 0.1)
    ((equal c '(Court-orders (:not Rifleman-A-is-nervous))) 0.2)
    ((equal c '((:not Court-orders) Rifleman-A-is-nervous)) 0.3)
    ((equal c '(Court-orders Rifleman-A-is-nervous)) 0.4)
    (t (error (format nil "unexpected combination: ~A" c)))))

(joint-causal-crank *causal* '(Court-orders Rifleman-A-is-nervous) #'riflemen-jointf)
#|
Outcome probability: 0.33.
|#

(causal-crank *causal*)
#|
Outcome probability: 0.32.
|#

(cdr (assoc (find-node (causal-atms *causal*) 'Prisoner-dies) (causal-all-ps *causal*)))
;; 0.88

(cdr (assoc (find-node (causal-atms *causal*) 'Prisoner-dies) (causal-given-ps *causal*)))
;; 1.0

(cdr (assoc (find-node (causal-post-atms *causal*) 'Prisoner-dies) (causal-post-ps *causal*)))
;; 0.68

(defun show-causal-alist (ps)
  (dolist (r ps)
    (let ((name (tms-node-datum (car r))))
      (when (not (consp name))
        (unless (equal "The contradiction" name)
          (format t "~A:" name)
          (format t "~2$ " (cdr r)))))))

(defun show-pre-intervention (causal)
  (show-causal-alist (causal-given-ps causal)))

(defun show-post-intervention (causal)
  (show-causal-alist (causal-post-ps causal)))

(show-pre-intervention *causal*)
;; PRISONER-DIES:1.00 CAPTAIN-SIGNALS:0.68 RIFLEMAN-A-SHOOTS:1.00 RIFLEMAN-B-SHOOTS:0.68 RIFLEMAN-A-IS-NERVOUS:0.80 COURT-ORDERS:0.68 outcome:0.00 given:1.00

(show-post-intervention *causal*)
;; CAPTAIN-SIGNALS:0.68 PRISONER-DIES:0.68 RIFLEMAN-B-SHOOTS:0.68 COURT-ORDERS:0.68 RIFLEMAN-A-SHOOTS:0.00 outcome:0.32 given:0.68

(setq *print-right-margin* 1000)
(symbolic-causal-crank *causal*)
#|
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
Outcome probability: 0.00.
|#

(symbolic-causal-crank *expected-disablement*)
#|
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
Outcome probability: 1.00.
|#

(symbolic-causal-crank *expected-sufficiency*)
#|
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
Outcome probability: 0.49.
|#

(symbolic-causal-crank *necessary-causation*)
#|
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
Outcome probability: 1.00.
|#

(symbolic-causal-crank *sufficient-causation*)
#|
Outcome probability: 1.00
|#
