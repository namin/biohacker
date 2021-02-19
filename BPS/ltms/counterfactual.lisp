(setq *ltms* (create-ltms
                "Rifleman Example"
                :complete t))
(setq
 U (tms-create-node *ltms*
     "Court orders" :ASSUMPTIONP t)
 C (tms-create-node *ltms*
    "Captain signals" :ASSUMPTIONP t)
 A (tms-create-node *ltms*
    "Rifleman A shoots" :ASSUMPTIONP t)
 B (tms-create-node *ltms*
     "Rifleman B shoots" :ASSUMPTIONP t)
 D (tms-create-node *ltms*
     "Prisoner dies" :ASSUMPTIONP t)
 )

(setq
  U=>C (tms-create-node *ltms*
      "Court orders => Captain signals" :ASSUMPTIONP t)
  C=>A (tms-create-node *ltms*
      "Captain signals => Rifleman A shoots" :ASSUMPTIONP t)
  C=>B (tms-create-node *ltms*
      "Captain signals => Rifleman B shoots" :ASSUMPTIONP t)
  A-or-B=>D (tms-create-node *ltms*
      "Rifleman A or B shoots => Prisoner dies" :ASSUMPTIONP t))

(compile-formula *ltms*
`(:IMPLIES "Court orders => Captain signals"
  (:AND (:IMPLIES
           "Court orders"
           "Captain signals")
        (:IMPLIES
           (:NOT "Court orders")
           (:NOT "Captain signals")))))
(compile-formula *ltms*
`(:IMPLIES "Captain signals => Rifleman A shoots"
  (:AND (:IMPLIES
            "Captain signals"
            "Rifleman A shoots")
        (:IMPLIES
           (:NOT "Captain signals")
           (:NOT "Rifleman A shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Captain signals => Rifleman B shoots"
  (:AND (:IMPLIES
           "Captain signals"
           "Rifleman B shoots")
        (:IMPLIES
           (:NOT "Captain signals")
           (:NOT "Rifleman B shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Rifleman A or B shoots => Prisoner dies"
 (:AND (:IMPLIES
          (:OR "Rifleman A shoots"
               "Rifleman B shoots")
          "Prisoner dies")
       (:IMPLIES
	(:AND
	    (:NOT "Rifleman A shoots")
            (:NOT "Rifleman B shoots"))
	(:NOT "Prisoner dies")))))
(enable-assumption U=>C :TRUE)
(enable-assumption C=>A :TRUE)
(enable-assumption C=>B :TRUE)
(enable-assumption A-or-B=>D :TRUE)


;; \subsection{Prediction}
;; If rifleman A did not shoot,the prisoner is alive.
;; $$\lnot A\implies \lnot D$$
;; Standard LTMS works for forward Prediction:

(enable-assumption A :FALSE)
  (explain-node D)

(retract-assumption A)



(setq *ltms* (create-ltms
                "Rifleman Example"
                :complete t))
(setq
 U (tms-create-node *ltms*
     "Court orders" :ASSUMPTIONP t)
 C (tms-create-node *ltms*
    "Captain signals" :ASSUMPTIONP t)
 A (tms-create-node *ltms*
    "Rifleman A shoots" :ASSUMPTIONP t)
 B (tms-create-node *ltms*
     "Rifleman B shoots" :ASSUMPTIONP t)
 D (tms-create-node *ltms*
    "Prisoner dies" :ASSUMPTIONP t)
 C* (tms-create-node *ltms*
    "Captain* signals" :ASSUMPTIONP t)
 A* (tms-create-node *ltms*
    "Rifleman A* shoots" :ASSUMPTIONP t)
 B* (tms-create-node *ltms*
     "Rifleman B* shoots" :ASSUMPTIONP t)
 D* (tms-create-node *ltms*
     "Prisoner* dies" :ASSUMPTIONP t)
 )

(setq
  U=>C (tms-create-node *ltms*
      "Court orders => Captain signals" :ASSUMPTIONP t)
  C=>A (tms-create-node *ltms*
      "Captain signals => Rifleman A shoots" :ASSUMPTIONP t)
  C=>B (tms-create-node *ltms*
      "Captain signals => Rifleman B shoots" :ASSUMPTIONP t)
  A-or-B=>D (tms-create-node *ltms*
			     "Rifleman A or B shoots => Prisoner dies" :ASSUMPTIONP t)
  U=>C* (tms-create-node *ltms*
      "Court orders => Captain* signals" :ASSUMPTIONP t)
  C*=>A* (tms-create-node *ltms*
      "Captain* signals => Rifleman A* shoots" :ASSUMPTIONP t)
  C*=>B* (tms-create-node *ltms*
      "Captain* signals => Rifleman B* shoots" :ASSUMPTIONP t)
  A*-or-B*=>D* (tms-create-node *ltms*
      "Rifleman A* or B* shoots => Prisoner* dies" :ASSUMPTIONP t)

  )

(compile-formula *ltms*
`(:IMPLIES "Court orders => Captain signals"
  (:AND (:IMPLIES
           "Court orders"
           "Captain signals")
        (:IMPLIES
           (:NOT "Court orders")
           (:NOT "Captain signals")))))
(compile-formula *ltms*
`(:IMPLIES "Captain signals => Rifleman A shoots"
  (:AND (:IMPLIES
            "Captain signals"
            "Rifleman A shoots")
        (:IMPLIES
           (:NOT "Captain signals")
           (:NOT "Rifleman A shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Captain signals => Rifleman B shoots"
  (:AND (:IMPLIES
           "Captain signals"
           "Rifleman B shoots")
        (:IMPLIES
           (:NOT "Captain signals")
           (:NOT "Rifleman B shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Rifleman A or B shoots => Prisoner dies"
 (:AND (:IMPLIES
          (:OR "Rifleman A shoots"
               "Rifleman B shoots")
          "Prisoner dies")
       (:IMPLIES
	(:AND
	    (:NOT "Rifleman A shoots")
            (:NOT "Rifleman B shoots"))
	(:NOT "Prisoner dies")))))
(compile-formula *ltms*
`(:IMPLIES "Court orders => Captain* signals"
  (:AND (:IMPLIES
           "Court orders"
           "Captain* signals")
        (:IMPLIES
           (:NOT "Court orders")
           (:NOT "Captain* signals")))))
(compile-formula *ltms*
`(:IMPLIES "Captain* signals => Rifleman A* shoots"
  (:AND (:IMPLIES
            "Captain* signals"
            "Rifleman A* shoots")
        (:IMPLIES
           (:NOT "Captain* signals")
           (:NOT "Rifleman A* shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Captain* signals => Rifleman B* shoots"
  (:AND (:IMPLIES
           "Captain* signals"
           "Rifleman B* shoots")
        (:IMPLIES
           (:NOT "Captain* signals")
           (:NOT "Rifleman B* shoots")))))
(compile-formula *ltms*
`(:IMPLIES "Rifleman A* or B* shoots => Prisoner* dies"
 (:AND (:IMPLIES
          (:OR "Rifleman A* shoots"
               "Rifleman B* shoots")
          "Prisoner* dies")
       (:IMPLIES
	(:AND
	    (:NOT "Rifleman A* shoots")
            (:NOT "Rifleman B* shoots"))
	(:NOT "Prisoner* dies")))))

(enable-assumption U=>C :TRUE)
(enable-assumption C=>A :TRUE)
(enable-assumption C=>B :TRUE)
(enable-assumption A-or-B=>D :TRUE)
(enable-assumption U=>C* :TRUE)
(enable-assumption C*=>A* :TRUE)
(enable-assumption C*=>B* :TRUE)
(enable-assumption A*-or-B*=>D* :TRUE)


;; Given prisoner is dead, would he still be dead in a world where A did not shoot?

(enable-assumption D :TRUE)  ;; Prisoner is dead in the factual world
(retract-assumption C*=>A*)  ;; Counterfactal Rifleman A* makes his own choice
(enable-assumption A* :FALSE) ;; Counterfactual Rifleman A* chooses not to shoot
(why-node D*)
#|
Prisoner* dies is TRUE via #<Clause 74> on
   Rifleman B* shoots is TRUE
   Rifleman A* or B* shoots => Prisoner* dies is TRUE
#<NODE: Prisoner* dies>
|#
