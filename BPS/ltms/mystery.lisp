(setq *ltms* (create-ltms
                "Murder Mystery"
                :complete t))

(setq
 A
 (tms-create-node *ltms*
                  "Alice is guilty")
 A-motive
 (tms-create-node *ltms*
                  "Alice has motive" :ASSUMPTIONP t)
 A-means
 (tms-create-node *ltms*
                  "Alice has means" :ASSUMPTIONP t)
 A-opportunity
 (tms-create-node *ltms*
                  "Alice has opportunity" :ASSUMPTIONP t)
 B
 (tms-create-node *ltms*
                  "Bob is guilty")
 B-motive
 (tms-create-node *ltms*
                  "Bob has motive" :ASSUMPTIONP t)
 B-means
 (tms-create-node *ltms*
                  "Bob has means" :ASSUMPTIONP t)
 B-opportunity
 (tms-create-node *ltms*
                  "Bob has opportunity" :ASSUMPTIONP t)
 )

(compile-formula *ltms*
                 `(:IMPLIES (:AND "Alice has motive"
                                  "Alice has means"
                                  "Alice has opportunity")
                            "Alice is guilty"))

(compile-formula *ltms*
                 `(:IMPLIES (:AND "Bob has motive"
                                  "Bob has means"
                                  "Bob has opportunity")
                            "Bob is guilty"))

(compile-formula *ltms*
                 `(:OR (:AND "Alice is guilty" (:NOT "Bob is guilty"))
                       (:AND "Bob is guilty" (:NOT "Alice is guilty"))))

(enable-assumption (find-node *ltms* "Alice has motive") :TRUE)
(enable-assumption (find-node *ltms* "Alice has means") :TRUE)
(enable-assumption (find-node *ltms* "Alice has opportunity") :TRUE)

(enable-assumption (find-node *ltms* "Bob has motive") :FALSE)
(enable-assumption (find-node *ltms* "Bob has means") :TRUE)
(enable-assumption (find-node *ltms* "Bob has opportunity") :TRUE)

(explain-node (find-node *ltms* "Alice is guilty"))
#|
  1 Alice has opportunity             ()   Assumption
  2 Alice has means             ()   Assumption
  3 Alice has motive             ()   Assumption
  4 Alice is guilty         (1 2 3)  (:OR Alice is guilty (:NOT Alice has motive) (:NOT Alice has means) (:NOT Alice has opportunity))
|#

(retract-assumption (find-node *ltms* "Bob has motive"))
(retract-assumption (find-node *ltms* "Alice has motive"))
(enable-assumption (find-node *ltms* "Bob has motive") :TRUE)
(explain-node (find-node *ltms* "Alice is guilty"))
#|
  1 Bob has opportunity             ()   Assumption
  2   Bob has means             ()   Assumption
  3  Bob has motive             ()   Assumption
  4   Bob is guilty         (1 2 3)  (:OR Bob is guilty (:NOT Bob has motive) (:NOT Bob has means) (:NOT Bob has opportunity))
  5 (:NOT Alice is guilty)             (4)  (:OR (:NOT Alice is guilty) (:NOT Bob is guilty))
|#
