(setq *atms* (create-atms "riflemen" :debugging t))
(setq u (tms-create-node *atms* "U"))
(setq w (tms-create-node *atms* "W"))
(setq c (tms-create-node *atms* "C"))
(setq a (tms-create-node *atms* "A"))
(setq b (tms-create-node *atms* "B"))
(setq d (tms-create-node *atms* "D"))

(setq *twin-atms* (create-atms "riflemen twin" :debugging t))
(setq u* (tms-create-node *twin-atms* "U*"))
(setq c* (tms-create-node *twin-atms* "C*"))
(setq a* (tms-create-node *twin-atms* "A*"))
(setq b* (tms-create-node *twin-atms* "B*"))
(setq d* (tms-create-node *twin-atms* "D*"))

(setq nu (tms-create-node *atms* "notU"))
(setq nw (tms-create-node *atms* "notW"))
(setq nc (tms-create-node *atms* "notC"))
(setq na (tms-create-node *atms* "notA"))
(setq nb (tms-create-node *atms* "notB"))
(setq nd (tms-create-node *atms* "notD"))
(setq nu* (tms-create-node *twin-atms* "notU*"))
(setq nc* (tms-create-node *twin-atms* "notC*"))
(setq na* (tms-create-node *twin-atms* "notA*"))
(setq nb* (tms-create-node *twin-atms* "notB*"))
(setq nd* (tms-create-node *twin-atms* "notD*"))

(nogood-nodes 'nogood-u (list u nu))
(nogood-nodes 'nogood-w (list w nw))
(nogood-nodes 'nogood-c (list c nc))
(nogood-nodes 'nogood-a (list a na))
(nogood-nodes 'nogood-b (list b nb))
(nogood-nodes 'nogood-d (list d nd))
(nogood-nodes 'nogood-u* (list u* nu*))
(nogood-nodes 'nogood-c* (list c* nc*))
(nogood-nodes 'nogood-a* (list a* na*))
(nogood-nodes 'nogood-b* (list b* nb*))
(nogood-nodes 'nogood-d* (list d* nd*))

(justify-node 'J_UC c (list u))
(justify-node 'J_CU u (list c))
(justify-node 'J_CA a (list c))
(justify-node 'J_WA a (list w))
;; A shooting no longer implies captain gave the order,
;; unless notW.
(justify-node 'J_AC c (list a nw))
(justify-node 'J_CB b (list c))
(justify-node 'J_BC c (list b))
(justify-node 'J_AD d (list a))
(justify-node 'J_DA a (list d))
(justify-node 'J_BD d (list b))
(justify-node 'J_DB b (list d nw))

(justify-node 'JN_UC nc (list nu))
(justify-node 'JN_CU nu (list nc))
(justify-node 'JN_CA na (list nc nw))
(justify-node 'JN_AC nc (list na))
(justify-node 'JN_CB nb (list nc))
(justify-node 'JN_BC nc (list nb))
(justify-node 'JN_DA na (list nd))
(justify-node 'JN_DB nb (list nd))
(justify-node 'JN_ABD nd (list na nb))

(justify-node 'J_UC c* (list u*))
(justify-node 'J_CU u* (list c*))
;;(justify-node 'J_CA a* (list c*))
;;(justify-node 'J_AC c* (list a*))
(justify-node 'J_CB b* (list c*))
(justify-node 'J_BC c* (list b*))
(justify-node 'J_AD d* (list a*))
(justify-node 'J_DA a* (list d* nb*))
(justify-node 'J_BD d* (list b*))
(justify-node 'J_DB b* (list d* na*))

(justify-node 'JN_UC nc* (list nu*))
(justify-node 'JN_CU nu* (list nc*))
;;(justify-node 'JN_CA na* (list nc*))
;;(justify-node 'JN_AC nc* (list na*))
(justify-node 'JN_CB nb* (list nc*))
(justify-node 'JN_BC nc* (list nb*))
(justify-node 'JN_DA na* (list nd*))
(justify-node 'JN_DB nb* (list nd*))
(justify-node 'JN_ABD nd* (list na* nb*))

(assume-node u)
(assume-node nu)
(assume-node w)
(assume-node nw)
(assume-node c)
(assume-node nc)
(assume-node a)
(assume-node na)
(assume-node b)
(assume-node nb)
(assume-node d)
;;(assume-node nd)
(nogood-nodes 'nogood-nd (list nd))

(assume-node u*)
(assume-node nu*)
(assume-node c*)
(assume-node nc*)
;;(assume-node a*)
(nogood-nodes 'nogood-a* (list a*))
(assume-node na*)
(assume-node b*)
(assume-node nb*)
(assume-node d*)
(assume-node nd*)

#|
;; Step 1: calculate the joint distribution given outcome
;; joint distribution
P(U) = p
P(W) = q
P(u,w) = P(u)*P(w)
;; calculate the joint distribution given outcome
P(u,w | D) = P(u,w) / (1 - p(u=0,w=0)) if u=1 or w=1 else 0 (if u=0 and w=0)
;; would we get same result using different denominator?
P(D) = (1 - p(u=0,w=0)) = 1-(1-p)(1-q)
P(D) = P(U) + P(W) - P(U)P(W) = p + q - pq
1-(1-p)(1-q) = 1 - 1 - pq +  p + q = p + q - pq ;; same

;; Step 2:  form submodel, while retaining the posterior probability
;; A does not shoot
;; Step 3: compute oucome in this model
;; need to note that not D => not U
|#

(setq *ps*
      (list (cons u 0.6)
            (cons nu 0.4)
            (cons w 0.7)
            (cons nw 0.3)))

(setq pd (node-prob d *ps*))
(setq pud (/ 0.6 pd))

(setq *twin-ps*
      (list (cons u* pud)
            (cons nu* (- 1 pud))
            (cons na* 1.0)
            (cons a* 0.0)))

(why-nodes *twin-atms*)
#|
<The contradiction,{}>
<U*,{{D*,notA*}{B*}{C*}{U*}}>
<C*,{{D*,notA*}{B*}{C*}{U*}}>
<A*,{}>
<B*,{{D*,notA*}{B*}{C*}{U*}}>
<D*,{{D*}{B*}{C*}{U*}}>
<notU*,{{notD*}{notB*}{notC*}{notU*}}>
<notC*,{{notD*}{notB*}{notC*}{notU*}}>
<notA*,{{notD*}{notA*}}>
<notB*,{{notD*}{notB*}{notC*}{notU*}}>
<notD*,{{notD*}{notA*,notB*}{notA*,notC*}{notA*,notU*}}>
|#

(prob-nodes *twin-atms* *twin-ps*)
#|
<The contradiction,0.00>
<U*,0.68>
<C*,0.68>
<A*,0.00>
<B*,0.68>
<D*,0.68>
<notU*,0.32>
<notC*,0.32>
<notA*,1.00>
<notB*,0.32>
<notD*,0.32>
|#

(why-prob-nodes *twin-atms* *twin-ps*)
#|
<The contradiction,0.00:{}>
<U*,0.68:{{D*,notA*}{B*}{C*}0.68:{U*}}>
<C*,0.68:{{D*,notA*}{B*}{C*}0.68:{U*}}>
<A*,0.00:{}>
<B*,0.68:{{D*,notA*}{B*}{C*}0.68:{U*}}>
<D*,0.68:{{D*}{B*}{C*}0.68:{U*}}>
<notU*,0.32:{{notD*}{notB*}{notC*}0.32:{notU*}}>
<notC*,0.32:{{notD*}{notB*}{notC*}0.32:{notU*}}>
<notA*,1.00:{{notD*}1.00:{notA*}}>
<notB*,0.32:{{notD*}{notB*}{notC*}0.32:{notU*}}>
<notD*,0.32:{{notD*}{notA*,notB*}{notA*,notC*}0.32:{notA*,notU*}}>
|#
