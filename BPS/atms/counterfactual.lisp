(setq *atms* (create-atms "riflemen" :debugging t))
(setq u (tms-create-node *atms* "U"))
(setq c (tms-create-node *atms* "C"))
(setq a (tms-create-node *atms* "A"))
(setq b (tms-create-node *atms* "B"))
(setq d (tms-create-node *atms* "D"))

(setq nu (tms-create-node *atms* "notU"))
(setq nc (tms-create-node *atms* "notC"))
(setq na (tms-create-node *atms* "notA"))
(setq nb (tms-create-node *atms* "notB"))
(setq nd (tms-create-node *atms* "notD"))

(nogood-nodes 'nogood-u (list u nu))
(nogood-nodes 'nogood-c (list c nc))
(nogood-nodes 'nogood-a (list a na))
(nogood-nodes 'nogood-b (list b nb))
(nogood-nodes 'nogood-d (list d nd))

(assume-node u)
(assume-node nu)

(justify-node 'J_UC c (list u))
(justify-node 'J_CU u (list c))
(justify-node 'J_UC a (list c))
(justify-node 'J_CU c (list a))
(justify-node 'J_UC b (list c))
(justify-node 'J_CU c (list b))
(justify-node 'J_UC d (list a))
(justify-node 'J_CU a (list d))
(justify-node 'J_UC d (list b))
(justify-node 'J_CU b (list d))

(justify-node 'J_UC nc (list nu))
(justify-node 'J_CU nu (list nc))
(justify-node 'J_UC na (list nc))
(justify-node 'J_CU nc (list na))
(justify-node 'J_UC nb (list nc))
(justify-node 'J_CU nc (list nb))
(justify-node 'J_CU na (list nd))
(justify-node 'J_CU nb (list nd))
(justify-node 'J_UC nd (list na nb))


(why-nodes *atms*)
#|
<The contradiction,{}>
<U,{{U}}>
<C,{{U}}>
<A,{{U}}>
<B,{{U}}>
<D,{{U}}>
<notU,{{notU}}>
<notC,{{notU}}>
<notA,{{notU}}>
<notB,{{notU}}>
<notD,{{notU}}>
|#
