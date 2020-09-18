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

(setq aORb (tms-create-node *atms* "AorB"))

(nogood-nodes 'nogood-u (list u nu))
(nogood-nodes 'nogood-c (list c nc))
(nogood-nodes 'nogood-a (list a na))
(nogood-nodes 'nogood-b (list b nb))
(nogood-nodes 'nogood-d (list d nd))
(nogood-nodes 'nogood-d (list aORb na nb))
(assume-node u)
(assume-node nu)

(justify-node 'J_UC c (list u))
(justify-node 'J_CU u (list c))
(justify-node 'J_CA a (list c))
(justify-node 'J_AC c (list a))
(justify-node 'J_CB b (list c))
(justify-node 'J_BC c (list b))
(justify-node 'J_AD d (list a))
(justify-node 'J_BD d (list b))
(justify-node 'J_DAB aORb (list d))

(justify-node 'JN_UC nc (list nu))
(justify-node 'JN_CU nu (list nc))
(justify-node 'JN_CA na (list nc))
(justify-node 'JN_AC nc (list na))
(justify-node 'JN_CB nb (list nc))
(justify-node 'JN_BC nc (list nb))
(justify-node 'JN_DA na (list nd))
(justify-node 'JN_DB nb (list nd))
(justify-node 'JN_ABD nd (list na nb))


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

(assume-node c)
(assume-node nc)
(assume-node a)
(assume-node na)
(assume-node b)
(assume-node nb)
(assume-node d)
(assume-node nd)

(why-nodes *atms*)
#|
<The contradiction,{}>
<U,{{D}{B}{A}{C}{U}}>
<C,{{D}{B}{A}{C}{U}}>
<A,{{D}{B}{A}{C}{U}}>
<B,{{D}{B}{A}{C}{U}}>
<D,{{D}{B}{A}{C}{U}}>
<notU,{{notD}{notB}{notA}{notC}{notU}}>
<notC,{{notD}{notB}{notA}{notC}{notU}}>
<notA,{{notD}{notB}{notA}{notC}{notU}}>
<notB,{{notD}{notB}{notA}{notC}{notU}}>
<notD,{{notD}{notB}{notA}{notC}{notU}}>
|#
