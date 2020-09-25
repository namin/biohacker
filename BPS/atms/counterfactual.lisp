(setq *atms* (create-atms "riflemen" :debugging t))
(setq u (tms-create-node *atms* "U"))
(setq c (tms-create-node *atms* "C"))
(setq a (tms-create-node *atms* "A"))
(setq b (tms-create-node *atms* "B"))
(setq d (tms-create-node *atms* "D"))
(setq c* (tms-create-node *atms* "C*"))
(setq a* (tms-create-node *atms* "A*"))
(setq b* (tms-create-node *atms* "B*"))
(setq d* (tms-create-node *atms* "D*"))

(setq nu (tms-create-node *atms* "notU"))
(setq nc (tms-create-node *atms* "notC"))
(setq na (tms-create-node *atms* "notA"))
(setq nb (tms-create-node *atms* "notB"))
(setq nd (tms-create-node *atms* "notD"))
(setq nc* (tms-create-node *atms* "notC*"))
(setq na* (tms-create-node *atms* "notA*"))
(setq nb* (tms-create-node *atms* "notB*"))
(setq nd* (tms-create-node *atms* "notD*"))

(nogood-nodes 'nogood-u (list u nu))
(nogood-nodes 'nogood-c (list c nc))
(nogood-nodes 'nogood-a (list a na))
(nogood-nodes 'nogood-b (list b nb))
(nogood-nodes 'nogood-d (list d nd))
(nogood-nodes 'nogood-c (list c* nc*))
(nogood-nodes 'nogood-a (list a* na*))
(nogood-nodes 'nogood-b (list b* nb*))
(nogood-nodes 'nogood-d (list d* nd*))

(justify-node 'J_UC c (list u))
(justify-node 'J_CU u (list c))
(justify-node 'J_CA a (list c))
(justify-node 'J_AC c (list a))
(justify-node 'J_CB b (list c))
(justify-node 'J_BC c (list b))
(justify-node 'J_AD d (list a))
(justify-node 'J_DA a (list d))
(justify-node 'J_BD d (list b))
(justify-node 'J_DB b (list d))

(justify-node 'JN_UC nc (list nu))
(justify-node 'JN_CU nu (list nc))
(justify-node 'JN_CA na (list nc))
(justify-node 'JN_AC nc (list na))
(justify-node 'JN_CB nb (list nc))
(justify-node 'JN_BC nc (list nb))
(justify-node 'JN_DA na (list nd))
(justify-node 'JN_DB nb (list nd))
(justify-node 'JN_ABD nd (list na nb))

(justify-node 'J_UC c* (list u))
(justify-node 'J_CU u (list c*))
;;(justify-node 'J_CA a* (list c*))
;;(justify-node 'J_AC c* (list a*))
(justify-node 'J_CB b* (list c*))
(justify-node 'J_BC c* (list b*))
(justify-node 'J_AD d* (list a*))
(justify-node 'J_DA a* (list d* nb*))
(justify-node 'J_BD d* (list b*))
(justify-node 'J_DB b* (list d* na*))

(justify-node 'JN_UC nc* (list nu))
(justify-node 'JN_CU nu (list nc*))
;;(justify-node 'JN_CA na* (list nc*))
;;(justify-node 'JN_AC nc* (list na*))
(justify-node 'JN_CB nb* (list nc*))
(justify-node 'JN_BC nc* (list nb*))
(justify-node 'JN_DA na* (list nd*))
(justify-node 'JN_DB nb* (list nd*))
(justify-node 'JN_ABD nd* (list na* nb*))

(assume-node u)
(assume-node nu)

(assume-node c)
(assume-node nc)
(assume-node a)
(assume-node na)
(assume-node b)
(assume-node nb)
(assume-node d)
(assume-node nd)

(assume-node c*)
(assume-node nc*)
(assume-node a*)
(assume-node na*)
(assume-node b*)
(assume-node nb*)
(assume-node d*)
(assume-node nd*)

(why-nodes *atms*)
#|
<The contradiction,{}>
<U,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<C,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<A,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<B,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<D,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<C*,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<A*,{{D*,notB*}{D*,notC*}{D*,notD}{D*,notB}{D*,notA}{D*,notC}{D*,notU}{A*}}>
<B*,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{U}}>
<D*,{{D*}{B*}{A*}{C*}{D}{B}{A}{C}{U}}>
<notU,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notC,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notA,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notB,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notD,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notC*,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notA*,{{notD*}{notA*}}>
<notB*,{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}{notU}}>
<notD*,{{notD*}{notA*,notB*}{notA*,notC*}{notA*,notD}{notA*,notB}{notA,notA*}{notA*,notC}{notA*,notU}}>
|#

;; prediction: notA implies notD
;; abduction: notD implies notC
;; transduction: A implies B
;; action: notC and A* implies D* and notB*

(setq *ps*
      (list (cons u 0.6)
            (cons nu 0.4)
            (cons a* 0.7)
            (cons na* 0.3)))

;; this logic is wrong (see below)
;; but we will use it as a starting point

(defun env-prob (e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply #'* (mapcar #'cdr kps)))))

(defun label-prob (l ps)
  (apply #'+ (remove nil (mapcar #'(lambda (e) (env-prob e ps)) l))))

(defun node-prob (n ps)
  (label-prob (tms-node-label n) ps))

(defun prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A,{" prefix (tms-node-datum node))
  (format stream "~A" (node-prob node ps))
  (format stream "}>"))

(defun prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (prob-node n ps stream )))

(prob-nodes *atms* *ps*)
#|
<The contradiction,{0}>
<U,{0.6}>
<C,{0.6}>
<A,{0.6}>
<B,{0.6}>
<D,{0.6}>
<C*,{0.6}>
<A*,{0.7}>
<B*,{0.6}>
<D*,{1.3}>
<notU,{0.4}>
<notC,{0.4}>
<notA,{0.4}>
<notB,{0.4}>
<notD,{0.4}>
<notC*,{0.4}>
<notA*,{0.3}>
<notB*,{0.4}>
<notD*,{0.120000005}>
|#
