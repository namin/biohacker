(setq *atms* (create-atms "riflemen" :debugging t))
(setq u (tms-create-node *atms* "U"))
(setq w (tms-create-node *atms* "W"))
(setq c (tms-create-node *atms* "C"))
(setq a (tms-create-node *atms* "A"))
(setq b (tms-create-node *atms* "B"))
(setq d (tms-create-node *atms* "D"))
(setq c* (tms-create-node *atms* "C*"))
(setq a* (tms-create-node *atms* "A*"))
(setq b* (tms-create-node *atms* "B*"))
(setq d* (tms-create-node *atms* "D*"))

(setq nu (tms-create-node *atms* "notU"))
(setq nw (tms-create-node *atms* "notW"))
(setq nc (tms-create-node *atms* "notC"))
(setq na (tms-create-node *atms* "notA"))
(setq nb (tms-create-node *atms* "notB"))
(setq nd (tms-create-node *atms* "notD"))
(setq nc* (tms-create-node *atms* "notC*"))
(setq na* (tms-create-node *atms* "notA*"))
(setq nb* (tms-create-node *atms* "notB*"))
(setq nd* (tms-create-node *atms* "notD*"))

(nogood-nodes 'nogood-u (list u nu))
(nogood-nodes 'nogood-w (list w nw))
(nogood-nodes 'nogood-c (list c nc))
(nogood-nodes 'nogood-a (list a na))
(nogood-nodes 'nogood-b (list b nb))
(nogood-nodes 'nogood-d (list d nd))
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

(why-nodes *atms*)
#|
<The contradiction,{}>
<U,{{D*,notA*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<W,{{W}}>
<C,{{D*,notA*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<A,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{W}{U}}>
<B,{{D*,notA*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<D,{{D*,notA*}{B*}{C*}{D}{B}{A}{C}{W}{U}}>
<C*,{{D*,notA*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<A*,{}>
<B*,{{D*,notA*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<D*,{{D*}{B*}{C*}{D,notW}{B}{A,notW}{C}{U}}>
<notU,{{notD*}{notB*}{notC*}{notB}{notC}{notU}}>
<notW,{{notW}}>
<notC,{{notD*}{notB*}{notC*}{notB}{notC}{notU}}>
<notA,{}>
<notB,{{notD*}{notB*}{notC*}{notB}{notC}{notU}}>
<notD,{}>
<notC*,{{notD*}{notB*}{notC*}{notB}{notC}{notU}}>
<notA*,{{notD*}{notA*}}>
<notB*,{{notD*}{notB*}{notC*}{notB}{notC}{notU}}>
<notD*,{{notD*}{notA*,notB*}{notA*,notC*}{notA*,notB}{notA*,notC}{notA*,notU}}>
|#

;; prediction: notA implies notD
;; abduction: notD implies notC
;; transduction: A implies B
;; action: notC and A* implies D* and notB*

(setq *ps*
      (list (cons u 0.6)
            (cons nu 0.4)
            (cons w 0.7)
            (cons nw 0.3)
            (cons na* 1.0)
            (cons d 1.0)))

(defun env-prob (e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply #'* (mapcar #'cdr kps)))))

(defun label-prob (l ps)
  (union-prob (remove nil (mapcar #'(lambda (e) (env-prob e ps)) l))))

(defun choose (vs k)
  (cond
    ((= k 0) '(()))
    ((null vs) '())
    (t
     (append
      (mapcar #'(lambda (x) (cons (car vs) x)) (choose (cdr vs) (- k 1)))
      (choose (cdr vs) k)))))

;; P(A U B) = P(A) + P(B) - P(A)*P(B)
;; P(A U B C) = P(A) + P(B) + P(C) - P(A)*P(B) - P(A)*P(C) - P(B)*P(C) + P(A)*P(B)*P(C)
(defun union-prob (vs)
  (union-prob-iter vs 1 (length vs)))

(defun union-prob-iter (vs k n)
  (let ((r (choose vs k)))
    (if (null r)
        0
        (- (apply #'+ (mapcar #'(lambda (x) (apply #'* x)) r))
           (union-prob-iter vs (+ k 1) n)))))

(defun node-prob (n ps)
  (label-prob (tms-node-label n) ps))

(defun prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$" (node-prob node ps))
  (format stream ">"))

(defun prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (prob-node n ps stream)))

(prob-nodes *atms* *ps*)
#|
<The contradiction,0.00>
<U,0.72>
<W,0.70>
<C,0.72>
<A,1.00>
<B,0.72>
<D,1.00>
<C*,0.72>
<A*,0.00>
<B*,0.72>
<D*,0.72>
<notU,0.40>
<notW,0.30>
<notC,0.40>
<notA,0.00>
<notB,0.40>
<notD,0.00>
<notC*,0.40>
<notA*,1.00>
<notB*,0.40>
<notD*,0.40>
|#

(defun why-prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$:{" (node-prob node ps))
  (dolist (e (tms-node-label node))
    (let ((pe (env-prob e ps)))
      (when pe (format stream "~2$:" pe))
      (env-string e stream)))
  (format stream "}>"))

(defun why-prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-prob-node n ps stream)))

(why-prob-nodes *atms* *ps*)
#|
;; 0.30:{D,notW} = 1.0 (for D) * 0.3 (for notW)
;; P(D /\ notW) = P(U) = 0.60
<The contradiction,0.00:{}>
<U,0.72:{{D*,notA*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<W,0.70:{0.70:{W}}>
<C,0.72:{{D*,notA*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<A,1.00:{{D*,notA*}{B*}{C*}1.00:{D}{B}{A}{C}0.70:{W}0.60:{U}}>
<B,0.72:{{D*,notA*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<D,1.00:{{D*,notA*}{B*}{C*}1.00:{D}{B}{A}{C}0.70:{W}0.60:{U}}>
<C*,0.72:{{D*,notA*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<A*,0.00:{}>
<B*,0.72:{{D*,notA*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<D*,0.72:{{D*}{B*}{C*}0.30:{D,notW}{B}{A,notW}{C}0.60:{U}}>
<notU,0.40:{{notD*}{notB*}{notC*}{notB}{notC}0.40:{notU}}>
<notW,0.30:{0.30:{notW}}>
<notC,0.40:{{notD*}{notB*}{notC*}{notB}{notC}0.40:{notU}}>
<notA,0.00:{}>
<notB,0.40:{{notD*}{notB*}{notC*}{notB}{notC}0.40:{notU}}>
<notD,0.00:{}>
<notC*,0.40:{{notD*}{notB*}{notC*}{notB}{notC}0.40:{notU}}>
<notA*,1.00:{{notD*}1.00:{notA*}}>
<notB*,0.40:{{notD*}{notB*}{notC*}{notB}{notC}0.40:{notU}}>
<notD*,0.40:{{notD*}{notA*,notB*}{notA*,notC*}{notA*,notB}{notA*,notC}0.40:{notA*,notU}}>
|#
