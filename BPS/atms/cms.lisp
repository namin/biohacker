(setq *pack-ltms* t)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)
(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)
(in-package :COMMON-LISP-USER)
(bps-load-file (make-bps-path "atms") "atre" :action :compile)
(bps-load-file (make-bps-path "atms") "atms" :action :compile)
(compile-atre)

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

(setq formula
      '(:and
        (:and
         (:implies "U" "C") (:implies "C" "U")
         (:implies "C" "A") (:implies "A" "C")
         (:implies "C" "B") (:implies "B" "C")
         (:implies (:or "A" "B") "D")
         (:implies "D" (:or "A" "B")))
        (:and
         (:implies "U" "C*") (:implies "C*" "U")
         ;;(:implies "C*" "A*") (:implies "A*" "C*")
         (:implies "C*" "B*") (:implies "B*" "C*")
         (:implies (:or "A*" "B*") "D*")
         (:implies "D*" (:or "A*" "B*")))
        (:and
         (:implies "notU" "notC") (:implies "notC" "notU")
         (:implies "notC" "notA") (:implies "notA" "notC")
         (:implies "notC" "notB") (:implies "notB" "notC")
         (:implies (:and "notA" "notB") "notD")
         (:implies "notD" (:or "notA" "notB"))
         )
        (:and
         (:implies "notU" "notC*") (:implies "notC*" "notU")
         ;;(:implies "notC*" "notA*") (:implies "notA*" "notC*")
         (:implies "notC*" "notB*") (:implies "notB*" "notC*")
         (:implies (:and "notA*" "notB*") "notD*")
         (:implies "notD*" (:and "notA*" "notB*")))))

(setq p (PLTMS::prime-implicates formula))

(setq clauses (PLTMS::collect p))

(defun horn-clause? (c)
  (= 1 (length (remove-if-not #'(lambda (l) (eq (cdr l) ':TRUE)) (PLTMS::clause-literals c)))))

(setq horn-clauses (remove-if-not #'horn-clause? clauses))

(length horn-clauses)

(setq non-horn-clauses (remove-if #'horn-clause? clauses))

(mapcar #'PLTMS::pretty-print-clause non-horn-clauses)
#|
(:OR A* (:NOT D*) C)
(:OR A* (:NOT D*) B*)
(:OR A* (:NOT D*) B)
(:OR A* (:NOT D*) A)
(:OR A* (:NOT D*) U)
(:OR A* (:NOT D*) C*)
(:OR A* (:NOT D*) D)
|#

(defun find-node (atms name)
  (find-if #'(lambda (n) (equal name (tms-node-datum n))) (atms-nodes atms)))

(defun translate-node (n)
  (find-node *atms* (PLTMS::tms-node-datum n)))

(defun add-horn-justification (c)
  (justify-node
   'J
   (translate-node (car (car (remove-if-not #'(lambda (l) (eq (cdr l) ':TRUE)) (PLTMS::clause-literals c)))))
   (mapcar #'translate-node (mapcar #'car (remove-if-not #'(lambda (l) (eq (cdr l) ':FALSE)) (PLTMS::clause-literals c))))))

(mapcar #'add-horn-justification  horn-clauses)

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
<U,{{B*}{C*}{D}{B}{A}{C}{U}}>
<C,{{B*}{C*}{D}{B}{A}{C}{U}}>
<A,{{B*}{C*}{D}{B}{A}{C}{U}}>
<B,{{B*}{C*}{D}{B}{A}{C}{U}}>
<D,{{B*}{C*}{D}{B}{A}{C}{U}}>
<C*,{{B*}{C*}{D}{B}{A}{C}{U}}>
<A*,{{A*}}>
<B*,{{B*}{C*}{D}{B}{A}{C}{U}}>
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

;; ...
(prob-nodes *atms* *ps*)
#|
<U,0.60>
<C,0.60>
<A,0.60>
<B,0.60>
<D,0.60>
<C*,0.60>
<A*,0.70>
<B*,0.60>
<D*,0.88>
<notU,0.40>
<notC,0.40>
<notA,0.40>
<notB,0.40>
<notD,0.40>
<notC*,0.40>
<notA*,0.30>
<notB*,0.40>
<notD*,0.12>
|#

;; ...
(why-prob-nodes *atms* *ps*)
#|
<The contradiction,0.00:{}>
<U,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<C,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<A,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<B,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<D,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<C*,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<A*,0.70:{0.70:{A*}}>
<B*,0.60:{{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<D*,0.88:{{D*}{B*}0.70:{A*}{C*}{D}{B}{A}{C}0.60:{U}}>
<notU,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notC,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notA,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notB,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notD,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notC*,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notA*,0.30:{{notD*}0.30:{notA*}}>
<notB*,0.40:{{notD*}{notB*}{notC*}{notD}{notB}{notA}{notC}0.40:{notU}}>
<notD*,0.12:{{notD*}{notA*,notB*}{notA*,notC*}{notA*,notD}{notA*,notB}{notA,notA*}{notA*,notC}0.12:{notA*,notU}}>
|#
