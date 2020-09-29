(setq *pack-ltms* t)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)
(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)
(in-package :COMMON-LISP-USER)
(bps-load-file (make-bps-path "atms") "atre" :action :compile)
(bps-load-file (make-bps-path "atms") "atms" :action :compile)
(compile-atre)

(setq *complements* nil)
(defun complements (x nx)
  (nogood-nodes 'nogood-complement (list x nx))
  (push (cons x nx) *complements*)
  (push (cons nx x) *complements*))

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

(complements u nu)
(complements c nc)
(complements a na)
(complements b nb)
(complements d nd)
(complements c* nc*)
(complements a* na*)
(complements b* nb*)
(complements d* nd*)

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

(defun print-clauses (cs)
  (loop for c in cs do
    (PLTMS::pretty-print-clause c)
    (format t "~%")))

(defun horn-clause? (c)
  (= 1 (length (remove-if-not #'(lambda (l) (eq (cdr l) ':TRUE)) (PLTMS::clause-literals c)))))

#|
Horn clause format:
(:OR (:NOT A1) (:NOT A2) ... (:NOT A3) B)
|#
(setq horn-clauses (remove-if-not #'horn-clause? clauses))
(print-clauses horn-clauses)

(length horn-clauses)

(setq non-horn-clauses (remove-if #'horn-clause? clauses))

(print-clauses non-horn-clauses)
#|
(:OR A* (:NOT D*) C) ;; can we use (:NOT D*) == notD*?
(:OR A* (:NOT D*) B*)
(:OR A* (:NOT D*) B)
(:OR A* (:NOT D*) A)
(:OR A* (:NOT D*) U) ;; (OR (:NOT notA*) (:NOT D*) U)
(:OR A* (:NOT D*) C*)
(:OR A* (:NOT D*) D)
|#

(defun take-complement (n)
  (cdr (assoc n *complements*)))

(defun make-positive-literal (l)
  (ecase (cdr l)
    (:TRUE (translate-node (car l)))
    (:FALSE (take-complement (translate-node (car l))))))

(defun make-negative-literal (l)
  (ecase (cdr l)
    (:TRUE (take-complement (translate-node (car l))))
    (:FALSE (translate-node (car l)))))

(defun add-non-horn-justification (c)
  (let ((cs (PLTMS::clause-literals c)))
    (justify-node
     'JNH
     (make-positive-literal (car (last cs)))
     (mapcar #'make-negative-literal (butlast cs)))))

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
(mapcar #'add-non-horn-justification non-horn-clauses)

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
<A*,{{A*}}>
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

(setq *ps*
      (list (cons u 0.6)
            (cons nu 0.4)
            (cons a* 0.7)
            (cons na* 0.3)))
;; ...
(prob-nodes *atms* *ps*)
#|
<The contradiction,0.00>
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
<U,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<C,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<A,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<B,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<D,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<C*,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
<A*,0.70:{0.70:{A*}}>
<B*,0.60:{{D*,notA*}{B*}{C*}{D}{B}{A}{C}0.60:{U}}>
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
