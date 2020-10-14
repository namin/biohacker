;; baby steps towards a
;; port of https://github.com/jtcbrule/whittemore
;; to do causal identification

(defun kget (k m)
  (cdr (assoc k m)))

(defun aget (k m)
  (cadr (assoc k m)))

(defun mget (m k)
  (cdr (assoc k m)))

(defun keys (m)
  (mapcar #'car m))

(defun lset (xs)
  xs)

(defun unions (xss)
  (if (null xss)
      '()
      (if (null (cdr xss))
          (car xss)
          (union (car xss) (unions (cdr xss))))))

(defun parents (m x)
  (unions (mapcan #'(lambda (node) (mget (kget :pa m) node)) x)))

(defun ancestors-iter (m frontier visited)
  (if (null frontier)
      visited
      (ancestors-iter m (parents m frontier) (union visited frontier))))
(defun ancestors (m x)
  (ancestors-iter m (lset x) '()))

(defun vertices (m)
  (lset (keys (kget :pa m))))

(defun pairs-of (coll)
  (let ((res '()))
    (dolist (i coll res)
      (dolist (j coll)
        (when (not (eq i j))
          (push (list i j) res))))))

(defun pairs-of (coll)
  (let ((res '()))
    (dotimes (i (length coll) res)
      (dotimes (j (length coll))
        (when (< i j)
          (push (list (nth i coll) (nth j coll)) res))))))

(pairs-of '(a b c))

(defun model (dag &rest confounding)
  `((:pa ,@(mapcar #'(lambda (kv) (list (car kv) (lset (cadr kv)))) dag))
    (:bi ,@(unions (mapcar #'pairs-of confounding)))))

(setq *m* (model '((x ()) (z (x)) (y (z)))))
(setq *m2* (model '((x (z)) (y (x z)) (z ())) '(z y)))

(vertices *m*)

(parents *m* '(z))

(ancestors *m* '(z))

(ancestors *m* '(x))

(defun sum (sub p)
  ;; Returns \\sum_{sub} p
  (if (null sub)
      p
      `((:sub ,sub) (:sum ,p))))

(defun product (exprs)
  (if (= 1 (length exprs))
      (car exprs)
      `((:prod ,exprs))))

(defun hedge (g s)
  `((:hedge ,g) (:s ,s)))

(defun take-while (pred list)
  (loop for x in list
        while (funcall pred x)
        collect x))

(defun predecessors (ordering v)
  (let ((before (take-while #'(lambda (x) (not (equal x v))) ordering)))
    (if (= (length before) (length ordering))
        (error "Not in ordering")
        before)))

(defun free-vars (form)
  (cond
    ((aget :given form)
     (union (aget :given form) (aget :p form)))
    ((aget :p form)
     (aget :p form))
    ((aget :prod form)
     (unions (mapcar #'free-vars (aget :prod form))))
    ((aget :sum form)
     (set-difference (free-vars (aget :sum form)) (aget :sub form)))
    ((aget :numer form)
     (union (free-vars (:numer form)) (free-vars (:denom form))))
    (t
     (error "free precondition failed"))))

(defun given-pi (p vi pi)
  (let* ((pred (predecessors pi vi))
         (unbound (set-difference (free-vars p) (cons vi pred)))
         (numer (sum unbound p))
         (denom (sum (list vi) numer)))
    (list (list :numer numer)
          (list :denom denom))))

(defun subedges (pa x)
  (let ((res '()))
    (dolist (kv pa res)
      (let ((k (car kv))
            (v (cadr kv)))
        (when (member k x)
          (push (list k (intersection v x)) res))))))

(subedges (kget :pa *m*) '(x z))

(defun graph-cut (g x)
  (mapcar #'(lambda (e) (if (member (car e) x) (list (car e) '()) e)) g))

(defun pair-cut (pairs x)
  (remove-if #'(lambda (p) (or (member (car p) x) (member (cadr p) x))) pairs))

(defun cut-incoming (m x)
  (let ((pa (graph-cut (kget :pa m) x))
        (bi (pair-cut (:bi m) x)))
    (list (cons :pa pa)
          (cons :bi bi))))

(defun subgraph (m x)
  (let* ((to-remove (set-difference (vertices m) x))
         (bi (pair-cut (kget :bi m) to-remove))
         (pa (subedges (kget :pa m) x)))
    (list (cons :pa pa)
          (cons :bi bi))))

(subgraph *m* '(x z))
(subgraph *m2* '(x z))
(subgraph *m2* '(z y))

(defun adjacent (pairs node)
  (remove node (unions (remove-if-not #'(lambda (p) (member node p)) pairs))))

(adjacent (kget :bi *m2*) 'z)

(defun connected-component-iter (pairs frontier visited)
  (if (null frontier)
      visited
      (let ((current (car frontier)))
        (if (member current visited)
            (connected-component-iter pairs (cdr frontier) visited)
            (connected-component-iter
             pairs
             (union (cdr frontier) (adjacent pairs current))
             (cons current visited))))))
(defun connected-component (pairs node)
  (connected-component-iter pairs (list node) '()))

(connected-component (kget :bi *m2*) 'z)

(defun c-components-iter (m nodes components)
  (if (null nodes)
      components
      (let* ((current-node (car nodes))
             (current-component (connected-component (kget :bi m) current-node)))
        (c-components-iter
         m
         (set-difference nodes current-component)
         (cons current-component components)))))
(defun c-components (m)
  (c-components-iter m (vertices m) '()))

(c-components *m*)
(c-components *m2*)

(defun kahn-cut (g x)
  (mapcar
   #'(lambda (kv) (list (car kv) (set-difference (cadr kv) x)))
   (remove-if #'(lambda (kv) (member (car kv) x)) g)))

(defun sources (g)
  (remove-if-not #'(lambda (k) (null (car (mget g k)))) (keys g)))

(defun topological-sort-iter (remaining result)
  (if (null remaining)
      result
      (let ((frontier (sources remaining)))
        (if (null frontier)
            (error "Not a dag")
            (topological-sort-iter
             (kahn-cut remaining frontier)
             (append result frontier))))))
(defun topological-sort (m)
  (topological-sort-iter (kget :pa m) '()))

(topological-sort *m*)
(topological-sort *m2*)

(defun find-superset (coll s)
  (car (remove-if-not #'(lambda (x) (subsetp s x)) coll)))

(find-superset '((1 2 3) (2 4) (1 2 3 4 5)) '(1 2 3 4))

(defun id (y x p g)
  (let ((v (vertices g)))
  ;; line 1
  (if (null x)
      (sum (set-difference v y) p)
  ;; line 2
  (let ((ancestors-y (ancestors g y)))
  (if (not (null (set-difference v ancestors-y)))
      (id y
          (intersection x ancestors-y)
          (sum (difference v ancestors-y) p)
          (subgraph g ancestors-y))
  ;; line 3
  (let ((w (set-difference (set-difference v x) (ancestors (cut-incoming g x) y))))
  (if (not (null w))
      (id y (union x w) p g)
  ;; line 4
  (let ((c-x (c-components (subgraph g (set-difference v x)))))
  (if (> (length c-x) 1)
      (sum (set-difference v (union y x))
           (product (mapcar #'(lambda (si) (id si (set-difference v si) p g)) c-x)))
  ;; line 5
  (let ((s (first c-x))
        (c (c-components g)))
  (if (equal c (list v))
      (hedge g s)
  ;; line 6
  (let ((pi (topological-sort g)))
  (if (member s c)
      (sum (set-difference s y)
           (product #'(lambda (vi) (given-pi p vi pi))))
   ;; line 7
   (let* ((s-prime (find-superset c s))
          (p-prime (product (mapcar #'(lambda (vi) (given-pi p vi pi)) s-prime))))
   (if (not (null s-prime))
       (id y
           (intersection x s-prime)
           p-prime
           (subgraph g s-prime))
     (error "ID precondition failed"))))))))))))))))

(defun simplify-form (form)
  ;; TODO
  form)

(defun extract-hedges (form)
  (cond
    ((aget :hedge form)
     form)
    ((aget :sum form)
     (extract-hedges (aget :sum form)))
    ((aget :prod form)
     (unions (mapcar #'extract-edges (aget :prod form))))
    ((aget :numer form)
     (union
      (extract-hedges (aget :numer form))
      (extract-hedges (aget :denom form))))
    ((aget :p form)
     '())
    (t
     (error "Unsupported formula type"))))

(defun identify (model query)
  (let ((q (kget :form query)))
    (let* ((raw-form (id (aget :p q) (aget :do q) `((:p ,@(vertices model))) model))
           (form (simplify-form raw-form))
           (hedges (extract-hedges form)))
      (cond
        ((not (null hedges))
         (list :fail hedges))
        (t
         (list :formula form))))))

(setq *ident-a* (model '((y (x)) (x ()))))
(identify *ident-a* '((:q (y)) (:do (x))))
