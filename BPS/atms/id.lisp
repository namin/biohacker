;; baby steps towards a
;; port of https://github.com/jtcbrule/whittemore
;; to do causal identification

(defun kget (k m)
  (cdr (assoc k m)))

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
      `(:sub ,sub :sum p)))

(defun subedges (pa x)
  (let ((res '()))
    (dolist (kv pa res)
      (let ((k (car kv))
            (v (cadr kv)))
        (when (member k x)
          (push (list k (intersection v x)) res))))))

(subedges (kget :pa *m*) '(x z))

(defun pair-cut (pairs x)
  (remove-if #'(lambda (p) (or (member (car p) x) (member (cadr p) x))) pairs))

(defun subgraph (m x)
  (let* ((to-remove (set-difference (vertices m) x))
         (bi (pair-cut (kget :bi m) to-remove))
         (pa (subedges (kget :pa m) x)))
    (list (cons :pa pa)
          (cons :bi bi))))

(subgraph *m* '(x z))
(subgraph *m2* '(x z))
(subgraph *m2* '(z y))

(defun id (y x p g)
  (let ((v (vertices g)))
    (if (null x)
        (sum (set-difference v y) p)
        (let ((ancestors-y (ancestors g y)))
          (if (not (null (set-difference v ancestors-y)))
              (id y
                  (intersection x ancestors-y)
                  (sum (difference v ancestors-y) p)
                  (subgraph g ancestors-y))
              'TODO)))))

(defun identify (model query)
  (let ((q (kget :form query)))
    (let ((raw-form (id (getk :p q) (getk :do q) `((:p ,@(vertices model))) model))
          ;; ...
          )
      'TODO)))

