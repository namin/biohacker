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

