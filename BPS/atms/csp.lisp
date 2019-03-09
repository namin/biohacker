(defvar *atms* nil)

(defun add-var (x vs)
  (let ((ns (mapcar #'(lambda (v)
                        (tms-create-node *atms* (list x v) :assumptionp t))
                    vs)))
    (loop for a in ns
        do (loop for b in ns
               when (not (equal (tms-node-datum a) (tms-node-datum b)))
               do (nogood-nodes 'unique-value (list a b))))
    ns))

(defun add-con (xs ys allowed)
  (loop for a in xs
      do (loop for b in ys
             when (not (member (list (cadr (tms-node-datum a)) (cadr (tms-node-datum b))) allowed :test #'equal))
             do (nogood-nodes 'not-allowed (list a b)))))

(defun find-var (x vars)
  (find-if #'(lambda (line) (equal x (car (tms-node-datum (car line))))) vars))

(defun csp (var-defs con-defs)
  (setq *atms* (create-atms "csp" :debugging t))
  (let ((vars (mapcar #'(lambda (line) (add-var (car line) (cdr line))) var-defs)))
    (mapcar #'(lambda (line) (add-con (find-var (caar line) vars)
                                      (find-var (cadar line) vars)
                                      (cdr line)))
            con-defs)
    (let ((i (interpretations *atms* vars)))
      (mapcar #'print-env i)
      i)))

(csp '((x a b)
       (y e f)
       (z c d g))
     '(((x y) (b e) (b f))
       ((x z) (b c) (b d) (b g))
       ((y z) (e d) (f g))))

(csp '((n1 r g b)
       (n2 r g b)
       (n3 r g b))
     '(((n1 n2) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n2 n3) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n1 n3) (r g) (r b) (g r) (g b) (b r) (b g))))

(csp '((n1 r g b)
       (n2 r g b)
       (n3 r g b)
       (n4 r g b))
     '(((n1 n2) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n2 n3) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n1 n3) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n1 n4) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n2 n4) (r g) (r b) (g r) (g b) (b r) (b g))
       ((n3 n4) (r g) (r b) (g r) (g b) (b r) (b g))))
