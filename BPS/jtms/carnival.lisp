;; inspired by
;; https://github.com/saezlab/CARNIVAL

(defvar *node-labels*)
(defvar *nodes*)

(defun new-carnival (title)
  (setq *jtms* (create-jtms (format nil "CARNIVAL ~A" title)))
  (setq *node-labels* '())
  (setq *nodes* '()))

(defun get-node (n)
  (cdr (assoc n *nodes*)))

(defun find-node (tms datum)
  (car (member datum (jtms-nodes tms) :key #'tms-node-datum)))

(defun opposite-valence (val)
  (ecase val (+ '-) (- '+)))

(defun node-name (val n)
  (read-from-string (concatenate 'string (string val) (string n))))

(defmacro node (val n &key (measured? nil) (top? nil))
  `(progn
     (push (cons ',n ',val) *node-labels*)
     (let ((na (tms-create-node *jtms* ',(node-name val n)))
           (nb (tms-create-node *jtms* ',(node-name (opposite-valence val) n)))
           (n! (tms-create-node *jtms* ',(node-name '! n) :CONTRADICTORYP t)))
       (justify-node 'CONTRA n! (list na nb))
       (push (cons ',(node-name val n) na) *nodes*)
       (push (cons ',(node-name (opposite-valence val) n) nb) *nodes*)
       ,(if top? `(assume-node na) t))))

(defun edge-name (val src dst)
  (read-from-string (concatenate 'string (string src) (string val) (string dst))))

(defun maybe-flip-valence (edge-val node-val)
  (ecase edge-val (+ node-val) (- (opposite-valence node-val))))

(defmacro edge (val src dst)
  (let ((edge-name (edge-name val src dst)))
    `(progn
       ,@(mapcar
          #'(lambda (node-val)
              `(justify-node
                ',edge-name
                (get-node ',(node-name (maybe-flip-valence val node-val) dst))
                (list (get-node ',(node-name node-val src)))))
          '(+ -)))))

(defmacro edge (val src dst)
  (let ((edge-name (edge-name val src dst)))
    `(let ((edge (tms-create-node *jtms* ',edge-name :ASSUMPTIONP t)))
       (enable-assumption edge)
       ,@(mapcar
          #'(lambda (node-val)
              `(justify-node
                ',val
                (get-node ',(node-name (maybe-flip-valence val node-val) dst))
                (list edge (get-node ',(node-name node-val src)))))
          '(+ -)))))

(defun solve ()
  'ok)

(defun check-consistency (&aux c)
  (setq c t)
  (mapc #'(lambda (x)
            (let ((n (car x))
                  (v (cdr x)))
              (let ((node (get-node (node-name v n))))
                (unless (equal ':IN (tms-node-label node))
                  (setq c nil)
                  (format t "~%Node ~A inconsistent." n)
                  (what-node n)))))
        *node-labels*)
  c)

(defun what-node (n)
  (let ((n+ (get-node (node-name '+ n)))
        (n- (get-node (node-name '- n))))
    (if (equal ':IN (tms-node-label n+)) (why-node n+))
    (if (equal ':IN (tms-node-label n-)) (why-node n-))))
