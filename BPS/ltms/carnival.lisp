;; inspired by
;; https://github.com/saezlab/CARNIVAL

(defvar *node-labels*)

(defun new-carnival (title)
  (setq *ltms* (create-ltms (format nil "CARNIVAL ~A" title) :complete t))
  (setq *node-labels* '()))

(defmacro node (val n &key (measured? nil) (top? nil))
  (let ((label (ecase val (+ :TRUE) (- :FALSE))))
    `(let ((node (tms-create-node *ltms* ',n :ASSUMPTIONP t)))
       (push (cons ',n ',label) *node-labels*)
       ,(if top?
            `(enable-assumption node ',label)
            t))))

(defun edge-name (val src dst)
  (read-from-string (concatenate 'string (string src) (string val) (string dst))))

(defmacro edge (val src dst)
  (let ((edge-name (edge-name val src dst)))
    `(let ((edge (tms-create-node *ltms* ',edge-name :ASSUMPTIONP t)))
       (enable-assumption edge :TRUE)
       ,(if (ecase val (+ t) (- nil))
            `(compile-formula
              *ltms*
              '(:IMPLIES ,edge-name
                (:AND
                 (:IMPLIES ,src ,dst)
                 (:IMPLIES (:NOT ,src) (:NOT ,dst)))))
            `(compile-formula
              *ltms*
              '(:IMPLIES ,edge-name
                (:AND
                 (:IMPLIES ,src (:NOT ,dst))
                 (:IMPLIES (:NOT ,src) ,dst))))))))

(defun check-consistency (&aux c)
  (setq c t)
  (mapc #'(lambda (x)
            (let ((n (car x))
                  (v (cdr x)))
              (let ((node (find-node *ltms* n)))
                (unless (equal v (tms-node-label node))
                  (setq c nil)
                  (format t "~%Node ~A inconsistent." n)
                  (explain-node node)))))
        *node-labels*)
  c)
