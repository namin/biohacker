;; inspired by
;; https://github.com/saezlab/CARNIVAL

(defmacro node (val n &key (measured? nil) (top? nil))
  `(let ((node (tms-create-node *ltms* ',n :ASSUMPTIONP t)))
     ,(if top?
          `(enable-assumption
            node
            (ecase ',val
              (+ :TRUE)
              (- :FALSE)))
          t)))

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
