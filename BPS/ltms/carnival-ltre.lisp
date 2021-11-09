;; inspired by
;; https://github.com/saezlab/CARNIVAL

(defvar *node-labels*)
(defvar *edge-labels*)

(defun new-carnival (title &key (debugging t))
  (setq *debug-dds* debugging)
  (in-LTRE (create-ltre (format nil "CARNIVAL ~A" title) :DEBUGGING debugging))
  (carnival-rules)
  (setq *node-labels* '())
  (setq *edge-labels* '()))

(defun choices ()
  (mapcar #'(lambda (e)
              (let ((src (caddr e))
                    (dst (cadddr e)))
                `((E + ,src ,dst) (E - ,src ,dst))))
          *edge-labels*))

(defmacro node (val n &key (measured? nil) (top? nil))
  `(progn
     (push (cons ',n ',val) *node-labels*)
     ,(if (or measured? top?)
          `(assert!
            ',(ecase val
                (+ `(V ,n))
                (- `(:NOT (V ,n))))
            :MEASURED)
        t)))

(defmacro edge (val src dst)
  `(progn
     (push '(edge ,val ,src ,dst) *edge-labels*)
     ;;(assert! '(E ,val ,src ,dst) :GIVEN)
     ))

(defun carnival-rules ()
  (eval `(rule ((:TRUE (E + ?src ?dst)))
               (rassert! (:IMPLIES (E + ?src ?dst)
                                   (:AND (:IMPLIES (V ?src) (V ?dst))
                                         (:IMPLIES (:NOT (V ?src)) (:NOT (V ?dst))))))))

  (eval `(rule ((:TRUE (E - ?src ?dst)))
               (rassert! (:IMPLIES (E - ?src ?dst)
                                   (:AND (:IMPLIES (V ?src) (:NOT (V ?dst)))
                                         (:IMPLIES (:NOT (V ?src)) (V ?dst))))))))

(defun check-consistency (&aux c)
  (setq c t)
  (mapc #'(lambda (x)
            (let ((n (car x))
                  (val (cdr x)))
              (unless (ecase val
                        (+ (true? `(V ,n)))
                        (- (false? `(V ,n))))
                (setq c nil)
                (format t "~%Node ~A inconsistent." n)
                (what-node n))))
        *node-labels*)
  c)

(defvar *solutions*)
(defvar *n-consistent-solutions*)
(defvar *n-inconsistent-solutions*)

(defun show-solution ()
  (let ((r (remove-if-not #'true? (fetch-global '(E ?val ?src ?dst)))))
    (format t "~%~A~%" r)
    (let ((c (check-consistency)))
      (if c
          (incf *n-consistent-solutions*)
          (incf *n-inconsistent-solutions*))
      (when c
        (push r *solutions*)
        ;;(break)
        ))))

(defun solve ()
  (setq *solutions* '())
  (setq *n-consistent-solutions* 0)
  (setq *n-inconsistent-solutions* 0)
  (dd-search (assert-choices! (choices)) '(show-solution))
  (assert-solution! (car *solutions*))
  )

(defun assert-solution! (solution)
  (mapc #'(lambda (fact) (assert! fact :SOLUTION)) solution))

(defun node-of (n)
  (let ((r (referent `(V ,n) nil)))
    (and r (datum-tms-node r))))

(defun what-node (n)
  (let ((node (node-of n)))
    (and node  (why-node node))))
