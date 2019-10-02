(defvar *puzzle*
  '#(
     #(4 0 0 0 0 0 8 0 5)
     #(0 3 0 0 0 0 0 0 0)
     #(0 0 0 7 0 0 0 0 0)
     #(0 2 0 0 0 0 0 6 0)
     #(0 0 0 0 8 0 4 0 0)
     #(0 0 0 0 1 0 0 0 0)
     #(0 0 0 6 0 3 0 7 0)
     #(5 0 0 2 0 0 0 0 0)
     #(1 0 4 0 0 0 0 0 0)
     ))

(defvar *easy-puzzle*
    '#(
       #(0 0 3 0 2 0 6 0 0)
       #(9 0 0 3 0 5 0 0 1)
       #(0 0 1 8 0 6 4 0 0)
       #(0 0 8 1 0 2 9 0 0)
       #(7 0 0 0 0 0 0 0 8)
       #(0 0 6 7 0 8 2 0 0)
       #(0 0 2 6 0 9 5 0 0)
       #(8 0 0 2 0 3 0 0 9)
       #(0 0 5 0 1 0 3 0 0)
       ))

(defun name (i j)
  (format nil "c~A~A" i j))

(defun create-puzzle (puzzle)
  (setq *atms* (create-atms "sudoku" :debugging t))
  (apply #'concatenate 'list
   (loop for i from 1 to 9
   collect
   (loop for j from 1 to 9
   collect
   (let ((x (elt (elt puzzle (- i 1)) (- j 1))))
   (if (= x 0)
       (loop for v from 1 to 9
           collect (tms-create-node *atms* (list (name i j) v) :assumptionp t))
     (list (tms-create-node *atms* (list (name i j) x)))))))))

(defun rows ()
  (loop for i from 1 to 9 collect
  (loop for j from 1 to 9 collect
  (name i j))))

(defun cols ()
  (loop for i from 1 to 9 collect
  (loop for j from 1 to 9 collect
  (name j i))))

(defun units ()
  (apply #'concatenate 'list
  (loop for m from 0 to 2 collect
  (loop for n from 0 to 2 collect
  (apply #'concatenate 'list
  (loop for i from 0 to 2 collect
  (loop for j from 0 to 2 collect
  (name (+ i (* m 3) 1) (+ j (* n 3) 1)))))))))

(defun add-cell-constraints (reason cs)
  (let ((get #'(lambda (x v) (let ((d (list (elt cs (- x 1)) v)))
                 (find-if #'(lambda (n) (equal d (tms-node-datum n))) (atms-nodes *atms*))))))
    (loop for i from 1 to 9 do
    (loop for j from 1 to 9
     when (not (= i j)) do
    (loop for v from 1 to 9 do
          (let ((a (funcall get i v)) (b (funcall get j v)))
            (when (and a b)
              (nogood-nodes reason (list a b)))))))))

(defun add-unit-constraints (name css)
  (loop for cs in css do
      (add-cell-constraints name cs)))

(defun add-all-constraints ()
  (add-unit-constraints 'CONFLICT-ROW (rows))
  (add-unit-constraints 'CONFLICT-COL (cols))
  (add-unit-constraints 'CONFLICT-UNT (units)))

(defun prune (vss env)
  (sort
   (loop for vs in vss collect
   (loop for v in vs
       when (node-consistent-with? v env) collect v))
   #'< :key #'length))

(defun interpretations (atms choice-sets)
  (if (atms-debugging atms)
   (format *trace-output*
	   "~% Constructing interpretations..."))
  (setq *solutions* nil)
  (get-depth-solutions1 (atms-empty-env atms) choice-sets)
  (setq *solutions* (delete nil *solutions* :TEST #'eq))
  (unless *solutions*
    (if choice-sets (return-from interpretations nil)
      (setq *solutions* (list (atms-empty-env atms))))))

(defun get-depth-solutions1 (solution choice-sets &aux new-solution)
  (cond ((null choice-sets)
	   (push solution *solutions*))
	((env-nogood? solution)) ;something died.
	(t (dolist (choice (car choice-sets))
	     (setq new-solution (cons-env choice solution))
	     (unless (env-nogood? new-solution)
	       (get-depth-solutions1
          new-solution
          (prune (cdr choice-sets) new-solution)))))))

(defun show-solution (env vars &aux counter)
  (setq counter 0)
  (loop for ns in vars do
        (setq counter (+ 1 counter))
        (loop for n in ns
            when (in-node? n env) do
            (format t "~A " (cadr (tms-node-datum n))))
        (when (= counter 9)
          (format t "~%")
          (setq counter 0))))

(defvar *vars*)
(defun solve-sudoku (puzzle)
  (setq *vars* (create-puzzle puzzle))
  (loop for vs in *vars*
      when (= (length vs) 1) do
        (justify-node ':PUZZLE (car vs) nil))
  (add-all-constraints)
  (interpretations *atms* (prune (remove-if #'(lambda (vs) (= (length vs) 1)) *vars*) (atms-empty-env *atms*)))
  (assert (= (length *solutions*) 1))
  (show-solution (car *solutions*) *vars*))

;;(solve-sudoku *easy-puzzle*)
;;(solve-sudoku *puzzle*)