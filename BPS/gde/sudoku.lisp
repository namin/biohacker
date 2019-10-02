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

(defun sudoku-cell-name (ij)
  (intern (format nil "C~A~A" (car ij) (cdr ij))))

(defun unique-car (xs)
  (assert (= (length xs) 1))
  (car xs))

(defun lookup-cell (ij)
  (let ((x (sudoku-cell-name ij)))
    (unique-car
     (remove-if-not #'(lambda (cell) (eq x (cell-name cell)))
                    (atcon-cells *atcon*)))))

(defun calc-unit (i j)
  (let* ((x (- i 1))
         (y (- j 1))
         (m (floor x 3))
         (n (floor y 3)))
    (+ 1 n (* m 3))))

(defun calc-unit-check ()
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
      (format t "~D " (calc-unit i j)))
  (format t "~%")))

(defun rows ()
  (loop for i from 1 to 9 collect
  (loop for j from 1 to 9 collect
        (cons i j))))

(defun cols ()
  (loop for i from 1 to 9 collect
  (loop for j from 1 to 9 collect
      (cons j i))))

(defun units ()
  (apply #'concatenate 'list
  (loop for m from 0 to 2 collect
  (loop for n from 0 to 2 collect
  (apply #'concatenate 'list
  (loop for i from 0 to 2 collect
  (loop for j from 0 to 2 collect
  (cons (+ i (* m 3) 1) (+ j (* n 3) 1)))))))))

(defun sudoku-create-cells ()
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (create (sudoku-cell-name (cons i j)) 'cell))))

(defun puzzle-parameters (puzzle)
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (let ((x (elt (elt puzzle (- i 1)) (- j 1)))
        (c (lookup-cell (cons i j))))
    (if (= x 0)
        (loop for v from 1 to 9 do
              (assume-parameter c v))
      (set-parameter c x))))))

(defun add-constraints (reason css)
  (loop for cs in css do
  (loop for v from 1 to 9 do
  (loop for a in cs do
  (loop for b in cs
      when (not (equal a b)) do
        (nogood-nodes `(:CONFLICT ,reason) (list (lookup-node (lookup-cell a) v)
                                                 (lookup-node (lookup-cell b) v))))))))

(defun sudoku-constraints ()
  (add-constraints :ROW (rows))
  (add-constraints :COL (cols))
  (add-constraints :UNIT (units)))

(defun in-nodes (c &optional env)
  (remove-if-not #'(lambda (node) (in-node? node env)) (cell-nodes c)))

(defun show-partial-solution (&optional env &aux complete)
  (setq complete t)
  (loop for i from 1 to 9 do
        (loop for j from 1 to 9 do
              (let ((ns (in-nodes (lookup-cell (cons i j)) env)))
                (if (= 1 (length ns))
                    (format t "~D " (value-datum (tms-node-datum (car ns))))
                  (progn
                    (setq complete nil)
                    (format t ". ")))))
        (format t "~%"))
  complete)

(defun show-solution (&optional env)
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
        (format t "~D " (value-datum (tms-node-datum (unique-car (in-nodes (lookup-cell (cons i j)) env))))))
  (format t "~%")))

(defun create-choice-sets ()
  (loop for c in (atcon-cells *atcon*) do
  (let ((nodes (in-nodes c)))
    (if (or (not (= (length nodes) 1)) (and (tms-node-assumption? (car nodes)) (not (true-node? (car nodes)))))
        (eval
         `(disjunction
           ,@(loop for n in nodes collect
                   (list c (value-datum (tms-node-datum n))))))))))

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

(defun fire-elim-top (&aux op)
  (setq op nil)
  (loop for c in (atcon-cells *atcon*) do
  (let ((nodes (in-nodes c)))
    (when (and (= 1 (length nodes)) (tms-node-assumption? (car nodes)) (not (true-node? (car nodes))))
      (setq op t)
      (set-parameter c (value-datum (tms-node-datum (car nodes)))))))
  op)

(defun all-units ()
  (loop for vss in (list (rows) (cols) (units)) collect
  (loop for vs in vss collect
        (mapcar #'lookup-cell vs))))
(defvar *units*)

(defun fire-elim (d env)
  (loop for c in (atcon-cells *atcon*) do
  (let ((nodes (remove-if-not #'(lambda (node) (node-consistent-with? node env)) (cell-nodes c))))
    (cond ((and
            (= 1 (length nodes))
            (tms-node-assumption? (car nodes))
            (not (in-node? (car nodes) env)))
           (format t "Adding node ~A~%" (tms-node-datum (car nodes)))
           (setq env (cons-env (car nodes) env)))
          ((null nodes)
           (format t "New nogood stuck on ~A.~%" c)
           (return-from fire-elim nil)))))
  (loop for vss in *units* do
  (loop for vs in vss do
        (let ((places (delete
                       nil
                       (mapcar #'(lambda (c)
                                   (let ((n (lookup-node c d)))
                                     (and (node-consistent-with? n env) n)))
                               vs)
                       :TEST 'eq)))
          (cond ((null places)
                 (format t "New nogood stuck on ~A.~%" d)
                 (return-from fire-elim nil))
                ((and
                  (null (cdr places))
                  (tms-node-assumption? (car places))
                  (not (in-node? (car places) env)))
                 (format t "Adding place ~A~%" (tms-node-datum (car places)))
                 (setq env (cons-env (car places) env)))))))
  env)

(defun until-fixedpoint (f a &optional (n 0))
  (format t "fix: ~D~%" n)
  (let ((r (funcall f a)))
    (if (or (not r) (eq r a)) r
        (until-fixedpoint f r (+ n 1)))))

(defun until-noop (f &optional (n 0))
  (format t "fix: ~D~%" n)
  (let ((r (funcall f)))
    (if (not r) r
      (until-noop f (+ n 1)))))

(defun get-depth-solutions1 (solution choice-sets &aux new-solution)
  (cond ((null choice-sets)
	   (push solution *solutions*))
	((env-nogood? solution)) ;something died.
	(t (dolist (choice (car choice-sets))
	     (setq new-solution (cons-env choice solution))
	     (unless (env-nogood? new-solution)
         (let ((fixed-solution (until-fixedpoint #'(lambda (env) (fire-elim (value-datum (tms-node-datum choice)) env)) new-solution)))
           (if fixed-solution
               (get-depth-solutions1 fixed-solution
                                     (prune (cdr choice-sets) fixed-solution))
             ;;(new-nogood (atcon-atms *atcon*) new-solution ':STUCK)
             nil
             )))))))

(defun solve-sudoku (puzzle)
  (setq *atcon* (create-atcon "sudoku" :debugging t))
  ;;(change-atms (atcon-atms *atcon*) :debugging t)
  (format t "Creating cells...~%")
  (sudoku-create-cells)
  (format t "Adding sudoku constraints...~%")
  (sudoku-constraints)
  (format t "Adding puzzle constraints...~%")
  (puzzle-parameters puzzle)
  ;;(format t "Proceeding by elimination...~%")
  ;;(until-noop #'fire-elim-top)
  ;;(show-partial-solution)
  (format t "Finding interpretations...~%")
  (create-choice-sets)
  (setq *units* (all-units))
  (interpretations (atcon-atms *atcon*)
                   (sort (atcon-disjunctions *atcon*) #'< :key #'length))
  (show-solution (unique-car *solutions*)))

;;(solve-sudoku *easy-puzzle*)
;;(solve-sudoku *puzzle*)
