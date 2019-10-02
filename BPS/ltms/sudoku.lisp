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

(defun name (v i j)
  (list 'C v i j (calc-unit i j)))

(defun create-puzzle (puzzle)
  (apply #'concatenate 'list
   (loop for i from 1 to 9 collect
   (loop for j from 1 to 9 collect
   (let ((x (elt (elt puzzle (- i 1)) (- j 1))))
   (if (= x 0)
       (loop for v from 1 to 9 collect (name v i j))
     (list (name x i j))))))))

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

(defun namec (v ij)
  (name v (car ij) (cdr ij)))

(defun add-constraints (reason css)
  (loop for cs in css do
  (loop for v from 1 to 9 do
  (loop for b in cs do
  (loop for a in cs
      when (not (equal a b)) do
        (assert! `(:NOT (:AND ,(namec v a) ,(namec v b))) `(:CONFLICT ,reason)))))))

(defun add-constraints-trigger (reason css)
  (loop for cs in css do
  (loop for a in cs do
        (eval `(rule ((:TRUE ,(namec '?v a)))
                     (rassert! (:IMPLIES ,(namec '?v a)
                                         (:AND ,@(loop for b in cs when (not (equal a b)) collect `(:NOT ,(namec '?v b)))))
                               (:CONFLICT ,reason)))))))

(defun sudoku-constraints ()
  (add-constraints :ROW (rows))
  (add-constraints :COL (cols))
  (add-constraints :UNIT (units)))

(defun sudoku-constraints-trigger ()
  (add-constraints-trigger :ROW (rows))
  (add-constraints-trigger :COL (cols))
  (add-constraints-trigger :UNIT (units)))

(defun sudoku-constraints-alt ()
  (eval '(rule ((:TRUE (C ?v1 ?r1 ?c1 ?u1) :VAR ?a)
                (:TRUE (C ?v2 ?r2 ?c2 ?u2) :VAR ?b
                 :TEST (and (= ?v1 ?v2)
                        (or (= ?r1 ?r2) (= ?c1 ?c2) (= ?u1 ?u2))
                        (not (and (= ?r1 ?r2) (= ?c1 ?c2) (= ?u1 ?u2))))))
          (rassert! (:NOT (:AND ?a ?b)) :CONFLICT))))

(defun show-solution ()
  (let ((r (remove-if-not #'true? (fetch-global '(C ?v ?i ?j ?u)))))
  (format t "~%~A~%" r)

  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (loop for v from 1 to 9
      when (member (name v i j) r :TEST #'equal) do
      (format t "~D " v)))
  (format t "~%"))

  (break)
  ))


(defun solve-sudoku (puzzle &key (debugging t))
  (setq *debug-dds* debugging)
  (in-LTRE (create-ltre "Sudoku" :DEBUGGING debugging))
  ;;(sudoku-constraints)
  (sudoku-constraints-trigger)
  ;;(sudoku-constraints-alt)
  (dd-search (assert-choices! (create-puzzle puzzle)) '(show-solution)))

;;(solve-sudoku *easy-puzzle* :debugging t)
;;(solve-sudoku *puzzle* :debugging t)
