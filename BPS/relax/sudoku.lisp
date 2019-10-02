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

(defvar *net* nil)

(defun name (i j)
  (format nil "c~A~A" i j))

(defun create-puzzle (puzzle)
  (let ((net (create-network "sudoku")))
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (build-cell
   (name i j) net
   (let ((x (elt (elt puzzle (- i 1)) (- j 1))))
     (if (= 0 x)
         (loop for v from 1 to 9 collect v)
       (list x))))))
  net))

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

(defun add-cell-constraints (name net cells)
  (mapcar
   #'(lambda (cell1)
       (let ((cells2 (remove cell1 cells :TEST #'(lambda (x y) (funcall (network-name-test net) (cell-name x) (cell-name y))))))
         (add-constraint-cell
          cell1
          (build-constraint
           (concatenate 'string name (cell-name cell1)) net
           #'(lambda (constraint)
               (when (known? cell1)
                 (loop for cell2 in cells2
                     when (member (value cell1) (cell-value cell2) :TEST (network-equality-test net)) do
                       (queue-cell cell2 :EXCLUDE (value cell1) constraint))))))))
   cells))
  
(defun add-unit-constraints (name net css)
  (loop for cs in css do
  (add-cell-constraints
   name net
   (loop for c in cs collect (lookup-cell c net)))))

(defun add-all-constraints (net)
  (add-unit-constraints "row" net (rows))
  (add-unit-constraints "col" net (cols))
  (add-unit-constraints "unt" net (units)))

(defun sudoku-solve (puzzle)
  (let ((net (create-puzzle puzzle)))
    (add-all-constraints net)
    (clear-network net)
    (fire-constraints net)
    net))

(defun prune (cells)
  (sort cells #'<
        :key #'(lambda (cell) (length (cell-value cell)))))

(defun solution (net)
  (apply #'vector
  (loop for i from 1 to 9 collect
  (apply #'vector
  (loop for j from 1 to 9 collect
  (value (lookup-cell (name i j) net)))))))

(defun print-solution (solution)
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (format t "~D " (elt (elt solution (- i 1)) (- j 1))))
  (format t "~%")))

(defvar *solution* nil)
(defun say-sudoku-solution (net)
  (unless (determined? net)
    (error "Say-solution called with unsolved network ~A." (network-title net)))
  (format t "~% A solution for ~A:" (network-title net))
  (format t "~%")
  (setq *solution* (solution net))
  (print-solution *solution*)
  (break "Consistent solution"))

(defun solve-sudoku (puzzle)
  (setq *solution* nil)
  (setq *net* (sudoku-solve puzzle))
  (search-network *net* #'say-sudoku-solution #'say-contradiction #'prune)
  *solution*)

;;(solve-sudoku *puzzle*)
