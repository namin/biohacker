(bps-load-file (make-bps-path "relax") "waltzer" :action :compile)

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
    (loop
        for i from 1 to 9
        do (loop
               for j from 1 to 9
               do (build-cell (name i j) net
                              (let ((x (elt (elt puzzle (- i 1)) (- j 1))))
                                (if (= 0 x)
                                    (loop for v from 1 to 9 collect v)
                                  (list x))))))
    net))

(defun rows ()
  (loop
      for i from 1 to 9
      collect (loop for j from 1 to 9
                  collect (name i j))))

(defun cols ()
  (loop
      for i from 1 to 9
      collect (loop for j from 1 to 9
                  collect (name j i))))

(defun units ()
    (apply #'concatenate 'list
     (loop for m from 0 to 2
         collect (loop for n from 0 to 2
                     collect (apply #'concatenate 'list
                                    (loop for i from 0 to 2
                                        collect (loop for j from 0 to 2
                                                    collect (name (+ i (* m 3) 1) (+ j (* n 3) 1)))))))))

(defun add-constraint (name net cells)
  (let ((constraint (build-constraint name net
                                      #'(lambda (constraint)
                                          (loop for cell1 in cells
                                              when (known? cell1)
                                              do (loop for cell2 in cells
                                              when (and (not (funcall (network-name-test net) (cell-name cell1) (cell-name cell2)))
                                                        (member (value cell1) (cell-value cell2) :TEST (network-equality-test net)))
                                              do (queue-cell cell2 :EXCLUDE (value cell1) constraint)))))))
    (loop for cell in cells
        do (add-constraint-cell cell constraint))
    constraint))
  
(defun add-unit-constraints (name net css)
  (loop
      for cs in css
      for i from 1 to 9
      do (add-constraint (format nil "~A~A" name i) net
                         (mapcar #'(lambda (c) (lookup-cell c net)) cs))))

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

(setq *net* (sudoku-solve *puzzle*))
;; warning: this is slow!
(show-search *net*)

(defun solution (net)
  (apply #'vector
         (loop for i from 1 to 9
             collect (apply #'vector
                            (loop for j from 1 to 9
                                collect (value (lookup-cell (name i j) net)))))))

(solution *net*)