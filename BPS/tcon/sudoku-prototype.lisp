(in-package :COMMON-LISP-USER)

(eval
 `(constraint
   sudoku-board
   ,(apply #'concatenate 'list
    (loop for i from 1 to 9 collect
    (loop for j from 1 to 9 collect
          `(,(name i j) cell))))

   (formulae
   ,@(apply #'concatenate 'list
     (loop for vss in (list (rows) (cols) (units)) collect
     (apply #'concatenate 'list
     (loop for vs in vss collect
     (apply #'concatenate 'list
     (loop for a in vs collect
     (loop for b in vs when (not (eq a b)) collect
           `(,a (,a ,b) (if (= ,a ,b) :LOSE :DISMISS))
     ))))))))))
