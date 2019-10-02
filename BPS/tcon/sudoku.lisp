(in-package :COMMON-LISP-USER)

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

(defun as-symbol (s)
  (let ((x (intern s)))
    (import x)
    x))

(defun name (i j)
  (as-symbol (format nil "C~A~A" i j)))

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

(defun lookup-cell (i j)
  ;;(eval `(>> ,(name i j) s))
  (nested-lookup (list (name i j)) 's))

(defun set-puzzle (puzzle)
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (let ((x (elt (elt puzzle (- i 1)) (- j 1))))
    (unless (= x 0)
      (set-parameter (lookup-cell i j) x))))))

(defun update-min-unknowns (&optional us0)
  (let ((us (or us0
                (mapcar
                 #'(lambda (x)
                     (cons (cdr x)
                           (loop for v from 1 to 9 collect v)))
                 (remove-if
                  #'(lambda (x) (known? (cdr x)))
                  (constraint-parts (lookup-global 's *tcon*))))))
        (min 9))
    (sort
    (loop for u in us collect
          (let ((c (car u)))
            (let ((vs '())
                  (ns '()))
              (loop for v in (cdr u) when (<= (length vs) min) do
                    (let ((contra nil))
                      (with-contradiction-handler
                          *tcon* #'(lambda (cell newval newsetter tcon)
                                     (setq contra t))
                          (set-parameter c v)
                          (if contra (push v ns) (push v vs))
                          (forget-parameter c))))
              (if (> (length vs) min) (cons c (set-difference (cdr u) ns))
                (progn
                  (when (< (length vs) min) (setq min (length vs)))
                  (cons c vs))))))
    #'< :key #'length)))

(defun sudoku-solver (&optional us0)
  (let ((us (if (and (not (null us0)) (null (cdr (cdr (car us0)))))
                us0
              (update-min-unknowns us0))))
    (format t "In queue: ~D~%" (length us))
    (if (null us)
        t
      (let ((u (car us)))
        (format t "Considering ~A in ~A~%" (car u) (cdr u))
        (let ((c (car u))
              (vs (cdr u)))
          (loop for v in vs do
                (let ((contra nil))
                  (with-contradiction-handler
                      *tcon* #'(lambda (cell newval newsetter tcon)
                                 (setq contra t))
                      (set-parameter c v))
                  (if (and (not contra) (sudoku-solver (cdr us)))
                      (return-from sudoku-solver t)
                    (forget-parameter c))))
          nil)))))

(defun show-solution ()
  (loop for i from 1 to 9 do
  (loop for j from 1 to 9 do
  (let ((c (lookup-cell i j)))
    (assert (known? c))
    (format t "~D " (cell-value c))))
  (format t "~%")))

(defun solve-sudoku (puzzle)
  (create-tcon
   "Sudoku"
   :prototype-file (make-bps-source-file-name (make-bps-path "tcon") "sudoku-prototype"))

  (create 's 'sudoku-board)

  (set-puzzle puzzle)

  (sudoku-solver)

  (show-solution))

#|
(solve-sudoku *easy-puzzle*)
(solve-sudoku *puzzle*)
|#