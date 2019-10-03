#lang racket

(require "waltzer.rkt")

(define rows
  (for/list ([i (in-range 1 10)])
    (for/list ([j (in-range 1 10)])
      (cons i j))))
(define cols
  (for/list ([i (in-range 1 10)])
    (for/list ([j (in-range 1 10)])
      (cons j i))))
(define squares
  (for*/list ([m (in-range 3)] [n (in-range 3)])
    (for*/list ([i (in-range 3)] [j (in-range 3)])
      (cons (+ i (* n 3) 1) (+ j (* m 3) 1)))))

(define (name ij)
  (let ((i (car ij)) (j (cdr ij)))
    (string-append "c" (string-append (number->string i) (number->string j)))))
(define (model-assoc m)
  (map (lambda (x) (match x [(list _ lhs _ _ rhs) (cons lhs rhs)])) (cdr m)))
(define (model-get m ij) (cdr (assoc (name ij) m)))
(define (model->puzzle m)
  (for/vector ([i (in-range 1 10)])
    (for/vector ([j (in-range 1 10)])
      (model-get m (cons i j)))))
(define (print-puzzle puzzle)
  (printf "~a\n" "'#(")
  (for ([line puzzle])
    (printf "   ~a\n" line))
  (printf "  ~a\n" ")"))
(define (puzzle-index puzzle i j)
  (vector-ref (vector-ref puzzle (- i 1)) (- j 1)))

(define puzzle
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

(define (create-puzzle puzzle)
  (let ((net (create-network "sudoku")))
    (for ([i (in-range 1 10)])
      (for ([j (in-range 1 10)])
        (build-cell
         (name (cons i j)) net
         (let ((x (puzzle-index puzzle i j)))
           (if (= 0 x)
               (for/list ([v (in-range 1 10)]) v)
               (list x))))))
    net))

(define (add-cell-constraints unit-kind net cells)
  (for ([cell1 cells])
    (let ((cells2 (filter (lambda (cell2) (not (equal? (cell-name cell1) (cell-name cell2)))) cells))
          (con-name (string-append unit-kind (cell-name cell1))))
     (add-constraint-cell
      cell1
      (build-constraint
       con-name net
       (lambda (constraint)
         (when (known? cell1)
           (for ([cell2 cells2])
             (when (member (value cell1) (cell-value cell2))
               (queue-cell cell2 '#:exclude (value cell1) con-name))))))))))

(define (add-unit-constraints unit-kind net css)
  (for ([cs css])
    (add-cell-constraints
     unit-kind net
     (map (lambda (ij) (lookup-cell (name ij) net)) cs))))

(define (add-all-constraints net)
  (add-unit-constraints "row" net rows)
  (add-unit-constraints "col" net cols)
  (add-unit-constraints "unt" net squares))

(define (prune cells)
  (sort cells < #:key (lambda (cell) (length (cell-value cell)))))

(define (solution net)
  (for/vector ([i (in-range 1 10)])
    (for/vector ([j (in-range 1 10)])
      (value (lookup-cell (name (cons i j)) net)))))

(define (setup-sudoku puzzle)
  (let ((net (create-puzzle puzzle)))
    (add-all-constraints net)
    (clear-network net)
    (fire-constraints net)
    net))

(define (solve-sudoku puzzle)
  (let ((net (setup-sudoku puzzle)))
    (with-handlers ([(lambda (e) (equal? e "Consistent solution"))
                     (lambda (e)
                       (printf "\n\n")
                       (solution net))])
      (show-search net prune))))

(define (solve-sudoku-to-completion puzzle)
  (let ((net (setup-sudoku puzzle))
        (answer #f))
    (search-network
     net
     (lambda (net)
       (cond
         (answer
          (printf "\nOld solution:\n")
          (print-puzzle answer)
          (printf "\nNew solution:\n")
          (print-puzzle (solution net))
          (raise "Multiple solutions"))
         (else
          (set! answer (solution net))
          (printf "\nFound a solution:\n")
          (print-puzzle answer)
          (printf "\n\nNow checking for uniqueness..."))))
     say-contradiction
     prune)
    (printf "\n\n")
    answer))

;;(solve-sudoku puzzle)
;;(solve-sudoku-to-completion puzzle)
