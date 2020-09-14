(bps-load-file (make-bps-path "jtms") "jtre" :action :compile)
(compile-jtre)
(bps-load-file (make-bps-path "jtms") "jtms" :action :compile)

(bps-load-file (make-bps-path "jtms") "jtms-ex" :action :compile)
(ex1)
(ex2)
(ex3)

(bps-load-file (make-bps-path "jtms") "jqueens" :action :compile)
(test-queens 1 8)

(setup-queens-puzzle 2 t)
(change-jtms (jtre-jtms *jtre*) :debugging t)
(assume! '(queen 1 1) '(try queen 1 1))
(assume! '(queen 2 1) '(try queen 2 1))
(run-rules)
(assume! '(queen 2 2) '(try queen 2 2))
(run-rules)

(bps-load-file (make-bps-path "jtms") "jtest" :action :compile)
(shakedown-jtre)

(bps-load-file (make-bps-path "jtms") "dds" :action :compile)
(test-dd-search)

(bps-load-file (make-bps-path "jtms") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle* :debugging t)

(bps-load-file (make-bps-path "jtms") "match" :action :compile)
(bps-load-file (make-bps-path "jtms") "simplify" :action :compile)
(bps-load-file (make-bps-path "jtms") "jsaint" :action :compile)
(try-jsaint '(integrate (integral x x)))
(try-jsaint problem2)
(explain-result)
(try-jsaint '(integrate (integral (+ (* 3 x) (cos (* 1.1 x))) x)))
(explain-result)
