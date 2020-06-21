(bps-load-file (make-bps-path "jtms") "jtre" :action :compile)
(compile-jtre)
(bps-load-file (make-bps-path "jtms") "jtms" :action :compile)

(bps-load-file (make-bps-path "jtms") "jqueens" :action :compile)
(test-queens 1 8)

(bps-load-file (make-bps-path "jtms") "jtest" :action :compile)
(shakedown-jtre)

(bps-load-file (make-bps-path "jtms") "dds" :action :compile)
(test-dd-search)

(bps-load-file (make-bps-path "jtms") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle* :debugging t)

(bps-load-file (make-bps-path "jtms") "match" :action :compile)
(bps-load-file (make-bps-path "jtms") "simplify" :action :compile)
(bps-load-file (make-bps-path "jtms") "jsaint" :action :compile)
(try-jsaint problem2)
(explain-result)
(try-jsaint '(integrate (integral (+ (* 3 x) (cosh x)) x)))
(explain-result)
