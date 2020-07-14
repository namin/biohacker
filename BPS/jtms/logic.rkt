 #lang racket
 (provide (all-defined-out))
 (struct interval
         (s p)
         #:transparent)

 ;;;Belief Manipulations;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (noti i1)
   (interval (interval-p i1)
             (interval-s i1)))

 (define (sumlst lst)
   (let ((x 0))
     (map (lambda (y)
            (set! x
                  (+ y x)))
          lst)
     x))

 (define (maxlst lst)
   (cond
    ((empty? lst) -1) ;;assuming lst has non-negative numbers only
    (else (let ((temp_max (maxlst (rest lst))))
            (max (car lst)
                 temp_max)))))

 (define (andi intervals)
   (interval (max 0
                  (+ (sumlst (for/list ((i1 intervals))
                                       (interval-s i1)))
                     (- 1
                        (length intervals))))
             (maxlst (for/list ((i1 intervals))
                               (interval-p i1)))))


 (define (ori . intervals)
   (interval (maxlst (for/list ((i1 intervals))
                               (interval-s i1)))
             (max 0
                  (+ (sumlst (for/list ((i1 intervals))
                                       (interval-p i1)))
                     (- 1
                        (length intervals))))))

 (define (impli i1 s) ;; antecedent i1, support for implication s
   (interval (* (interval-s i1) (interval-s s)) (* (interval-s i1) (interval-p s))))
