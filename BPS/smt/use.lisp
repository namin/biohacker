(use-package :cl-smt-lib/cl-smt-lib)
(named-readtables:in-readtable :cl-smt-lib)
(defparameter smt (make-smt "z3" "-in" "-smt2"))

(write-to-smt smt
              (let ((range 8))
                #!`((set-option :produce-models true)
                    (set-logic QF_BV)

                    (define-fun hamming-weight ((bv (_ BitVec ,RANGE)))
                        (_ BitVec ,RANGE)
                      ,(REDUCE (LAMBDA (ACC N)
                                 `(bvadd ,ACC ((_ zero_extend ,(1- RANGE))
                                               ((_ extract ,N ,N) bv))))
                               (LOOP :FOR I :UPFROM 1 :BELOW (1- RANGE) :COLLECT I)
                               :INITIAL-VALUE
                               `((_ zero_extend ,(1- RANGE)) ((_ extract 0 0) bv))))
                    (declare-const example1 (_ BitVec ,RANGE))
                    (declare-const example2 (_ BitVec ,RANGE))
                    (assert (= (_ bv3 ,RANGE) (hamming-weight example1)))
                    (assert (= (_ bv3 ,RANGE) (hamming-weight example2)))
                    (assert (distinct example1 example2))
                    (check-sat)
                    (get-model))))

(read smt)

(read smt)

(write-to-smt t
              (let ((range 8))
                #!`((set-option :produce-models true)
                    (set-logic QF_BV)

                    (define-fun hamming-weight ((bv (_ BitVec ,RANGE)))
                        (_ BitVec ,RANGE)
                      ,(REDUCE (LAMBDA (ACC N)
                                 `(bvadd ,ACC ((_ zero_extend ,(1- RANGE))
                                               ((_ extract ,N ,N) bv))))
                               (LOOP :FOR I :UPFROM 1 :BELOW (1- RANGE) :COLLECT I)
                               :INITIAL-VALUE
                               `((_ zero_extend ,(1- RANGE)) ((_ extract 0 0) bv))))
                    (declare-const example1 (_ BitVec ,RANGE))
                    (declare-const example2 (_ BitVec ,RANGE))
                    (assert (= (_ bv3 ,RANGE) (hamming-weight example1)))
                    (assert (= (_ bv3 ,RANGE) (hamming-weight example2)))
                    (assert (distinct example1 example2))
                    (check-sat)
                    (get-model))))
