(in-package :COMMON-LISP-USER)

(contradiction 'Conflict *jtre*)

 (rule ((:IN (C ?v1 ?r1 ?c1 ?u1) :VAR ?a)
	(:IN (C ?v2 ?r2 ?c2 ?u2) :VAR ?b
	       :TEST (and (= ?v1 ?v2)
			  (or (= ?r1 ?r2) (= ?c1 ?c2) (= ?u1 ?u2))
			  (not (and (= ?r1 ?r2) (= ?c1 ?c2) (= ?u1 ?u2))))))
       (rassert! Conflict (Death ?a ?b)))
