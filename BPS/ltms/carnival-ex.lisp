(progn
  (new-carnival "toy example")
  (node + A :measured? t :top? t)
  (node + B)
  (node - C)
  (node + D)
  (node - E :measured? t :top? t)
  (node + F :measured? t)
  (node - G :measured? t)
  (edge + A B)
  (edge - A C)
  (edge - B C)
  (edge - E D)
  (edge + D F)
  (edge + C G)
  (edge + E G)
  )
(solve)
(check-consistency)

(what-node 'A)
#|
  1               A             ()   Assumption
|#

(what-node 'B)
#|
  1             A+B             ()   Assumption
  2               A             ()   Assumption
  3               B           (1 2)  (:OR (:NOT A) B (:NOT A+B))
|#

(what-node 'G)
#|
  1             C+G             ()   Assumption
  2             A-C             ()   Assumption
  3               A             ()   Assumption
  4        (:NOT C)           (2 3)  (:OR (:NOT A) (:NOT C) (:NOT A-C))
  5        (:NOT G)           (1 4)  (:OR C (:NOT G) (:NOT C+G))
|#

#|
;; Contradiction
(edge + A G)
Contradiction found:
1 A-C
2 C+G
3 A+G
4 A
;; In slime, type :, then (tms-answer 3)

(explain-node (find-node *ltms* 'A+G))
  1             C+G             ()   Assumption
  2             A-C             ()   Assumption
  3               A             ()   Assumption
  4        (:NOT C)           (2 3)  (:OR (:NOT A) (:NOT C) (:NOT A-C))
  5        (:NOT G)           (1 4)  (:OR C (:NOT G) (:NOT C+G))
  6      (:NOT A+G)           (5 3)  (:OR (:NOT A) G (:NOT A+G))
|#
