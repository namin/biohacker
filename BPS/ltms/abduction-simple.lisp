(in-ltre (create-ltre "Abduction Simple Example"))

(rassert! (:IMPLIES (:AND a b) c) :USER)
(rassert! (:IMPLIES (:AND x y) a) :USER)
(rassert! (:IMPLIES (:OR x y) b) :USER)
(rassert! (:IMPLIES c d) :USER)

#|
(setq node (get-tms-node 'c))
(setq label :TRUE)
(setq sets-1 (node-needs-1 node label))
(setq matching-patterns nil)
(setq new-nodes (list node))
|#

(needs 'c :TRUE)
; ((A B) (A Y) (A X) (X Y))

