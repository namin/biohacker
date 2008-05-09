(in-ltre (create-ltre "Abduction Test"))


(rule ((:INTERN (son ?son of ?father) :var ?def))
      (rassert! (:IMPLIES ?def (father ?father of ?son))
		:EQUIVALENCE)
      (rassert! (:IMPLIES ?def (related ?son ?father)) :FAMILY)
      (rassert! (:IMPLIES ?def (related ?father ?son))) :FAMILY)

(rule ((:INTERN (father ?father of ?son) :var ?def))
      (rassert! (:IMPLIES ?def (son ?son of ?father))
		:EQUIVALENCE)
      (rassert! (:IMPLIES ?def (related ?son ?father)) :FAMILY)
      (rassert! (:IMPLIES ?def (related ?father ?son))) :FAMILY)

(rule ((:INTERN (brothers ?a ?b) :var ?def))
      (rassert! (:IMPLIES ?def (brothers ?b ?a)) :EQUIVALENCE)
      (rassert! (:IMPLIES ?def (related ?a ?b)) :FAMILY)
      (rassert! (:IMPLIES ?def (related ?b ?a)) :FAMILY))

(rule ((:INTERN (uncle ?uncle of ?nephew) :var ?def))
      (rassert! (:IMPLIES ?def (nephew ?nephew of ?uncle))
		:EQUIVALENCE)
      (rassert! (:IMPLIES ?def (related ?nephew ?uncle)) :FAMILY)
      (rassert! (:IMPLIES ?def (related ?uncle ?nephew))) :FAMILY)

(rule ((:INTERN (nephew ?nephew of ?uncle) :var ?def))
      (rassert! (:IMPLIES ?def (uncle ?uncle of ?nephew))
		:EQUIVALENCE)
      (rassert! (:IMPLIES ?def (related ?nephew ?uncle)) :FAMILY)
      (rassert! (:IMPLIES ?def (related ?uncle ?nephew))) :FAMILY)

(rule ((:INTERN (brothers ?a ?b) :var ?brothers-def)
       (:INTERN (son ?c of ?a) :var ?son-def))
      (rassert! (:IMPLIES (:AND ?brothers-def ?son-def)
			  (uncle ?b of ?c))
		:FAMILY))

(rule ((:INTERN (related ?a ?b) :var ?def))
      (rassert! (:IMPLIES ?def
			  (:OR (brothers ?a ?b)
			       (father ?a of ?b)
			       (son ?a of ?b)
			       (uncle ?a of ?b)
			       (nephew ?a of ?b)))
		:RELATED-SIMPLE-MINDED-GUESS))

(setq patterns '((son ?son of ?father) (father ?father of ?son) (brothers ?a ?b) (uncle ?a of ?b) (nephew ?a of ?b)))

(needs '(father mohamed of aadel) :TRUE patterns)
; (((FATHER MOHAMED OF AADEL)) ((SON AADEL OF MOHAMED)))

(sort-fact-sets (needs '(related mohamed aadel) :TRUE patterns))
;(((BROTHERS AADEL MOHAMED)) 
; ((BROTHERS MOHAMED AADEL))
; ((FATHER AADEL OF MOHAMED)) 
; ((FATHER MOHAMED OF AADEL))
; ((NEPHEW AADEL OF MOHAMED)) 
; ((NEPHEW MOHAMED OF AADEL))
; ((SON AADEL OF MOHAMED)) 
; ((SON MOHAMED OF AADEL))
; ((UNCLE AADEL OF MOHAMED)) 
; ((UNCLE MOHAMED OF AADEL)))

(setq pattern-cost-list
      '(((son ?son of ?father) . 20) 
	((father ?father of ?son) . 10) 
	((brothers ?a ?b) . 30)
	((uncle ?a of ?b) . 40)
	((nephew ?a of ?b) . 50)))

(labduce '(related mohamed aadel) :TRUE pattern-cost-list)
; (((FATHER MOHAMED OF AADEL)) . 10)