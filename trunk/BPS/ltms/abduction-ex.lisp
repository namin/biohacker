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

(rule ((:INTERN (related ?a ?b) :var ?def))
      (rassert! (:IMPLIES ?def
			  (:OR (brothers ?a ?b)
			       (father ?a of ?b)
			       (son ?a of ?b)))
		:RELATED-SIMPLE-MINDED-GUESS))

(setq patterns '((son ?son of ?father) (father ?father of ?son) (brothers ?a ?b)))

(needs '(father mohamed of aadel) :TRUE patterns)
; (((SON AADEL OF MOHAMED)))

(needs '(related mohamed aadel) :TRUE patterns)
;(((SON AADEL OF MOHAMED)) 
; ((FATHER MOHAMED OF AADEL))
; ((BROTHERS AADEL MOHAMED)) 
; ((BROTHERS MOHAMED AADEL))
; ((FATHER AADEL OF MOHAMED)) 
; ((SON MOHAMED OF AADEL)))

(setq pattern-cost-list
      '(((son ?son of ?father) . 20) 
	((father ?father of ?son) . 10) 
	((brothers ?a ?b) . 30)))

(labduce '(related mohamed aadel) :TRUE pattern-cost-list)
; (((FATHER MOHAMED OF AADEL)) . 10)