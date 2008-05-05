(in-package :ecocyc)
;;;   Copyright (C) 1993-2004 by SRI International.  All rights reserved.

;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                    ;; Copyright SRI International 1994 ;;
;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      SMILES - Atom and Valence lists                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *two-char-atoms* is a list of lists of (atom-string subset valence).  
;; Subset is either :organic or :any.  :organic is defined in SMILES as
;; any of B, C, N, O, P, S, Cl, F, Br, I.

(defconstant *two-char-atoms*
  (list (list "Cl" :organic 7)
	(list "Br" :organic 7)
	(list "He" :any 2)
	(list "Li" :any 1)
	(list "Be" :any 2)
	(list "Ne" :any 8)
	(list "Na" :any 1)
	(list "Mg" :any 2)
	(list "Al" :any 3)
	(list "Si" :any 4)
	(list "Ar" :any 8)
	(list "Ca" :any 2)
	(list "Sc" :any 2)
	(list "Ti" :any 2)
	(list "Cr" :any 2)
	(list "Mn" :any 2)
	(list "Fe" :any 2)
	(list "Co" :any 2)
	(list "Ni" :any 2)
	(list "Cu" :any 2)
	(list "Zn" :any 2)
	(list "Ga" :any 3)
	(list "Ge" :any 4)
	(list "As" :any 5)
	(list "Se" :any 6)
	(list "Kr" :any 8)
	(list "Rb" :any 1)
	(list "Sr" :any 2)
	(list "Zr" :any 2)
	(list "Nb" :any 2)
	(list "Mo" :any 2)
	(list "Tc" :any 2)
	(list "Ru" :any 2)
	(list "Rh" :any 2)
	(list "Pd" :any 2)
	(list "Ag" :any 2)
	(list "Cd" :any 2)
	(list "In" :any 3)
	(list "Sn" :any 4)
	(list "Sb" :any 5)
	(list "Te" :any 6)
	(list "Xe" :any 8)
	(list "Cs" :any 1)
	(list "Ba" :any 2)
	(list "Hf" :any 2)
	(list "Ta" :any 2)
	(list "Re" :any 2)
	(list "Os" :any 2)
	(list "Ir" :any 2)
	(list "Pt" :any 2)
	(list "Au" :any 2)
	(list "Hg" :any 2)
	(list "Tl" :any 3)
	(list "Pb" :any 4)
	(list "Bi" :any 5)
	(list "Po" :any 6)
	(list "At" :any 7)
	(list "Rn" :any 8)
	(list "Fr" :any 1)
	(list "Ra" :any 2)
	(list "La" :any 2)
	(list "Ce" :any 2)
	(list "Pr" :any 2)
	(list "Nd" :any 2)
	(list "Pm" :any 2)
	(list "Sm" :any 2)
	(list "Eu" :any 2)
	(list "Gd" :any 2)
	(list "Tb" :any 2)
	(list "Dy" :any 2)
	(list "Ho" :any 2)
	(list "Er" :any 2)
	(list "Tm" :any 2)
	(list "Yb" :any 2)
	(list "Lu" :any 2)
	(list "Ac" :any 2)
	(list "Th" :any 2)
	(list "Pa" :any 2)
	(list "Np" :any 2)
	(list "Pu" :any 2)
	(list "Am" :any 2)
	(list "Cm" :any 2)
	(list "Bk" :any 2)
	(list "Cf" :any 2)
	(list "Es" :any 2)
	(list "Fm" :any 2)
	(list "Md" :any 2)
	(list "No" :any 2)
	(list "Lr" :any 2)
	)
  )

;; *one-char-atoms* is a list of lists of (atom-char subset valence).  
;; Subset is either :organic or :any.

(defconstant *one-char-atoms*
  (list (list #\B :organic 3)
	(list #\C :organic 4)
	(list #\N :organic 5)
	(list #\O :organic 6)
	(list #\P :organic 5)
	(list #\S :organic 6)
	(list #\F :organic 7)
	(list #\I :organic 7)
	(list #\b :organic 3)
	(list #\c :organic 4)
	(list #\n :organic 5)
	(list #\o :organic 6)
	(list #\p :organic 5)
	(list #\s :organic 6)
	(list #\H :any 1)
	(list #\K :any 1)
	(list #\V :any 2)
	(list #\Y :any 2)
	(list #\I :any 7)
	(list #\W :any 2)
	(list #\U :any 2)
	(list #\X :organic 0)  ;; X is a wildcard, listed as organic
	(list #\x :organic 0)  ;; so brackets need not be used
	(list #\R :any 1)      ;;kr:Mar-3-2008 R is also a wild-card
	)
  )


;; Element must be a string or a symbol.

(defun organic-element? (element)
  (let (tuple)
    (unless (stringp element)
      ;;kr98-05-28: added the get-frame-name
      (setq element (string (if (coercible-to-frame-p element)
				(get-frame-name element)
			      element)))
      )
    
    (setq tuple (if (eql 1 (length element))
		    (assoc (char element 0) *one-char-atoms*
			   :test #'eql)
		    (assoc element *two-char-atoms*
			   :test #'string-equal) ) )

    (eq :organic (second tuple))
    ) )
