(in-package :ecocyc)
;;;   Copyright (C) 1993-2004 by SRI International.  All rights reserved.

;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                    ;; Copyright SRI International 1994 ;;
;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  SMILES - Adjacency List Translator                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains functions that convert the representation of molecules
;; from SMILES (Simplified Molecular Input Line Entry System) format to the
;; adjacency list (connection table) format used by Ecocyc for certain
;; computations.  As intermediary forms, the equivalents of the stored forms
;; of the Ecocyc knowledge base are generated.

;; These functions effectively comprise a top-down LL(2) recursive-descent
;; parser that converts chemical formulas from linear strings to adjacency 
;; lists.

;; In addition to parsing extended SMILES (ignoring information not
;; used by Ecocyc like stereochemistry), this parser also accepts
;; wildcard atoms and bonds.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following SMILES grammar was used in generating these functions:

;;			EBNF SMILES Grammar

;; {aromatic atom}	= 'b | 'c | 'n | 'o | 'p | 's
;; {organic atom}	= 'B | 'C | 'N | 'O | 'P | 'S | 
;; 			'F | 'Cl | 'Br | 'I | {aromatic atom}
;; {atom}	        = <all possible atoms, incl. 'H >
;; {#}		        = <any numerical digit>
;; {plus charge}	= '+* | {#}?
;; {minus charge}	= '-* | {#}?
;; {charge}	        = '+ {plus charge} | '- {minus charge}
;; {bond}		= nil | '- | '= | '# | ':
;; {cycle}              = <any numerical digit>
;; {nonbracketed}	= {organic atom} 
;; {bracketed}	        = '[ {atom} ('H {#}?)? {charge}? '] 
;; {node option}	= {bracketed} {cycle}? | {nonbracketed} {cycle}?
;; {node}		= {node option} ('. {node option} )?
;; {branchII}	        = '( {compound} ')
;; {branch}	        = '( {bond} {compound} ')

;;NEW top part by pkarp:
;; {continue}	        = {bond} {node} | {bond} {branch} | {branchII}
;; {compound} 		= {node} | {compound} {continue}
;; {smiles}             = {branch} | {compound}

;;OLD top part by Adarsh:
;; {compound}	        = {node} ({branch}* {bond} ({branchII}|{node}))*
 
 
;; KEY:
;; 	Literals are denoted with a single quote, such as 'C.
;; 	Grammar is fully left-factored extended Bacchus Naur form.
;; 	This version allows both C=CC=CC(=CC)C=CC=C (valid SMILES)
;; 	and C=CC=CC=(CC)C=CC=C (invalid SMILES !) to parse normally as the same compound.
;; 	(Branch bonds may be specified outside branch parens.)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                         SMILES Notation Conventions

;; Here's a quick summary of the specification of SMILES*.  See sample
;; translations below for examples.

;; 1>  Atoms are represented by atomic symbols.
;;     Atoms not in the organic subset (B, C, N, O, P, S, F, Cl, Br, I),
;;     charged atoms, or organic atoms with an unusual valence must be
;;     specified within brackets.
;;     Hydrogens need not be explicitly stated.
;;     Aromatic atoms may be indicated with lower-case letters.
;;     Examples:  C      methane (CH4)
;;                [OH-]  hydroxyl anion
;;                [Au]   elemental gold

;; 2>  Single, double, triple, and aromatic bonds are represented by the
;;     symbols -, =, #, and :, respectively.  Single bonds and aromatic
;;     bonds (with atoms in lower case) need not explicitly be stated.
;;     Examples:  CC     ethane (CH3CH3)
;;                C-C    also ethane
;;                C=C    ethylene
;;                C#N    hydrogen cyanide (HCN)

;; 3>  Branches are specified by enclosures in parentheses.
;;     Example:   CC(C)C isopropane

;; 4>  Cyclic structures are represented by breaking one bond in the cycle
;;     and designating the ring closure with a number next to the two atoms
;;     involved in the ring closure bond.
;;     Examples:  c1ccccc1  benzene
;;                C1CCCCC1  hexane

;; 5>  Disconnected compounds, including ionic bonds, are written as individual
;;     structures separated by a period.
;;     Example:  [NH4+].[OH-]  ammonium hydroxide

;; 6>  Isomeric SMILES structures are treated as their generic SMILES
;;     counterparts, as CompoundKB does not currently utilize stereochemical
;;     or isotopic information.  This parser accepts isomeric SMILES and
;;     simply screens out the unused information.  The code is flagged
;;     at the appropriate locations for future changes in case CompoundKB
;;     eventually uses isomeric information.  The relevant procedures are
;;     smiles-bond, smiles-bracketed, and smiles-stereochemistry.

;; 7>  Wildcards (NOT PART OF EXTENDED SMILES):
;;     The generic wildcard atom X may be used to symbolize an unknown
;;     atom of any element and has the full flexibility (incl. shorthand
;;     notations) of organic atoms.  The generic wildcard bond ? may
;;     be used in place of any bond.
;;     Examples:   CXC            c1ccxcc1
;;                 C=CC?C         C?X?C
;;     User-defined wildcards of any character length are also accepted.
;;     User-defined wildcards of length greater than one character must
;;     be enclosed in <> brackets.  Single-character wildcards using
;;     characters of the alphabet not covered by any other elements are
;;     also accepted (ex. D, Q, Z).  Both also have the full flexibility
;;     and shorthand of organic atoms -- lower case may be used to denote
;;     aromaticity, for example.
;;     To define a wildcard, use setq:
;;     (setq GROUP4 '(C S))  -- to represent common organic group 4 elements
;;     Example use:  CC<GROUP4>CN       c1c<group4>ccc1

;; * For a more detailed description, see 
;;  "SMILES, a Chemical Language and Information System.  1.  Introduction to 
;;       Methodolgy and Encoding Rules," Weininger, D.,  J. Chem. Inf. Comput. 
;;       Sci. 1988, 28, pp. 31-36.
;;   The rules summary here borrows heavily from this article.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                Sample SMILES - Adjacency List Translations

;; Smiles-to-adj-list is the function called by the user, followed by
;; a chemical formula in SMILES format, enclosed in quotes and possibly
;; terminated with a space.

;; 1>  HCO3-
;; Charges are specified in brackets, branches with parentheses.
;; ecocyc > (smiles-to-adj-list "C(=O)(O)[O-]")
;; ((C (2 2) (3 1) (4 1)) (O (1 2)) (O (1 1)) (O (- 1) (1 1)))
;; ecocyc > (hydrogenate (smiles-to-adj-list "C(=O)(O)[O-]"))
;; ((C (2 2) (3 1) (4 1)) (O (1 2)) (O (5 1) (1 1)) (O (- 1) (1 1)) (H (3 1)))

;; 2>  AMMONIUM HYDROXIDE -- NH4OH
;; Disjoint compounds, including ionic bonds, are split with a period ('.).
;; Charges in charged groups are placed on the non-hydrogen atom, unless
;; explicitly stated otherwise by the user.
;; ecocyc > (smiles-to-adj-list "[NH4+].[OH-]")
;; ((N (+ 1) (2 1) (3 1) (4 1) (5 1) (6 :IONIC)) (H (1 1)) (H (1 1)) 
;;  (H (1 1)) (H (1 1)) (O (- 1) (7 1) (1 :IONIC)) (H (6 1)))

;; 3>  PHENYL CHLORIDE
;; Cycles are indicated by following an atom with a number.  The next 
;; appearance of the same number after an atom indicates the closure of 
;; the cycle.  Numbers may be reused after each cycle closure.
;; Aromatic atoms may be indicated with lower-case letters or aromatic
;; bonds can be stated explicitly with a ': .
;; ecocyc> (smiles-to-adj-list "Clc1ccccc1")
;; ((CL (2 1)) (C (1 1) (3 :AROMATIC) (7 :AROMATIC)) 
;;  (C (2 :AROMATIC) (4 :AROMATIC)) (C (3 :AROMATIC) (5 :AROMATIC)) 
;;  (C (4 :AROMATIC) (6 :AROMATIC)) (C (5 :AROMATIC) (7 :AROMATIC)) 
;;  (C (6 :AROMATIC) (2 :AROMATIC)))
;; ecocyc > (smiles-to-adj-list "ClC1:C:C:C:C:C1")
;; ((CL (2 1)) (C (1 1) (3 :AROMATIC) (7 :AROMATIC)) 
;;  (C (2 :AROMATIC) (4 :AROMATIC)) (C (3 :AROMATIC) (5 :AROMATIC)) 
;;  (C (4 :AROMATIC) (6 :AROMATIC)) (C (5 :AROMATIC) (7 :AROMATIC)) 
;;  (C (6 :AROMATIC) (2 :AROMATIC)))

;; 4>  MALATE
;; ecocyc > (smiles-to-adj-list "C(=O)(O)C(O)CC(=O)O")
;; ((C (2 2) (3 1) (4 1)) (O (1 2)) (O (1 1)) (C (1 1) (5 1) (6 1)) (O (4 1)) 
;;  (C (4 1) (7 1)) (C (6 1) (8 2) (9 1)) (O (7 2)) (O (7 1)))

;; 5>  TYROSINE
;; ecocyc > (smiles-to-adj-list "C(C(=O)[O-])([NH3+])Cc1ccc(O)cc1")
;; ((C (2 1) (5 1) (9 1)) (C (1 1) (3 2) (4 1)) (O (2 2)) (O (- 1) (2 1)) 
;;  (N (+ 1) (1 1) (6 1) (7 1) (8 1)) (H (5 1)) (H (5 1)) (H (5 1)) (C (1 1) (10 1)) 
;;  (C (9 1) (11 :AROMATIC) (16 :AROMATIC)) (C (10 :AROMATIC) (12 :AROMATIC))
;;  (C (11 :AROMATIC) (13 :AROMATIC)) (C (12 :AROMATIC) (14 1) (15 :AROMATIC)) 
;;  (O (13 1)) (C (13 :AROMATIC) (16 :AROMATIC)) (C (15 :AROMATIC) (10 :AROMATIC)))

;;                                                    - AB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ----------------------------------------------------     defs

(defvar *atom-list* nil "list of atoms in the structure")
(defvar *bond-list* nil "list of bonds between atoms in the structure")
(defvar *charge-list* nil "list of atom-charge bindings in the structure")
(defvar *branch-list* nil 
  "stack of atoms at branch points, used in constructing *bond-list*")
(defvar *multiple-branch-atom* nil "to trap multiple branches from same atom")
(defvar *cycle-list* nil 
  "list of atoms at cycle junction points, also used for *bond-list*")
(defvar *index* 0 
  "index into *structure-formula* string of current parse location")
(defvar *charge* nil 
  "for charged atoms, used to construct *charge-list*")
(defvar *add-ionic-bond* nil "flag to determine :ionic bond addition")
(defvar *h-count* 0 "# hydrogens attached to current bracketed atom")
(defvar *last-h-count* 0 "# hydrogens attached to last bracketed atom")
(defvar *current-atom* 1 "# of current atom of parse in *atom-list*")
(defvar *add-branch-bond* 0 "determines when to pop off *branch-list*")
(defvar *bond-type* 1 "one of :ionic, :aromatic, 1, 2, or 3")
(defvar *prev-bond-type* 1 "cached in case of branching")
(defvar *saved-bond-list* nil "to restore after branching")
(defvar *structure-formula* nil "SMILES input string of structure")
(defvar *len* 0 "length in characters of *structure-formula*")

;; additional defs in atoms.lisp

;;;kr:Oct-20-2006 To debug the parsing of SMILES strings, it is helpful
;;; to trace all of these functions:
;;; (trace MATCH-GENERATE-QUERY SMILES-TO-ADJ-LIST SMILES-COMPOUND SMILES-BRANCH SMILES-BRANCHII SMILES-NODE SMILES-ATOM SMILES-ONE-CHAR-ATOM SMILES-CYCLE SMILES-BOND SMILES-GET-CURRENT-CHAR)

;; ----------------------------------------------------     smiles-to-adj-list
;; Top-level sentence symbol.  This is the function called by the user.
;; Structure-formula is entered by the user and consists of a chemical
;; formula string enclosed in quotes, possibly terminated with a space.
;; See examples above.
;; The optional flag match? if set to T calls charge information to
;; be ignored, which is often more suitable for substructure matching.

(defun smiles-to-adj-list (structure-formula &key match?)
  ;; init vars
  (setq *len* (length (setq *structure-formula* structure-formula)))
  (setq *atom-list* nil)
  (setq *bond-list* nil)
  (setq *charge-list* nil)
  (setq *branch-list* nil)
  (setq *multiple-branch-atom* nil)
  (setq *cycle-list* nil)
  (setq *index* 0)
  (setq *charge* nil)
  (setq *add-ionic-bond* nil)
  (setq *h-count* 0)
  (setq *last-h-count* 0)
  (setq *current-atom* 1)
  (setq *add-branch-bond* 0)
  (setq *bond-type* 1)
  (setq *prev-bond-type* 1)
  (setq *saved-bond-list* nil)

  (if (not (or (= *len* 0) (eq (char *structure-formula* 0) #\Space)))
      (smiles-compound))
  
  (make-adj-list *atom-list* *bond-list*
		 (if match? nil *charge-list*)
		 )
  )

;; ----------------------------------------------------     smiles-compound

(defun smiles-compound ()

  (smiles-node)

  (loop until (or (= *index* *len*) ;; end of formula string
		  (eq (smiles-get-current-char) #\Space) ;; end of formula
		  (eq (smiles-get-current-char) #\) )) ;; end of branch

	do
	(loop while (and (< *index* *len*)
			 (eq (smiles-get-current-char) #\( ) )
	    do (smiles-branch)
	       ;;kr:Feb-25-2008 Needed for fixing [Bug 1637] smiles error
	       (smiles-cycle)
	       )

	(unless (or (= *index* *len*) ;; end of formula string
		    (eq (smiles-get-current-char) #\Space) ;; end of formula
		    )
	  ;;kr:Oct-19-2006 Had to add a termination condition here,
	  ;; that is similar to the above, to deal with the closing paren.
	  ;; Otherwise, there is a break with "C(C(CC))C"
	  ;; Fixes [Bug 961] compound structure editor break (Marvin)
	  (if (eq (smiles-get-current-char) #\) ) ;; end of branch
	      ;;kr:Oct-20-2006 If we get here, it means that on this branch,
	      ;; after a sub-branch has ended, there is no follow-on atom,
	      ;; and instead, this branch ends too.  In other words, this is
	      ;; the case of consecutive closing parens.  We need to unwind this
	      ;; stack (without forming a bond).
	      (progn
		(decf *add-branch-bond*)
		(pop *branch-list*)
		)
	    (progn
	      (smiles-bond)
	      ;;kr:Feb-26-2008 Now that (smiles-bond) can terminate cycles, we have to again check for
	      ;; a closing paren to see if the branch ends too.
	      ;; Part of fixing [Bug 1004] Compound structure does not write to DB
	      (if (eq (smiles-get-current-char) #\) ) ;; end of branch
		  ;; We need to unwind this
		  ;; stack (without forming a bond).
		  (progn
		    (decf *add-branch-bond*)
		    (pop *branch-list*)
		    )
		;; Normal case, as before.
		(if (eq (smiles-get-current-char) #\( )
		    (progn (smiles-branchII)
			   ;;kr:Feb-25-2008 Probably is needed:
			   (smiles-cycle)
			   )
		  (progn (smiles-node)
			 (setq *multiple-branch-atom* nil)))
		)
	      )
	    )
	  )
	)
  )

;; ----------------------------------------------------     smiles-branch

(defun smiles-branch ()
  ;; match left paren
  (incf *index*)

  ;; determine which atom is the starting atom for any
  ;; branch bonds.  If there are multiple branches from
  ;; the same atom, several bonds must begin with the same
  ;; atom.  Otherwise the immediately previous atom (minus
  ;; hydrogens) is assumed to be bonded to the branch.
  (if *multiple-branch-atom*
      (push *multiple-branch-atom* *branch-list*)
    (push (- *current-atom* *h-count* 1) *branch-list*))
  ;;(format t "*branch-list* , after push     = ~A~%" *branch-list*)

  ;;kr:Feb-25-2008 Commentary: Is this bond processing really the only difference between
  ;; (smiles-branch) and (smiles-branchII) ??
  (smiles-bond)

  ;; keep track of the type of bond outside the branch
  ;; so we don't lose :aromatics.
  (push *prev-bond-type* *saved-bond-list*)
  (smiles-compound)

  ;; match right paren
  (if (eq (smiles-get-current-char) #\) )
      (progn (incf *index*)
	     (setq *multiple-branch-atom* (car *branch-list*))
	     (incf *add-branch-bond*)
	     (setq *bond-type* (pop *saved-bond-list*)))
    (smiles-error "Syntax error -- check branch parentheses"))
  )

;; ----------------------------------------------------     smiles-branchII

(defun smiles-branchII ()
  ;; match left paren
  (incf *index*)

  (if *multiple-branch-atom*
      (push *multiple-branch-atom* *branch-list*)
    (push (- *current-atom* *h-count* 1) *branch-list*))

  (push *prev-bond-type* *saved-bond-list*)
  (smiles-compound)

  ;; match right paren
  (if (eq (smiles-get-current-char) #\) )
      (progn (incf *index*)
	     (setq *multiple-branch-atom* (car *branch-list*))
	     (incf *add-branch-bond*)
	     (setq *bond-type* (pop *saved-bond-list*)))
    (smiles-error "Syntax error -- check branch parentheses"))
  )

;; ----------------------------------------------------     smiles-node
;; A node is defined as an atom, a group of atoms within brackets,
;; or two such atoms or groups separated by a period.

(defun smiles-node ()
  (smiles-node-option)

  ;; '. = disjoint compounds (may be an :ionic bond)
  (if (and (< *index* *len*) (eq (smiles-get-current-char) #\. ))
      (progn (incf *index*)   ;; match '.
	     (smiles-node-option))

    ;; no '. = no possibility of an ionic bond, as it would have been disjoint
    (setq *add-ionic-bond* nil))
  )

;; ----------------------------------------------------     smiles-node-option

(defun smiles-node-option ()
  (if (eq (smiles-get-current-char) #\[)
      (smiles-bracketed)
    (progn (smiles-nonbracketed)
	   ;; no '[ = no possibility of :ionic bond, as it would have been charged
	   (setq *add-ionic-bond* nil)))
  )

;; ----------------------------------------------------     smiles-bracketed

(defun smiles-bracketed ()
  ;; match left bracket
  (incf *index*)

  ;; screen out isotopic information, if any.  Isotopes are specified in
  ;; brackets by preceeding the atom type with the atomic mass.
  (loop while (digit-char-p (smiles-get-current-char) 10)
	do (incf *index*))

  ;; any type of atom may be within brackets
  (smiles-atom :any)

  ;; screen out stereochemical information, if any.
  (if (eq (smiles-get-current-char) #\@ )
      (smiles-stereochemistry))

  ;; check for hydrogen atoms
  (setq *last-h-count* *h-count*)
  (if (eq (smiles-get-current-char) #\H )
      (smiles-hydrogenate)
    (setq *h-count* 0))

  ;; check for charges and place them
  (smiles-charge)

  ;; match right bracket
  (incf *index*)

  (smiles-cycle)
  )

;; ----------------------------------------------------     smiles-stereochemistry
;; screens out stereochemical information, effectively treating isomeric
;; SMILES as generic SMILES.  Change this procedure if CompoundKB can
;; represent stereochemical information.

(defun smiles-stereochemistry ()
  ;; match the first '@
  (incf *index*)

  (case (smiles-get-current-char)
	(#\@ (smiles-stereochemistry))
	((#\T #\S #\A #\O) (setq *index* (+ *index* 3))))
  )

;; ----------------------------------------------------     smiles-hydrogenate
;; hydrogens are specified within a bracketed node.  Add them to the *atom-list*
;; and *bond-list*, set *h-count* appropriately for cycles, etc.

(defun smiles-hydrogenate ()
  ;; match the 'H
  (incf *index*)

  (let ((hydrogen-count (digit-char-p (smiles-get-current-char) 10)))
    (if hydrogen-count
	(progn (incf *index*)     ;; match the hydrogen-count
	       (setq *h-count* hydrogen-count)
	       ;; add all 'H's to the *atom-list* and *bond-list*
	       ;; and increment *current-atom* appropriately
	       (let ((start-bond-atom (- *current-atom* 1)))
		 (loop for i from 1 to hydrogen-count
		       do (nconc *atom-list* (list 'H))
		       (push (list start-bond-atom *current-atom* 1) *bond-list*)
		       (incf *current-atom*))))

      ;; no explicit hydrogen-count -- only one hydrogen present
      (progn (setq *h-count* 1)
	     (nconc *atom-list* (list 'H))
	     (push (list (- *current-atom* 1) *current-atom* 1) *bond-list*)
	     (incf *current-atom*))))
  )

;; ----------------------------------------------------     smiles-nonbracketed

(defun smiles-nonbracketed ()
  ;; no attached hydrogens
  (setq *last-h-count* *h-count*)
  (setq *h-count* 0)

  ;; only organic atoms are specified without brackets
  (smiles-atom :organic)

  (smiles-cycle)
  )

;; ----------------------------------------------------     smiles-cycle

(defun smiles-cycle (&optional incoming-bond-type)
  ;; structure is cyclic when an organic item is immediately followed by a number
  (when (< *index* *len*)
    (let ((cycle-number (digit-char-p (smiles-get-current-char) 10)))
      ;;kr:Mar-7-2008 Several rings can be opened or closed at one atom.
      ;; So we have to loop through all of those.
      ;; Otherwise, there is a break with "C12C(C1)C2"
      (loop while cycle-number
	  do (smiles-cycle-internal cycle-number incoming-bond-type)
	     ;; When cycle-number is set to NIL, the loop terminates
	     (setf cycle-number (when (< *index* *len*)
				  (digit-char-p (smiles-get-current-char) 10)
				  ))
	     ;;kr:Mar-7-2008 In cases that look like "...C=23..." , it presumably means
	     ;; that the double-bond is only to 2, not to 3 as well.
	     ;; So, we have to make sure the bond-type will the default (which is 1),
	     ;; after the first number has been consumed.
	     (setf incoming-bond-type nil)
	     )
      )
    )
  )

(defun smiles-cycle-internal (cycle-number incoming-bond-type)
  (incf *index*)
  (let ((current-cycle (assoc cycle-number *cycle-list*)))
    
    ;; if cycle-number is already in *cycle-list*, close the cycle by
    ;; removing *cycle-list* element containing the cycle-number
    ;; (current-cycle) and adding a bond consisting of current-cycle's 
    ;; atom number, the *current-atom* - (+ *h-count* 1), 
    ;; and the *bond-type* (either :aromatic or 1). 
    (if current-cycle
	;; Close an existing cycle:
	(progn (setq *cycle-list* (remove current-cycle *cycle-list* :test #'equal))
	       (when (not (eq *bond-type* :aromatic))
		 (setq *bond-type* (or incoming-bond-type
				       ;;kr:Feb-26-2008 The assumed default of 1 can be overridden now
				       ;; with the incoming-bond-type argument.
				       1)))
	       (push (list (cadr current-cycle) 
			   (- *current-atom* *h-count* 1) *bond-type*) *bond-list*))
      
      ;; else add (*current-atom* - *h-count* - 1) to the cycle list.
      ;; Open a new cycle:
      (push (list cycle-number (- *current-atom* *h-count* 1)) *cycle-list*)
      )
    )
  )

;; ----------------------------------------------------     smiles-bond

(defun smiles-bond ()
  (let ((current-char (smiles-get-current-char)))
    (setq *prev-bond-type* *bond-type*)

    ;; BOND TYPE DETERMINATION
    (case current-char
	  ;; screen out isomeric information, ie. E/Z cis/trans info.
	  ((#\- #\\ #\/) (incf *index*) (setq *bond-type* 1))
	  (#\: (incf *index*) (setq *bond-type* :aromatic))
	  (#\= (incf *index*) (setq *bond-type* 2))
	  (#\# (incf *index*) (setq *bond-type* 3))
	  (#\? (incf *index*) (setq *bond-type* '?))
	  ;; if bond is nil...
	  ;; Use lookahead to determine if *bond-type* should be :aromatic or 1.
	  (otherwise (unless (and (eq *bond-type* :aromatic)
				  (or (find current-char "bcnopsx[<")
				      (and (lower-case-p current-char)
					   (boundp (intern (string (char-upcase current-char)))))))
			     (setq *bond-type* 1))))
    (if (< *index* *len*)
	(if (digit-char-p (smiles-get-current-char) 10)
	    ;; Part of fixing [Bug 1004] Compound structure does not write to DB
	    ;;kr:Feb-26-2008 We ran into the case where a cycle seems to be ending,
	    ;; _after_ an explicitly specified bond-type.
	    ;; As an example, we are now here:  C1(CC=1)C
	    ;;                                        ^
	    (smiles-cycle *bond-type*)
	  
	  ;; ADDITION OF NEW BOND TO *BOND-LIST*
	  ;; if *add-branch-bond* > 0, decrement it, pop *branch-list* for
	  ;; first element (start-atom) of current bond.  Otherwise keep *current-atom* - 1.
	  (let ((start-atom (- *current-atom* *h-count* 1)))
	    (when (> *add-branch-bond* 0)
	      ;;(format t "*add-branch-bond* = ~A~%" *add-branch-bond*)
	      ;;(format t "*branch-list*     = ~A~%" *branch-list*)
	      (decf *add-branch-bond*)
	      (setq start-atom (pop *branch-list*)))
	    
	    ;; second element is *current-atom*, third is *bond-type*.
	    ;; add this list to *bond-list*, ex: *bond-list* of "CC" is ((1 2 1)).
	    (push (list start-atom *current-atom* *bond-type*) *bond-list*))
	  )
      ;;kr:Mar-3-2008 We are the end of the string, before we can get to the next atom.
      ;; Fixes the naked break of [Bug 1419] searching by smiles substructure breaks
      ;; and presents a more intelligible error message instead.
      (smiles-error (format nil "Syntax error -- Expected atom not found after a bond.~%       Premature end of string"))
      )
    )
  )

;; ----------------------------------------------------     smiles-charge

(defun smiles-charge ()
  ;; check for charges
  (if (eq (smiles-get-current-char) #\+)
      (smiles-plus-charge)
    (if (eq (smiles-get-current-char) #\-)
	(smiles-minus-charge)
      (setq *charge* nil)))

  (if *charge*
      (push  (list (- *current-atom* *h-count* 1) *charge*) *charge-list*))

  ;; if charged and *add-ionic-bond* set, add it & reset *add-ionic-bond*
  (if (and *charge* *add-ionic-bond*)
      (progn (setq *add-ionic-bond* nil)
	     (push (list (- *current-atom* *h-count* *last-h-count* 2) 
			 (- *current-atom* *h-count* 1) :ionic) *bond-list*))

    ;; otherwise set *add-ionic-bond*
    (setq *add-ionic-bond* T))
  )

;; ----------------------------------------------------     smiles-plus-charge
;; set *charge* appropriately and return

(defun smiles-plus-charge ()
  ;; match '+
  (incf *index*)
  (setq *charge* 1)

  (let ((charge-count (digit-char-p (smiles-get-current-char) 10)))
    (if charge-count
	(progn (incf *index*)     ;; match the charge-count
	       (setq *charge* charge-count))
      (loop while (eq (smiles-get-current-char) #\+)
	    do (incf *charge*)
	    (incf *index*))))

  )

;; ----------------------------------------------------     smiles-minus-charge
;; set *charge* appropriately and return

(defun smiles-minus-charge ()
  ;; match '-
  (incf *index*)
  (setq *charge* -1)

  (let ((charge-count (digit-char-p (smiles-get-current-char) 10)))
    (if charge-count
	(progn (incf *index*)     ;; match the charge-count
	       (setq *charge* (- charge-count)))
      (loop while (eq (smiles-get-current-char) #\-)
	    do (decf *charge*)
	    (incf *index*))))
  )

;; ----------------------------------------------------     smiles-atom
;; includes {atom}, {organic-atom}, and {aromatic-atom} from grammar

(defun smiles-atom (atom-type)
  ;; first check two-character atom possibilities
  (if (< (+ *index* 1) *len*)
      (let* ((next-atom (subseq *structure-formula* *index* (+ *index* 2))) 
	    (atom-match (assoc next-atom *two-char-atoms* :test #'string=)))

	(if (and atom-match (or (eql atom-type :any) 
				(eql (cadr atom-match) atom-type)))
	    (progn (incf *current-atom*)
		   (incf *index*)
		   (incf *index*)
		   (setq *atom-list* (nconc *atom-list* 
					    (list (intern (string-upcase next-atom))))))
	  ;; else no two-char match, try one-char
	  (smiles-one-char-atom atom-type)))

    ;; else trying to match last char of string, try one-char
    (smiles-one-char-atom atom-type))
  )

;; ----------------------------------------------------     smiles-one-char-atom
;; if smiles-atom does not find a two-character-atom, 
;; try one-char-atom possibilities and wildcards 

(defun smiles-one-char-atom (atom-type)
  (let* ((current-char (smiles-get-current-char)) 
	(atom-match (assoc current-char *one-char-atoms*)))

    (if (or (and atom-match (or (eql atom-type :any) 
			    (eql (cadr atom-match) atom-type)))
	    ;; check for user-defined wildcard atoms
	    (boundp (intern (string (char-upcase current-char)))))
	(progn (when (lower-case-p current-char)
		     (setq current-char (char-upcase current-char))
		     (setq *bond-type* :aromatic))

	       (incf *current-atom*)
	       (incf *index*)

	       (setq *atom-list* 
		     (nconc *atom-list* (list
					 (intern (string current-char)))))
	       )

      ;; not single char atom or wildcard -- test for marked wildcard string
      (if (eq current-char #\<)
	  (let ((wildcard-index-start (incf *index*))
		(wildcard-aromatic 
		 (lower-case-p (smiles-get-current-char))))
	    (loop for wildcard-index-end from wildcard-index-start
		  when (eq (char *structure-formula* wildcard-index-end) #\>)
		  do (setq *index* (1+ wildcard-index-end))
		  (incf *current-atom*)
		  (setq *atom-list* (nconc *atom-list*
					   (list (intern (string-upcase
							  (subseq *structure-formula* 
								  wildcard-index-start wildcard-index-end))))))
		  return T)
	    (when wildcard-aromatic (setq *bond-type* :aromatic))
	    )

	;; else error... not an atom
	(smiles-error "Syntax error -- unknown atom \"~C\"" current-char)
	)
      )
    )
  )

;;;kr:Feb-25-2008 Splitting out this repeated idiom makes debugging easier.
;;; We can trace this.
;;;
(defun smiles-get-current-char ()
  (char *structure-formula* *index*)
  )

(defvar *smiles-error-use-popup?* t) ;; Can also be NIL

;; paley:Aug-10-2006 Rewrote this fn to either popup a message dialog or 
;; drop into the debugger depending on the value of *smiles-error-use-popup?*.
;; Previously, this fn always dropped into the debugger.  New default is to
;; popup the dialog.
(defun smiles-error (msg &optional arg)
  (let ((indicator-string 
	 (format nil "~%~A~%~A^~%"
		 *structure-formula*
		 (make-string *index* :initial-element #\Space))))
    (if *smiles-error-use-popup?*
	(progn
	  (ctv:dialogue :text (format nil "~AError: ~A~%" indicator-string
				      (if arg (format nil msg arg) msg))
			:buttons '(("OK" :abort))
			:label "SMILES Error"
			:dialogue-text-style (clim:make-text-style :fix :bold :large)
			)
	  (abort))
      (progn
	;;(format *terminal-io* indicator-string)
	(error (concatenate 'string indicator-string msg) arg)
	)
      )))






(defvar *failed-smiles-parses* nil
  "For accumulating erroneous SMILES strings.
This is a list of lists, with the sublists of the form (CPD-ID \"indicator-string    error msg string\") .")

;; ====================================================================== 
;; [API]
;; kr:Feb-25-2008    Description : 
;; 
;; 
;;      Arguments : 
;;                  
;;                  
;;        Returns : 
;;   Side Effects : 
;; Update History :

(defun test-smiles-parser (&optional (cpd-list (get-class-all-instances '|Amino-Acids-20|)))
  (setf *failed-smiles-parses* nil)
  (let* ((*smiles-error-use-popup?* nil))
    (loop for cpd in cpd-list
	for molecule = (frame-to-molecule cpd)
	for smiles = (when molecule
		       (molecule-to-smiles molecule) )
	when smiles
	do
	  (handler-case
	      (smiles-to-adj-list smiles)
	    (error (the-error-condition)
	      (push (list (get-frame-name cpd) (format nil "~A" the-error-condition)) *failed-smiles-parses*)
	      )
	    )
	  )
    )
  *failed-smiles-parses*
  )

#||
 ;;;kr:Feb-25-2008 on metacyc-12.0+
(test-smiles-parser (gcai '|Compounds|))

;;; Useful for testing parsing of SMILES:
http://www.daylight.com/daycgi_tutorials/depict.cgi

;;;kr:Feb-26-2008 Some test cases, which are now fixed:

;;; [Bug 1637]
(smiles-to-adj-list "C1(CC(C)1)")
;;; after fixing:
==> (length *) = 1149  : lots of parse errors

;;; [Bug 1004] Compound structure does not write to DB
(smiles-to-adj-list "C1(CC=1)C")

;;; after these fixes:
==> (length *) =  333


;;;kr:Mar-7-2008 fixing multiple ring closures at one atom.
;;; before:
EC(5): (smiles-to-adj-list "C3(=O)(C1(C(=C(O)C(O)=CC=1)OC2(C=C(O)C=C(C=23)O)))")
Error: 
C3(=O)(C1(C(=C(O)C(O)=CC=1)OC2(C=C(O)C=C(C=23)O)))
                                            ^
Syntax error -- unknown atom "3"

;;; after:
EC(19): (smiles-to-adj-list "C3(=O)(C1(C(=C(O)C(O)=CC=1)OC2(C=C(O)C=C(C=23)O)))")
((C (2 2) (3 1) (7 1) (18 1)) (O (1 2)) (C (1 1) (4 1) (10 2)) (C (3 1) (5 2)) (C (4 2) (6 1)) (O (5 1))
 (C (1 1) (8 1) (9 2)) (O (7 1)) (C (7 2) (10 1)) (C (9 1) (3 2) (11 1)) (O (10 1) (12 1))
 (C (11 1) (13 1) (18 2)) (C (12 1) (14 2)) (C (13 2) (15 1) (16 1)) (O (14 1)) (C (14 1) (17 2))
 (C (16 2) (18 1)) (C (17 1) (12 2) (1 1) (19 1)) (O (18 1)))
EC(20): 

;;; small test case:
(smiles-to-adj-list "C12C(C1)C2")
EC(16): (smiles-to-adj-list "C12C(C1)C2")
((C (2 1) (3 1) (4 1)) (C (1 1) (3 1) (4 1)) (C (2 1) (1 1)) (C (2 1) (1 1)))
EC(17): 
||#
