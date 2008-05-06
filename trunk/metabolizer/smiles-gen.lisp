(in-package :ecocyc)
;;;   Copyright (C) 1993-2004 by SRI International.  All rights reserved.

;;; This file generates a Smiles representation for a compound

;;; Current limitations: Doesn't handle ionic bonds, stereochemistry,
;;; non-existent elements.  We don't generate canonical Smiles.


(defvar *smibuf* nil
  "Character buffer that holds the Smiles string being generated.")
(defvar *smi-i* nil
  "Index into the next unused element in *smibuf*.")
(defvar *smiles-visited* nil
  "Every element is a list (atom (ring-closing-bonds) *smi-i*)
   Ring-closing-bonds is a list of numbers; each number represents
   one ring-closing-bond to that atom.")  
(defvar *smiles-cycles* nil
  "A counter for the number of number-labeled ring-closing bonds thus far
   assigned due to cycles in the molecule.")
(defvar *smiles-debug* nil
  "Causes debugging info to be printed during processing.")
(defvar *include-h?* nil
  "T when we should include H atoms in the generated smiles,
   which would be the case either if the user requested it
   or if H atoms are the only atoms in the molecule.")


(defun frsmi (cpd)
  (molecule-to-smiles (frame-to-molecule cpd))
  )


;;; This is the main user-callable function.  Input: a molecule defined
;;; as an atom-struct.  Returns: a Smiles string.

;;; The algorithm is to walk through the molecule, and to emit into
;;; the Smiles string appropriately as we encounter bonds and atoms.
;;; We then have to patch the string because of cycles.  For example,
;;; for cyclohexane, we'd first generate CCCCCCC1 and then backpatch
;;; the first C to make it  C1CCCCCC1  because the first time we
;;; encountered C1 we didn't know it was part of a cycle.

(defun molecule-to-smiles (molecule
			   &key
			   (include-h? t) ;; Include H atoms in the Smiles string?
			   )
  (let ()
    (setq *smibuf* (make-string (max 50 (* 6 (length molecule))))
	  *smi-i* 0
	  *smiles-visited* nil
	  *smiles-cycles* 0
	  *include-h?* (or include-h?
			   (loop for i from 0 below (length molecule)
				 for atom = (aref molecule i)
				 always
				 (eq 'H (ec-atom-element atom)) ) )
	  )

    (gen-smiles molecule (find-start-atom molecule))

    (back-patch-smiles)

    (subseq *smibuf* 0 *smi-i*)
    ) )



(defun sample-smiles ()
  (let ((sample-cpds (list 'l-lactate 'uracil 'thymidine 'ser 'trp
			   'carbon-monoxide 'oxonium)))
    (loop for x in sample-cpds
	  do
	  (format t "The Smiles for  ~A  is  ~A~%~%"
		  x
		  (molecule-to-smiles (frame-to-molecule x))
		  )
	  )
    (replace-answer-list sample-cpds)
    ) )


;;; Test smiles generation by looping through a bunch of compounds,
;;; generating smiles for compound X, and then using the matcher
;;; to match the smiles representation of the compound against
;;; itself.  Unfortunately, this tester turned up problems in both
;;; the smiles parser and (I suspect) the matcher.

(defun test-smiles-gen (&optional (cpd-list (get-class-all-instances '|Amino-Acids-20|)))
  (loop for cpd in cpd-list
	for smiles = (molecule-to-smiles (frame-to-molecule cpd))
	unless (match-single smiles cpd
			     :ref-format :id
			     :match-h? nil)
	do
	(format t "Mismatch for ~A~%" cpd)
	)
  )


;;; When we walk the molecule, we have to start somewhere.  This function
;;; determines that somewhere.  We try to make a choice that will make
;;; the Smiles strong look like a person would have generated it.

(defun find-start-atom (molecule)
  (or
   ;; A carbon connected to a single carbon
   (loop for i from 0 below (length molecule)
	 for atom = (aref molecule i)
	 when (and (eq 'C (ec-atom-element atom))
		   (eql 1
			(loop for bond in (ec-atom-bonds atom)
			      count
			      (eq 'C (ec-atom-element (bond-atom2 bond)))
			      ) ) )
	 return atom)

   ;; Any carbon atom
   (loop for i from 0 below (length molecule)
	 for atom = (aref molecule i)
	 when (eq 'C (ec-atom-element atom))
	 return atom)
      
   ;; Any non-H atom
   (loop for i from 0 below (length molecule)
	 for atom = (aref molecule i)
	 unless (eq 'H (ec-atom-element atom))
	 return atom)

   ;; Any atom
   (aref molecule 0)
   )
  )


;;; Main workhorse function.  Atom is the current atom in our walk;
;;; Previous-atom is the atom we just came from.
;;;
;;; We emit Atom into the Smiles string, and then proceed to all atoms
;;; connected to Atom.  

(defun gen-smiles (molecule atom &optional previous-atom)
  (let (bond-to-longest-chain len bonds last-bond
        (chain-length 0)
	(visited-top (1- (length *smiles-visited*))) )

    (setq bonds
	  (loop for bond in (ec-atom-bonds atom)
		when (and (not (eq 'H (ec-atom-element (bond-atom2 bond))))
			  (not (eq previous-atom (bond-atom2 bond)))
			  )
		collect bond) )

    ;; Emit Atom
    (gen-atom atom)

    ;; Find the adjacent non-H atom that leads to the longest branch of
    ;; the molecule, because this is how people tend to generate Smiles
    ;; (they define short branches within parens, and leave the longest
    ;; branch for last).
    (loop for bond in bonds
	  do
	  (setq len (compute-chain-length molecule (bond-atom2 bond)))
	  (when (> len chain-length)
	    (setq chain-length len
		  bond-to-longest-chain bond)
	    ) )

    ;; Re-order the bonds so the bond to the longest chain is last.
    (when bond-to-longest-chain
      (setq bonds (delete bond-to-longest-chain bonds))
      (setq bonds (nconc bonds (list bond-to-longest-chain)))
      )
    (setq last-bond (1- (length bonds)))
    

    ;; Here we emit a bond to each connected atom, and then recurse
    ;; to walk from that connected atom.  When we're processing a
    ;; branch, we emit parens.
    ;;
    ;; The visited business pertains to cycles.  Now Smiles strings
    ;; only encode each bond in the molecule once (single bonds of
    ;; course are not encoded explicitly).  However, with cycles,
    ;; each bond is encountered twice in the walk, e.g., in cyclohexane,
    ;; C1CCCCCC1, there is a bond between C1 and C6, but we only encode
    ;; it in the C6-C1 direction, not the C1-C6 direction.  We actually
    ;; do encounter the latter direction during our walk, so we have to
    ;; take pains to avoid generating the bond then.  The algorithm is
    ;; that we do not traverse a bond if the atom2 was newly visited,
    ;; meaning that it wasn't visited when we first entered this function,
    ;; but it was visited by the time we actually encountered atom2 in
    ;; our walk, meaning that we encountered it during our walk to
    ;; earlier connected atoms.
    (loop for bond in bonds
	  for i from 0
	  for atom2 = (bond-atom2 bond)
	  for visited-pos = (position atom2 *smiles-visited*
				      :key #'first)
	  for newly-visited? = (and visited-pos
				    (> visited-pos
				       visited-top) )
	  for emit-parens? = (and (not (eql i last-bond))
				  (not (assoc atom2 *smiles-visited*)) )
	  when (not (and visited-pos newly-visited?))
	  do
	  (when emit-parens?  (gen-string "("))
	  (gen-bond bond atom2)
	  (unless visited-pos
	    (gen-smiles molecule atom2 atom)
	    )
	  (when emit-parens?  (gen-string ")"))
	  )
    ) )


;; Emit an atom into the Smiles string and track the visited status
;; of the atom.

(defun gen-atom (atom)
  (let (name long? n-h-present)

    (setq n-h-present
	  (if *include-h?*
	      (loop for bond in (ec-atom-bonds atom)
		    count (eq 'H (ec-atom-element (bond-atom2 bond)))
		    )
	    0) )

    (setq name (get-name-string (ec-atom-element atom)))
    (when (loop for bond in (ec-atom-bonds atom)
		thereis
		(eq :AROMATIC (bond-type bond)) )
      (setq name (string-downcase name))
      )

    (when (or (not (organic-element? (ec-atom-element atom)))
	      (not (eql 0 (ec-atom-charge atom)))
	      (< 0 n-h-present)
	      )
      (setq long? t)
      )
    (when long?  (gen-string "["))
    (gen-string name)
    (when (> n-h-present 0)
      (gen-string "H")
      (when (< 1 n-h-present)
	(gen-string (prin1-to-string n-h-present))
	) )
    (when (not (eql 0 (ec-atom-charge atom)))
      (dotimes (i (abs (ec-atom-charge atom)))
	(gen-string (if (> (ec-atom-charge atom) 0) "+" "-"))
	)
      )
    (when long?  (gen-string "]"))
    ;; It's important that the visited list contain atoms in the
    ;; order in which they were visited so we can tell which atoms
    ;; were newly visited.
    (setq *smiles-visited* (nconc *smiles-visited*
				  (list (list atom nil *smi-i*))))
  ))


;; Emit a bond.  Ring-closing bonds are numbered in Smiles.

(defun gen-bond (bond atom2)
  (let (visited ring-closing-bonds)
    (unless (find (bond-type bond) '(1 :AROMATIC))
      (gen-string (cdr (assoc (bond-type bond)
			      '((2 . "=")
				(3 . "#")) ) ) )
      )

    (when (setq visited (assoc atom2 *smiles-visited*) )
      ;; Emit a new ring-closing bond at this atom.
      (setq ring-closing-bonds (second visited))
      (rplaca (cdr visited)
	      (cons (incf *smiles-cycles*) ring-closing-bonds) )
      (gen-string (prin1-to-string *smiles-cycles*))
      )
    ) )


;; Copy a string into *smibuf*

(defun gen-string (s)
  (when *smiles-debug*
    (format t "    Adding    ~A~%" s)
    )

  (loop for i from 0 below (length s)
	do
	(setf (aref *smibuf* *smi-i*)
	      (char s i) )
	(incf *smi-i*)
	)
  )


;; Compute the length of some branch of a molecule by finding the
;; length of the longest sequence of edges within that branch.
;; Another approach would be to count the atoms in this branch
;; of the molecule, but this is only a heuristic anyway.  Stop
;; when we encounter an atom that has been visited, either by the
;; main walk or by our walk here.

(defun compute-chain-length (molecule atom &optional chain)
  (if (find atom chain)
      (length chain)
      (+ 1 (loop for bond in (ec-atom-bonds atom)
		 for atom2 = (bond-atom2 bond)
		 unless (or (eq 'H (ec-atom-element atom2))
			    (assoc atom2 *smiles-visited*)
			    (find atom2 chain)
			    )
		 maximize
		 (compute-chain-length molecule atom2 (push atom chain))
		 ) )
    )
  )


;; In a Smiles string for a compound containing cycles, the bonds that
;; close the cycles are numbered, e.g., in cyclohexane, C1CCCCC1.
;; However, when we walk a molecule, the first time we encounter the
;; C1 atom, we don't know that a bond to it will be used to close the 
;; cycle, so the string is initially CCCCCC1.  Here we go back to patch 
;; the first references to these numbered atoms based on info in
;; *smiles-visited*.

(defun back-patch-smiles ()
  (let (cycles
	(offset 0))
    ;; Extract those visited items that have cycle numbers, and then
    ;; sort them by position of the atoms in *smibuf*.
    (setq cycles (sort (loop for item in *smiles-visited*
			     when (second item)
			     collect item)
		       #'<
		       :key #'third) )

    ;; Now enter the bond numbers after the atom names in *smibuf*.
    ;; We have to reverse the list of ring-closing bonds because
    ;; they're consed on in the order in which they're generated.
    (loop for (nil ring-closing-bonds smi-i) in cycles
	  do
	  (loop for bond-i in (nreverse ring-closing-bonds)
		for s-i = (prin1-to-string bond-i)
		do
		(setq *smibuf*
		      (concat (subseq *smibuf* 0 (+ smi-i offset))
			      s-i
			      (subseq *smibuf* (+ smi-i offset))
			      ) )
		(incf offset (length s-i))
		) )

    ;; Adjust *smi-i* to reflect the inserts we just made in the buffer.
    (incf *smi-i* offset)
    ) )
