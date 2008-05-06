; load lcoverage.lisp
; load real-ex.lisp

(setq *gcs* '(L-ALPHA-ALANINE ARG ASN L-ASPARTATE CYS GLN GLT GLY HIS ILE LEU LYS MET PHE PRO SER THR TRP TYR VAL DATP TTP DGTP DCTP ATP UTP GTP CTP L-1-PHOSPHATIDYL-ETHANOLAMINE CARDIOLIPIN L-1-PHOSPHATIDYL-GLYCEROL C6 BISOHMYR-GLC ADP-L-GLYCERO-D-MANNO-HEPTOSE KDO UDP-GLUCOSE UDP-GALACTOSE DTDP-RHAMNOSE GDP-MANNOSE N-ACETYL-D-GLUCOSAMINE))

; essential compounds not produced
(remove-if #'(lambda (c) (true? `(compound ,c))) *gcs*)
;(C6 KDO GDP-MANNOSE)

(load-debug-rules)

;; This will blow your mind :)
(start-investigating-experiment)
(setq correction 
      (needs 'growth :TRUE '((reaction-enabled ?r) (nutrient ?c))))
(stop-investigating-experiment)

(setq correction
      (sort-fact-sets correction))
;; and correction is now
#|
(((NUTRIENT ARABINOSE-5P) (NUTRIENT C1) (NUTRIENT GDP-MANNOSE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT C1) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT C5) (NUTRIENT GDP-MANNOSE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT C5) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT C6) (NUTRIENT GDP-MANNOSE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT C6) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT GDP-MANNOSE) (NUTRIENT UDP-AA-GLUTAMATE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT GDP-MANNOSE) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT GDP-MANNOSE) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT GDP-MANNOSE) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT GDP-MANNOSE) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AA-GLUTAMATE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT ARABINOSE-5P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT C1) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO))
 ((NUTRIENT C1) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P))
 ((NUTRIENT C1) (NUTRIENT KDO) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT C1) (NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT C5) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO))
 ((NUTRIENT C5) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P))
 ((NUTRIENT C5) (NUTRIENT KDO) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT C5) (NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT C6) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO))
 ((NUTRIENT C6) (NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P))
 ((NUTRIENT C6) (NUTRIENT KDO) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT C6) (NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO) (NUTRIENT UDP-AA-GLUTAMATE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT GDP-MANNOSE) (NUTRIENT KDO-8P) (NUTRIENT UDP-AA-GLUTAMATE))
 ((NUTRIENT KDO) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT KDO) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT KDO) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AA-GLUTAMATE))
 ((NUTRIENT KDO) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT KDO) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AAGM-DIAMINOHEPTANEDIOATE))
 ((NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-ACETYLMURAMOYL-ALA))
 ((NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-MURNAC-TETRAPEPTIDE))
 ((NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-N-ACETYLMURAMATE))
 ((NUTRIENT KDO-8P) (NUTRIENT MANNOSE-1P) (NUTRIENT UDP-AA-GLUTAMATE)))
|#

(defmacro tofile (filename &body body)
  `(with-open-file (file ,filename
		    :direction :output
		    :if-exists :supersede)
     (let ((*standard-output* file))
       ,@body) ))

(setq *nutrients* (mapcar #'cadr (remove-duplicates (apply #'append correction))))
(tofile "nutrient-list.txt" (loop for cpd in *nutrients* do (format t "~A	10~%" cpd)))