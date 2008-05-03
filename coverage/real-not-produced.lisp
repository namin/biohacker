; load coverage2.lisp
; load real-ex.lisp

(setq *env* (environment-of (fetch '(experiment . ?x))))

(setq *gcs* '(L-ALPHA-ALANINE ARG ASN L-ASPARTATE CYS GLN GLT GLY HIS ILE LEU LYS MET PHE PRO SER THR TRP TYR VAL DATP TTP DGTP DCTP ATP UTP GTP CTP L-1-PHOSPHATIDYL-ETHANOLAMINE CARDIOLIPIN L-1-PHOSPHATIDYL-GLYCEROL C6 BISOHMYR-GLC ADP-L-GLYCERO-D-MANNO-HEPTOSE KDO UDP-GLUCOSE UDP-GALACTOSE DTDP-RHAMNOSE GDP-MANNOSE N-ACETYL-D-GLUCOSAMINE))

; essential compounds not produced
(remove-if #'(lambda (c) (in? `(compound ,c) *env*)) *gcs*)
