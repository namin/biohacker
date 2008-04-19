;; -*- Mode: Lisp; -*-

;;;; A definition of Allen's temporal logic for Tlogic/Waltzer.
;;; Last edited 1/29/93, by KDF

;; Copyright (c) 1988 --- 1992 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;; First constants defining the relational vocabulary.

(defTemporalRelation <)
(defTemporalRelation  >)
(defTemporalRelation =)
(defTemporalRelation m)
(defTemporalRelation mi)
(defTemporalRelation d)
(defTemporalRelation di)
(defTemporalRelation o)
(defTemporalRelation oi)
(defTemporalRelation s)
(defTemporalRelation si)
(defTemporalRelation f)
(defTemporalRelation fi)

;;;; Transitivity table

;; Adapted from Allen, CACM, Vol 26, No. 11, page 836

;; Involving <
(t-transitivity < < <)
(t-transitivity < = <)
(t-transitivity < d < o m d s)
(t-transitivity < di <)
(t-transitivity < o <)
(t-transitivity < oi < o m d s)
(t-transitivity < m <)
(t-transitivity < mi < o m d s)
(t-transitivity < s <)
(t-transitivity < si <)
(t-transitivity < f < o m d s)
(t-transitivity < fi <)

(t-transitivity > > >)
(t-transitivity > = >)
(t-transitivity > d > oi mi d f)
(t-transitivity > di >)
(t-transitivity > o > oi mi d f)
(t-transitivity > oi >)
(t-transitivity > m > oi mi d f)
(t-transitivity > mi >)
(t-transitivity > s > oi mi d f)
(t-transitivity > si >)
(t-transitivity > f >)
(t-transitivity > fi >)

;;; Table, continued.
;; D and DI

(t-transitivity d < <)
(t-transitivity d > >)
(t-transitivity d = d)
(t-transitivity d d d) 
(t-transitivity d o < o m d s)
(t-transitivity d oi > oi mi d f)
(t-transitivity d m <)
(t-transitivity d mi >)
(t-transitivity d s d)
(t-transitivity d si > oi mi d f)
(t-transitivity d f d)
(t-transitivity d fi < o m d s)

(t-transitivity di < < o m di fi)
(t-transitivity di > > oi di mi si)
(t-transitivity di = di)
(t-transitivity di d o oi d s f di si fi)
(t-transitivity di di di)
(t-transitivity di o o di fi)
(t-transitivity di oi oi di si)
(t-transitivity di m o di fi)
(t-transitivity di mi oi di si)
(t-transitivity di s di fi o)
(t-transitivity di si di)
(t-transitivity di f fi si oi)
(t-transitivity di fi di)

;;; Table, continued
;; O and OI 		     

(t-transitivity o < <)
(t-transitivity o > > oi di mi s)
(t-transitivity o d o d s)
(t-transitivity o di < o m di f)
(t-transitivity o o < o m)
(t-transitivity o oi o oi = d f s di fi si)
(t-transitivity o m <)
(t-transitivity o mi oi di si)
(t-transitivity o s o)
(t-transitivity o si di fi o)
(t-transitivity o f d s o)
(t-transitivity o fi < o m)
(t-transitivity o = o)

(t-transitivity oi = oi)
(t-transitivity oi < < o m di fi)
(t-transitivity oi > >)
(t-transitivity oi d oi d f)
(t-transitivity oi di < oi mi di si)
(t-transitivity oi o o oi d s f di si fi)
(t-transitivity oi m o di fi)
(t-transitivity oi mi >)
(t-transitivity oi s oi d f)
(t-transitivity oi si oi > mi)
(t-transitivity oi f oi)
(t-transitivity oi fi < o m)

;;;; M and MI

(t-transitivity m < <)
(t-transitivity m > > oi mi
		     di si)
(t-transitivity m d o d s)
(t-transitivity m di <)
(t-transitivity m o <)
(t-transitivity m oi o d s)
(t-transitivity m m <)
(t-transitivity m mi f fi =)
(t-transitivity m s m)
(t-transitivity m si m)
(t-transitivity m f d s o)
(t-transitivity m fi <)
(t-transitivity m = m)

(t-transitivity mi < o m di fi)
(t-transitivity mi = mi)
(t-transitivity mi > >)
(t-transitivity mi d oi d f)
(t-transitivity mi di >)
(t-transitivity mi o oi d f)
(t-transitivity mi oi >)
(t-transitivity mi m s si =)
(t-transitivity mi mi >)
(t-transitivity mi s d f oi)
(t-transitivity mi si >)
(t-transitivity mi f mi)
(t-transitivity mi fi mi)

;;;; S and SI

(t-transitivity s < <)
(t-transitivity s > >)
(t-transitivity s d d)
(t-transitivity s di < o m di fi)
(t-transitivity s o < o m)
(t-transitivity s oi oi d f)
(t-transitivity s m <)
(t-transitivity s mi mi)
(t-transitivity s s s)
(t-transitivity s si s si =)
(t-transitivity s f d)
(t-transitivity s fi < m o)
(t-transitivity s = s)

(t-transitivity si < < o m di fi)
(t-transitivity si > >)
(t-transitivity si d oi d f)
(t-transitivity si di di)
(t-transitivity si o o di fi)
(t-transitivity si oi oi)
(t-transitivity si m o di fi)
(t-transitivity si mi mi)
(t-transitivity si s s si =)
(t-transitivity si si si)
(t-transitivity si f oi)
(t-transitivity si fi di)
(t-transitivity si = si)

;;;; F and FI

(t-transitivity f < <)
(t-transitivity f > >)
(t-transitivity f d d)
(t-transitivity f di > oi mi di si)
(t-transitivity f o o d s)
(t-transitivity f oi > oi mi)
(t-transitivity f m m)
(t-transitivity f mi >)
(t-transitivity f s d)
(t-transitivity f si > oi mi)
(t-transitivity f f f)
(t-transitivity f fi f fi =)
(t-transitivity f = f)

(t-transitivity fi < <)
(t-transitivity fi > > oi mi di si)
(t-transitivity fi d o d s)
(t-transitivity fi di di)
(t-transitivity fi o o)
(t-transitivity fi oi oi
		     di si)
(t-transitivity fi m m)
(t-transitivity fi mi si oi di)
(t-transitivity fi s o)
(t-transitivity fi si di) 
(t-transitivity fi f f fi =)
(t-transitivity fi fi fi)
(t-transitivity fi = fi)

(t-transitivity = = =)
(t-transitivity = < <)
(t-transitivity = > >)
(t-transitivity = d d)
(t-transitivity = di di)
(t-transitivity = o o)
(t-transitivity = oi oi)
(t-transitivity = m m)
(t-transitivity = mi mi)
(t-transitivity = s s)
(t-transitivity = si si)
(t-transitivity = f f)
(t-transitivity = fi fi)
;; END OF TABLE
