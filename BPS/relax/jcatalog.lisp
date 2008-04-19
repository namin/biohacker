;; -*- Mode: Lisp; -*-

;;;; Junction catalog
;; Last edited 1/29/93, by KDF

;; Copyright (c) 1988, 1989, 1991 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Based on Figure 3-14, Page 56, Winston.

(Junction-Labelling Ell 1 :LEFT < :RIGHT <)
(Junction-Labelling Ell 2 :LEFT > :RIGHT >)
(Junction-Labelling Ell 3 :LEFT + :RIGHT >)
(Junction-Labelling Ell 4 :LEFT > :RIGHT +)
(Junction-Labelling Ell 5 :LEFT - :RIGHT <)
(Junction-Labelling Ell 6 :LEFT < :RIGHT -)

(Junction-Labelling Fork 1 :LEFT + :RIGHT + :BOTTOM +)
(Junction-Labelling Fork 2 :LEFT - :RIGHT - :BOTTOM -)
(Junction-Labelling Fork 3 :LEFT > :RIGHT > :BOTTOM -)
(Junction-Labelling Fork 4 :LEFT - :RIGHT < :BOTTOM <)
(Junction-Labelling Fork 5 :LEFT < :RIGHT - :BOTTOM >)

(Junction-Labelling Tee 1 :LEFT > :RIGHT > :BOTTOM +)
(Junction-Labelling Tee 2 :LEFT > :RIGHT > :BOTTOM -)
(Junction-Labelling Tee 3 :LEFT > :RIGHT > :BOTTOM <)
(Junction-Labelling Tee 4 :LEFT > :RIGHT > :BOTTOM >)

(Junction-Labelling Arrow 1 :LEFT > :RIGHT > :BOTTOM +)
(Junction-Labelling Arrow 2 :LEFT - :RIGHT - :BOTTOM +)
(Junction-Labelling Arrow 3 :LEFT + :RIGHT + :BOTTOM -)
