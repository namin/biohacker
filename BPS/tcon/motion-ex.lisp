(create-tcon
 "1D Motion"
 :prototype-file (make-bps-source-file-name (make-bps-path "tcon") "motion"))

(create 'm '1d-uniform-motion)

(set-parameter (>> xstart m) 0.0)
(set-parameter (>> velocity m) 3.2)

(known? (>> xend m))

(constraint-values (>> m))

(set-parameter (>> deltat m) 24.0)

(what-is (>> xend m))

(why (>> xend m))

(premises (>> xend m))

(forget-parameter (>> deltat m))

(set-parameter (>> deltat m) 5000)

(what-is (>> deltat m))