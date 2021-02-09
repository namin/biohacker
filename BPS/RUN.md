Running _Building Problem Solvers_

# One-Time Installation

- Create `init.lisp` by:
  - `cd utils`
  - `./gen.sh`

- Install [Allegro Common Lisp](https://franz.com/downloads/clp/survey)
  - add Allegro CL to your path (your mileage may vary):
    - in `~/.zshrc`, `export PATH=/Applications/AllegroCL64express.app/Contents/Resources/:$PATH`

- Install Slime in Emacs
  - `package-install slime`
  - Then add this to your `emacs.d/init.el`:
```
;; slime

;; update this path to the correct location.
(setq *slime-path* "~/.emacs.d/elpa/slime-20200810.224")
(add-to-list 'load-path *slime-path*)
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path *slime-path*)
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
(setq inferior-lisp-program "alisp")
```

# Running

- In Emacs, go to `utils/my.lisp`.
- Then call `M-x slime`.
- Now go to a directory of choise, e.g., `atms`. And load the `my.lisp` from there as needed.
