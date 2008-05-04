Just once, run ./gen.sh to generate init.lisp, jinit.lisp and linit.lisp.

Then, to get started, choose ONE of:
- ATMS: load init.lisp and one of coverage.lisp, coverage2.lisp, check-growth.lisp
- JTMS: load jinit.lisp and jcoverage.lisp
- LTMS: load linit.lisp and lcoverage.lisp

You can then load a specific experiment file, like organism-ex.lisp or
real-ex.lisp, which comes with real-not-produced-j.lisp (for JTMS) and
real-not-produced-l.lisp (for LTMS) to be loaded afterwards.
