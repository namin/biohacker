NOTES ON THE CODE FOR "BUILDING PROBLEM SOLVERS"

by Ken Forbus and Johan de Kleer

Version of October 23, 1993

------------------------------------------------------------------------
HOW TO GET THE CODE
------------------------------------------------------------------------
All the code described in the book is available via anonmyous ftp.
(If you do not have internet access, MIT Press will, for a fee, send
you a floppy.  Please contact them directly about this.  Please do not
ask us for floppies, we simply do not have the facilities to process
such requests.)  The ftp sites the code is currently installed on are:

	multivac.ils.nwu.edu: pub/BPS/
	parcftp.xerox.com:/pub/bps/ 

There are three formats: a unix tar file (compressed and
uncompressed), an MS-DOS zip archive, and a Macintosh Stuffit archive.
Please be sure to ftp the files you need in binary mode.

------------------------------------------------------------------------
HOW THE CODE IS ORGANIZED
------------------------------------------------------------------------

The code is contained in the following eleven directories:

	atms	Assumption-based truth maintenance system, inference engine,
			planner and examples.
	cps	Simple search engine with examples.
	ftre	Faster pattern-directed inference system plus examples
	gde	General Diagnostic Engine, ATMS-based constraint language
			 plus examples.
	jtms	Justification-based TMS, inference engine, symbolic 
			integration system and examples. 
	ltms	Logic-based TMS, inference engine and examples. 
	relax	Symbolic relaxation system and examples.
	tcon	Constraint language and examples.
	tgizmo  Simple Qualitative Process theory system plus examples.
	tre	Pattern-directed inference system plus examples
	utils	Loading and listing programs.

A complete listing of files is at the end of this note, along with the
legalese regarding their use.

------------------------------------------------------------------------
USING THE CODE
------------------------------------------------------------------------

Please see the appendix in Building Problem Solvers for details.

------------------------------------------------------------------------
COMPATIBILITY NOTES
------------------------------------------------------------------------

The programs in this book were developed on the following platforms:

	Symbolics Common Lisp, on 36XX's and XL1200's
	Lucid Common Lisp on IBM RT's, IBM RS/6000's.

In addition, various subsets of the code have been used on 

	Franz Common Lisp, various platforms
	Lucid Common Lisp, various platforms
	Kyoto Common Lisp, varous platforms
	Symbolics CLOE on 386's
	Macintosh Common Lisp, various Apple computers
	Allegro CL/PC, on 486's.

We have taken great pains to write our code to be as portable as
possible.  However, glitches can occur.  So far, every problem we know
about can be solved by placing the code in its own package, rather
than the user package.  (We recommend this in general anyway, but as
explained in the book, we left everything in the package
:COMMON-LISP-USER for simplicity.)

Here is the list of incompatibilities that we know of, and suggestions
for fixes.  Others will be added as they are discovered.

	MACINTOSH COMMON LISP: We use RLET to bind pattern variables
in rules.  The symbol ccl::rlet is exported into the user package,
which thus causes an error.  You can either tell the lisp to allow the
redefinition of RLET, or use a seperate package.

	ALLEGRO CL/PC: The symbols TRUE and FALSE exported to the user
package from an internal package as constants.  In this particular
port of Allegro Common Lisp, this causes problems even when using TRUE
and FALSE as field names (in the LTMS), even with a non-empty conc
name.  You can either rename the fields or put the LTMS in a package
other than COMMON-LISP-USER.

------------------------------------------------------------------------
BUGS
------------------------------------------------------------------------

The best way to report bugs is by sending electronic mail to

	bug-bps@ils.nwu.edu

Mail to this address is automatically forwarded to both Forbus and de Kleer.

------------------------------------------------------------------------
LEGAL NOTICE
------------------------------------------------------------------------

American society is growing increasingly litigeous; we regret the
necessity of the following notice.  That is, the copyright notice
below and the paragraph which follows must be included in any separate
copy of the code from this book:

Copyright (c) 1986-1993 Kenneth D. Forbus, Johan de Kleer and Xerox
Corporation.  All Rights Reserved.

Use, reproduction, and preparation of derivative works are permitted.
Any copy of this software or of any derivative work must include the
above copyright notice and this paragraph.  Any distribution of this
software or derivative works must comply with all applicable United
States export control laws.  This software is made available as is, and
Kenneth D. Forbus, Johan de Kleer and Xerox Corporation disclaim all
warranties, express or implied, including without limitation the implied
warranties of merchantability and fitness for a particular purpose, and
notwithstanding any other provision contained herein, any liability for
damages resulting from the software or its use is expressly disclaimed,
whether arising in contract, tort (including negligence) or strict
liability, even if Kenneth D. Forbus, Johan de Kleer or Xerox
Corporation is advised of the possibility of such damages.

------------------------------------------------------------------------
COMPLETE LISTING OF BPS FILES
------------------------------------------------------------------------
./atms/adata.lisp
./atms/ainter.lisp
./atms/unify.lisp
./atms/aplanr.lisp
./atms/plan-e.lisp
./atms/arules.lisp
./atms/atest.lisp
./atms/plan-a.lisp
./atms/atms.lisp
./atms/atre.lisp
./atms/funify.lisp
./atms/atret.lisp
./atms/blocks.lisp
./atms/bcode.lisp
./cps/algebra.lisp
./cps/variants.lisp
./cps/boston.lisp
./cps/search.lisp
./cps/cps.lisp
./cps/match.lisp
./cps/simplify.lisp
./cps/subways.lisp
./ftre/fdata.lisp
./ftre/finter.lisp
./ftre/unify.lisp
./ftre/fnd-ex.lisp
./ftre/funify.lisp
./ftre/fnd.lisp
./ftre/fqrule.lisp
./ftre/fqueens.lisp
./ftre/frules.lisp
./ftre/ftre.lisp
./gde/atcon.lisp
./gde/gde.lisp
./gde/condef.lisp
./gde/diagrams.lisp
./gde/2bit.txt
./gde/polyex.txt
./jtms/funify.lisp
./jtms/jdata.lisp
./jtms/unify.lisp
./jtms/jinter.lisp
./jtms/jqrule.lisp
./jtms/jqueens.lisp
./jtms/match.lisp
./jtms/jrules.lisp
./jtms/jsaint.lisp
./jtms/jtre.lisp
./jtms/jsops.lisp
./jtms/simplify.lisp
./jtms/jsrules.lisp
./jtms/jtest.lisp
./jtms/jtms-ex.lisp
./jtms/jtms.lisp
./jtms/jtms.lisp
./ltms/cltms.lisp
./ltms/cwa.lisp
./ltms/dds.lisp
./ltms/unify.lisp
./ltms/funify.lisp
./ltms/indirect.lisp
./ltms/laccept.lisp
./ltms/ldata.lisp
./ltms/ltre.lisp
./ltms/linter.lisp
./ltms/lrules.lisp
./ltms/ltms-ex.lisp
./ltms/ltms.lisp
./ltms/setrule.lisp
./relax/allen.lisp
./relax/cube.lisp
./relax/jcatalog.lisp
./relax/scene.lisp
./relax/stack.lisp
./relax/timedb.lisp
./relax/wedge.lisp
./relax/waltzer.lisp
./tcon/tcon.lisp
./tcon/condef.lisp
./tcon/debug.lisp
./tcon/intex.txt
./tcon/motion.lisp
./tcon/polybox.lisp
./tcon/suspend.lisp
./tgizmo/mi.lisp
./tgizmo/debug.lisp
./tgizmo/defs.lisp
./tgizmo/ex1.lisp
./tgizmo/ineqs.lisp
./tgizmo/ex2.lisp
./tgizmo/tnst.lisp
./tgizmo/tgizmo.lisp
./tgizmo/mlang.lisp
./tgizmo/ex3.lisp
./tgizmo/resolve.lisp
./tgizmo/ex4.lisp
./tgizmo/ex5.lisp
./tgizmo/ex6.lisp
./tgizmo/ex7.lisp
./tgizmo/states.lisp
./tgizmo/laws.lisp
./tgizmo/psvs.lisp
./tre/data.lisp
./tre/rules.lisp
./tre/unify.lisp
./tre/tinter.lisp
./tre/treex1.lisp
./tre/tre.lisp
./utils/lst.lisp
./utils/loader.lisp
------------------------------------------------------------------------
------------------------------------------------------------------------