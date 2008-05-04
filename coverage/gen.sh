#/bin/bash

cd ..
trunk="`pwd`/"
cd BPS
bps="`pwd`/"
cd ..
cd coverage

echo "; Load the BPS-related code needed by all coverage files" >init.lisp
echo "; Load the BPS-related code needed by all jcoverage files" >jinit.lisp
echo "; Load the BPS-related code needed by all lcoverage files" >linit.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/init.lisp\"))" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/init.lisp\"))" >>jinit.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/init.lisp\"))" >>linit.lisp
echo "(load (concatenate 'string \"$bps\" \"atms/atre.lisp\"))" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"jtms/jtre.lisp\"))" >>jinit.lisp
echo "(load (concatenate 'string \"$bps\" \"ltms/ltre.lisp\"))" >>linit.lisp
echo "(compile-atre)" >>init.lisp
echo "(compile-jtre)" >>jinit.lisp
echo "(load-ltre)" >>linit.lisp



