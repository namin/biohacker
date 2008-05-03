#/bin/bash

cd ..
trunk="`pwd`/"
cd BPS
bps="`pwd`/"
cd ..
cd coverage

echo "; Load the BPS-related code needed by all coverage files" >init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/init.lisp\"))" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"atms/atre.lisp\"))" >>init.lisp
echo "(compile-atre)" >>init.lisp


