#/bin/bash

cd ..
cd ..
trunk="`pwd`/"
cd BPS
bps="`pwd`/"
cd utils

echo "(defvar *trunk-home* \"$trunk\")" >init.lisp
echo "(defvar *bps-home* \"$bps\")" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/loader.lisp\"))" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/bps-init.lisp\"))" >>init.lisp

