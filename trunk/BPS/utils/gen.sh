#/bin/bash

cd ..
bps="`pwd`/"
cd utils

echo "(defvar *bps-home* \"$bps\")" >init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/loader.lisp\"))" >>init.lisp
echo "(load (concatenate 'string \"$bps\" \"utils/bps-init.lisp\"))" >>init.lisp

