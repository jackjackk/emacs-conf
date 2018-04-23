#!/bin/sh
# -*- mode: shell-script -*-
#
# tangle files with org-mode
#
command -v cygpath >/dev/null 2>&1 && DIR=$(cygpath -m $(pwd)) || DIR=$(pwd)
DIRORG=elpa/org-plus-contrib*
if [ $# -eq 0 ]; then
    ARGS=*org
else
    ARGS="$@"
fi
# wrap each argument in the code required to call tangle on it
for i in ${ARGS[@]}; do
    if [ ${i: -4} == ".org" ]; then
        FILES="$FILES \"$i\""
    fi
done
/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -l elpa/org-plus-contrib*/org \
--eval "(progn
(mapc (lambda (file)
       (find-file (expand-file-name file \"$DIR\"))
       (org-babel-tangle)
       (kill-buffer)) '($FILES)))" # 2>&1 |grep tangled
