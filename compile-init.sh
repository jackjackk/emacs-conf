#!/bin/sh
# -*- mode: shell-script -*-
#
# tangle files with org-mode
#
DIR=$(cygpath -m $(pwd))
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
emacs -Q --batch \
--eval "(progn
(add-to-list 'load-path (expand-file-name \"~/${DIRORG}/\"))
(require 'org)(require 'ob)(require 'ob-tangle)
(mapc (lambda (file)
       (find-file (expand-file-name file \"$DIR\"))
       (org-babel-tangle)
       (kill-buffer)) '($FILES)))" # 2>&1 |grep tangled
