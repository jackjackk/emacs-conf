#!/bin/sh
# -*- mode: shell-script -*-
#
# tangle files with org-mode
#
DIR=$(cygpath -m $(pwd))
DIRORG=elpa/org-plus-contrib*
ARGS=*org

# wrap each argument in the code required to call tangle on it
for i in ${ARGS[@]}; do
    FILES="$FILES \"$i\""
done
echo $DIR
emacs -Q --batch \
--eval "(progn
(add-to-list 'load-path (expand-file-name \"~/${DIRORG}/\"))
(require 'org)(require 'ob)(require 'ob-tangle)
(mapc (lambda (file)
       (find-file (expand-file-name file \"$DIR\"))
       (org-babel-tangle)
       (kill-buffer)) '($FILES)))" # 2>&1 |grep tangled