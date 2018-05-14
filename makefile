EMACS := $(shell sh -c '[ -f  /Applications/Emacs.app/Contents/MacOS/Emacs ] && echo /Applications/Emacs.app/Contents/MacOS/Emacs || echo emacs')

ORG_FILES := $(wildcard init*.org)
LISP_FILES := $(patsubst %.org,lisp/%.el,$(ORG_FILES))

tangle: init.el $(LISP_FILES)

init.el: README.org
	$(EMACS) -Q --batch -L elpa/org-plus-contrib* --eval '(progn (require (quote org)) (find-file "$<") (org-babel-tangle) (kill-buffer))'

lisp/init-%.el: init-%.org
	$(EMACS) -Q --batch -L elpa/org-plus-contrib* --eval '(progn (require (quote org)) (find-file "$<") (org-babel-tangle) (kill-buffer))'
