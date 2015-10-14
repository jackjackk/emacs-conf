
(add-to-list 'load-path "~/.emacs.d/lisp")

(load-library "init-startup.el")
(load-library "init-general.el")
(load-library "init-visual.el")
(load-library "init-text.el")
(load-library "init-os.el")
(load-library "init-python.el")
(load-library "init-latex.el")

;; * Extra packages
;; ** from repositories
(setq package-list '(company
                     helm
                     yasnippet
                     expand-region
                     multiple-cursors
                     org
;                     org-redmine
                     org-pomodoro
;                     org2blog
;                     websocket
;                     ein
;                     bm
                     zotelo
;                     simple-httpd
                     js2-mode
 ;                    skewer-mode
                     exec-path-from-shell
                     projectile
                     helm-projectile
                     transpose-frame
;                     elpy
                     spray
                     ;image+
                     eimp
                     auctex
                     cdlatex
                     latex-preview-pane
                     ;org-beautify-theme
 ;                    android-mode
                     neotree
                     auctex-latexmk
))
;; ** from local subfolders 
(setq other-package-list '(thing-edit
                           gams
                           ox-wk
                           ;matlab
                           ;speedread
                           org-checklist
                           org-bullets
                           ))
(load-library "init-packages.el")
