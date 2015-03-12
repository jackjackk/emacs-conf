(add-to-list 'load-path "~/.emacs.d/")

;; * Custom 
(load-library "init-startup.el")
(load-library "init-general.el")
(load-library "init-visual.el")
(load-library "init-text.el")
(load-library "init-os.el")
(load-library "init-python.el")
(load-library "init-latex.el")

;; * Extra packages
;; ** from repositories
(setq package-list '(helm
                     yasnippet
                     expand-region
                     multiple-cursors
                     org
;                     org-redmine
                     org-pomodoro
                     org2blog
;                     websocket
;                     ein
                     bm
                     zotelo
))
;; ** from local subfolders 
(setq other-package-list '(gams
                           matlab))
(load-library "init-packages.el")
