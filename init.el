
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
;                     bm
                     zotelo
                     simple-httpd
                     js2-mode
                     skewer-mode
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((zotero-collection . #("103" 0 3 (name "Enel")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
