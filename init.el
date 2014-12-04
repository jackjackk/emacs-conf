(add-to-list 'load-path "~/.emacs.d/")

;; * Custom 
(load-library "init-startup.el")
(load-library "init-general.el")
(load-library "init-visual.el")
(load-library "init-text.el")

;; * Extra packages from repositories
(setq package-list '(helm
		     yasnippet
		     expand-region
		     multiple-cursors))
(load-library "init-packages.el")
