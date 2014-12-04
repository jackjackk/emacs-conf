;; * Helm

(require 'helm-config)

(global-set-key (kbd "C-`") 'helm-for-files)
(global-set-key (kbd "C-~") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "s-`") 'helm-semantic-or-imenu)
(setq helm-locate-command "locate %s -e -A --regex %s")

