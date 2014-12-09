;; * Helm

(require 'helm-config)

(global-set-key (kbd "C-`") 'helm-for-files)
(global-set-key (kbd "C-~") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "s-`") 'helm-semantic-or-imenu)

;; Only if not in Windows use locate
(cond ( (eq window-system 'w32)
		(setq helm-locate-command "es %s %s"))
	  (t
        (setq helm-locate-command "locate %s -e -A --regex %s")))


;; ** Navigation
(defun switch-to-buffer-other-window-vertical()
  "Select a buffer in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-2") 'switch-to-buffer-other-window-vertical)

(defun find-file-other-window-vertical()
  "Edit a file in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-@") 'find-file-other-window-vertical)

(defun switch-to-buffer-other-window-horizontal()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-3") 'switch-to-buffer-other-window-horizontal)

(defun find-file-other-window-horizontal()
  "Edit a file in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-#") 'find-file-other-window-horizontal)

(defun my-switch-to-buffer-other-window()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-4") 'my-switch-to-buffer-other-window)

(defun my-find-file-other-window()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-$") 'my-find-file-other-window)

(defun my-switch-to-buffer-other-window-reverse()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window-reverse)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-5") 'my-switch-to-buffer-other-window-reverse)

(defun my-find-file-other-window-reverse()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window-reverse)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-%") 'my-find-file-other-window-reverse)
