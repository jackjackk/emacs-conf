;; * Start-up

;; ** Start server
(server-start)

;; ** Remove annoying startup message
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; ** Maximize window at startup
;(defun toggle-full-screen () (interactive) (shell-command "%APPDATA%/.emacs.d/emacs_fullscreen.exe"))
;(global-set-key (kbd "M-<f11>") 'toggle-full-screen)
;(add-hook 'window-setup-hook 'toggle-full-screen)
