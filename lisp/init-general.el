
(defalias 'yes-or-no-p 'y-or-n-p)

(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

;(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

(setq dired-listing-switches "-alh")

(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'enlarge-window)
(global-set-key (kbd "C-S-<up>") 'shrink-window)

;(desktop-save-mode 1)

(defun save-all () (interactive) (save-some-buffers t))

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

(global-set-key (kbd "<C-tab>") 'other-window)

(fset 'other-window-reverse
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("-1o" 0 "%d")) arg)))
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-window-reverse)

(winner-mode 1)

(defun delete-window-switch-previous()
  "Delete a window and give focus to the previous window"
  (interactive)
  (call-interactively 'delete-window)
  (call-interactively 'other-window-reverse)
)
(global-set-key (kbd "C-0") 'delete-window-switch-previous)

(defun expand-window()
  "Update current workgroup config, then delete other windows"
  (interactive)
;  (call-interactively 'wg-update-workgroup)
  (delete-other-windows)
)
(global-set-key (kbd "C-1") 'expand-window)

(windmove-default-keybindings)

(add-hook 'after-init-hook 'global-company-mode)

(display-battery-mode 1)
