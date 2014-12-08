;; * General behaviour

;; ** One-character yes-no answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Change backup behavior to save in a directory
;;Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

;; ** Revert buffer w/o confirm
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; ** Navigation
(global-set-key (kbd "<C-tab>") 'other-window)

(fset 'other-window-reverse
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("-1o" 0 "%d")) arg)))
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-window-reverse)

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

;; ** Resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
