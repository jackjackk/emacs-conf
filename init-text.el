
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

(pending-delete-mode 1)

(setq-default fill-column 79)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun progmodes-hooks ()
  "Hooks for programming modes"
  (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))
(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)))

(put 'erase-buffer 'disabled nil)

(defun copy-whole-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))
(global-set-key (kbd "C-x C-a")  'copy-whole-buffer)

(fset 'clone-previous-line
   [up ?\C-a ?\C-k ?\C-y down ?\C-a ?\C-y ?\C-a])
(global-set-key (kbd "C-S-z")  'clone-previous-line)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
