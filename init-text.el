;; * Text editing

;; ** Tab indentation
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; ** Overwrite selected region when typing after selection
(pending-delete-mode 1)

;; ** Fill-column in text-mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ** Copying
(defun copy-whole-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; ** Clean-up
; Untabify
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

; Indent
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

; Indent + Untabify + Strip
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

; Hook for code text
(defun progmodes-hooks ()
  "Hooks for programming modes"
  (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))
(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)))

;; ** Enable disabled commands

;; *** Restrict editing to region (C-x n n, C-x n w)
(put 'narrow-to-region 'disabled nil)

;; *** Convert region to upper case (C-x C-u)
(put 'upcase-region 'disabled nil)

;; *** Convert region to lower case (C-x C-l)
(put 'downcase-region 'disabled nil)

;; *** Erase buffer
(put 'erase-buffer 'disabled nil)
