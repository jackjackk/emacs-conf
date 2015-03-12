;; * Text editing

;; ** Tab indentation
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "<backtab>") 'indent-relative)

;; ** Overwrite selected region when typing after selection
(pending-delete-mode 1)

;; ** Fill-column in text-mode
(setq-default fill-column 79)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ** Grepping
;(keyboard-translate ?\C-i ?\H-i)
;(global-set-key [?\H-i] 'grep-find)
;(grep-apply-setting 'grep-command "grep -r --include=\"!\" -nH -e ! .")
;(setq grep-command  "grep -r --include=\"!\" -nH -e ! .")

;; ** Copying
(defun copy-whole-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))
;(global-set-key (kbd "C-x C-a")  'copy-whole-buffer)

(fset 'clone-previous-line
   [up ?\C-a ?\C-k ?\C-y down ?\C-a ?\C-y ?\C-a])
(global-set-key (kbd "C-S-z")  'clone-previous-line)

(fset 'copy-line
   "\C-a\C-k\C-y")
(global-set-key (kbd "C-z")  'copy-line)

(fset 'copy-sexp
   [?\C-  C-M-right escape ?w C-M-left])
(global-set-key (kbd "C-M-j")  'copy-sexp)

(fset 'copy-section-around-keeping-cursor-position
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([C-f1 67108925 134217847 C-f2] 0 "%d")) arg)))
(global-set-key (kbd "C-\\") 'copy-section-around-keeping-cursor-position)

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
