;; * Visual

;; ** Visual line mode for texts
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; ** Truncate lines
(setq-default truncate-lines t)

;; ** Turn on paren match highlighting
(show-paren-mode 1)

;; ** Turn off the scroll bar, menu bar, and tool bar
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; ** Frame title tweak
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; ** Empty line markers
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; ** Color theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes"))
(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)
(setq default-frame-alist
       `((background-color . "black")
         (foreground-color . "grey90")))

;; ** Font
(cond ((eq window-system 'w32)
           (set-face-font 'default "Consolas-16.0:antialias=natural"))
       (T
           (set-face-font 'default "DejaVu Sans Mono-14.0:antialias=natural")))
;(set-face-font 'default "bold DejaVu Sans Mono-12.0:antialias=subpixel")

;; ** Turn on column numbers
(setq column-number-mode t)

;; ** Turn off blinking
(blink-cursor-mode -1)

;; ** Show line/column numbers
(line-number-mode 1)
(column-number-mode 1)
