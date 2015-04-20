
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes"))
(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)
(setq default-frame-alist
       `((background-color . "black")
         (foreground-color . "grey90")))

(cond ((eq window-system 'w32)
           (set-face-font 'default "Consolas-16.0:antialias=natural"))
       (t
           (set-face-font 'default "DejaVu Sans Mono-12.0:antialias=natural")))

(setq-default truncate-lines t)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
