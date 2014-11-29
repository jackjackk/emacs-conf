;; * Server mechanism
(and (>= emacs-major-version 23)
     (defun server-ensure-safe-dir (dir) "Noop" t))
(server-start)

;; * Package Management

;; ** Initialize
(require 'package)
(package-initialize)

;; ** Add user-contributed repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; ** Make sure all default packages are installed
(defvar jack/packages '(alert
                          auto-complete
                          back-button
                          smartrep
                          visible-mark
                          deft
                          highlight
                          list-utils
                          nav-flash
                          org
                          org-cua-dwim
                          org-pomodoro
                          pcache
                          persistent-soft
                          popup
;                          smex
                          stem
                          ucs-utils
                          helm
                          ace-jump-mode
                          yasnippet
                          jump-char
                          key-chord
                          multiple-cursors
                          expand-region
                          ace-jump-buffer
                          dash
                          xml-rpc
                          metaweblog
                          org2blog)
  "Default packages")
(defun jack/packages-installed-p ()
  (loop for pkg in jack/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))
(unless (jack/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jack/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; ** Include extra packages directories
(defvar jack/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path jack/lisp-dir)
(dolist (project (directory-files jack/lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; * Start-up options

;; ** Remove annoying startup message
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; ** Maximize window at startup
(defun toggle-full-screen () (interactive) (shell-command "%APPDATA%/.emacs.d/emacs_fullscreen.exe"))
(global-set-key (kbd "M-<f11>") 'toggle-full-screen)
(add-hook 'window-setup-hook 'toggle-full-screen)

;; ** Visual line mode for texts
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; ** Color in shell-mode with ansi-color

;; * Operating behaviours
;; ** Change backup behavior to save in a directory
;;Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

;; ** One-character yes-no answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Default programs
(setq browse-url-default-browser "firefox.exe")

;; ** Tab indentation
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; ** Truncate lines
(setq-default truncate-lines t)

;; ** Turn on paren match highlighting
(show-paren-mode 1)

;; ** Enable all disabled commands
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; ** Auto revert for image mode
(add-hook 'image-mode-hook (lambda ()
                             (auto-revert-mode 1)
                             (auto-image-file-mode 1)
))

;; ** Overwrite selected region when typing after selection
(pending-delete-mode 1)
;; ** Fill-column in text-mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ** Super key <-> Windows key
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

;; * Appearance

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
(global-font-lock-mode 1)
;(load-theme 'wombat t)
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;; ** Font
;(set-face-font 'default "bold DejaVu Sans Mono-12.0:antialias=subpixel")
(set-face-font 'default "Consolas-16.0:antialias=natural")
;(set-face-font 'default "DejaVu Sans Mono-14.0:antialias=natural")
(setq default-frame-alist
       `((background-color . "black")
         (foreground-color . "grey90")))

;; ** Turn on column numbers
(setq column-number-mode t)

;; ** Turn off blinking
(blink-cursor-mode -1)
;; ** Show line/column numbers
(line-number-mode 1)
(column-number-mode 1)

;; * Utilities

;; ** (expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
(defun er/add-text-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  ;(setq er/try-expand-list (append
  ;                          er/try-expand-list
  ;                          '(mark-paragraph
  ;                            mark-page)))
  (setq er/try-expand-list '(mark-paragraph
                              mark-page))
)
(er/enable-mode-expansions 'python-mode 'er/add-text-mode-expansions)

;; ** (multiple-cursors)
(require 'multiple-cursors)
; When you have an active region that spans multiple lines, the following will
; add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; When you want to add multiple cursors not based on continuous lines, but based on
; keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-<") 'mc/mark-all-like-this)
; To override a mouse event, you will likely have to also unbind the
; `down-mouse` part of the event. Like this:
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
; Or you can do like me and find an unused, but less convenient, binding:
;(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; ** (jump-char)
(require 'jump-char)
(define-key global-map (kbd "M-,") 'jump-char-backward)
(define-key global-map (kbd "M-.") 'jump-char-forward)

;; ** (yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; ** (ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-.") 'ace-jump-mode)
(define-key global-map (kbd "C-,") 'ace-jump-line-mode)

;; ** (ace-jump-buffer)
(require 'ace-jump-buffer)

;; ** (key-chord)
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "fd" 'jump-char-backward)
(key-chord-define-global "fg" 'jump-char-forward)
(key-chord-define-global "fh" 'ace-jump-mode)
(key-chord-define-global "fj" 'ace-jump-line-mode)
(key-chord-define-global "fk" 'ace-jump-buffer)

;; ** (helm)
(require 'helm)

;; ** (worf)
(require 'worf)

;; ** (back-button) Visual navigation through mark rings
; C-x <SPC>/<left>/<right>  navigate in (buffer-local) mark-ring
; C-x C-<SPC>/<left>/<right>  navigate in global-mark-ring
(require 'back-button)
(back-button-mode 1)

;; ** (deft) Quickly browse, filter, and edit plain text notes
(require 'deft)
(setq deft-directory "~/Dropbox/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "C-d") 'deft)
(global-set-key (kbd "C-f") 'deft-find-file)

;; ** (smex) M-x interface with Ido-style fuzzy matching.
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ** (auto-complete) Auto Completion for GNU Emacs
(require 'auto-complete-config)
(ac-config-default)

;; ** (ido) Interactively do things with buffers and files
(ido-mode t)
(setq ido-case-fold t
      ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; ** (outshine) outline with outshine outshines outline
(require 'outorg)
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(setq outshine-use-speed-commands t)
(defvar outline-minor-mode-prefix "\M-#")
;(defun my-outline-easy-bindings ()
;  (require 'outline-mode-easy-bindings nil t))
;(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;(add-hook 'outline-mode-hook 'my-outline-easy-bindings)
;(add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
;(when (require 'outshine nil 'NOERROR)
;  (add-hook 'org-mode-hook
;            (lambda ()
;              ; Redefine arrow keys, since promoting/demoting and moving
;              ; subtrees up and down are less frequent tasks then
;              ; navigation and visibility cycling
;                (org-defkey org-mode-map
;                            (kbd "M-<left>") 'outline-hide-more)
;                (org-defkey org-mode-map
;                            (kbd "M-<right>") 'outline-show-more)
;                (org-defkey org-mode-map
;                            (kbd "M-<up>") 'outline-previous-visible-heading)
;                (org-defkey org-mode-map
;                            (kbd "M-<down>") 'outline-next-visible-heading))
;            'append))
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'python-mode-hook 'outline-minor-mode)
;(require 'python-magic)

;; ** Gnuplot mode
(require 'gnuplot)
; specify the gnuplot executable (if other than /usr/bin/gnuplot)
(setq gnuplot-program "C:/gnuplot/bin/pgnuplot.exe")
; automatically open files ending with .gp or .gnuplot in gnuplot mode
(setq auto-mode-alist 
(append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

;; ** GAMS
(require 'gams)
;;needed for correct coloring in multiline regions of code
(setq jit-lock-chunk-size 50000)
(setq gams:process-command-name "C:/GAMS/win32/24.2/gams.exe")
(setq gams-system-directory "C:/GAMS/win32/24.2/")
;(setq gams-docs-directory "C:/GAMS/win64/23.9/docs/")
;(setq gams:process-command-name "C:/GAMS/win64/24.2/gams.exe")
;(setq gams-system-directory "C:/GAMS/win64/24.2/")
(setq gams-docs-directory "C:/GAMS/win32/24.2/docs/")
(setq gams-indent-on t)
(setq gams-indent-number 4)
(setq gams-template-file "~/.emacs.d/lisp/gams/gams-template.txt")
(setq gams:process-command-option "ll=0 lo=3 pw=32767 ps=0")
(setq gams-statement-upcase nil) ; Use upper case for GAMS statements
(setq gams-dollar-control-upcase nil) ; Use upper case for dollar operations.
(setq gams-docs-view-program "C:/Program Files (x86)/Adobe/Acrobat 11.0/Acrobat/AcroRd32.exe")
(setq gams-lxi-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi.exe")
(setq gams-lxi-import-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi-import.exe")
(setq gams-ol-external-program "~/.emacs.d/lisp/gams/external/gamsolc.exe")
(setq gams-close-double-quotation-always t)
(setq gams-close-single-quotation-always t)
(setq gams-eolcom-symbol-default' "#")
;(setq font-lock-support-mode '((gams-mode . nil) (t . jit-lock-mode)))

;; ** Projectile
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "s-c"))
;(define-key (current-global-map) (kbd "s-p") 'projectile-command-map)
;(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
;(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
;(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
;(define-key projectile-mode-map [?\s-g] 'projectile-grep)
(require 'helm-projectile)
(helm-projectile-on)

;; ** Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-x C-\\"))
(setq wg-morph-on nil)
(workgroups-mode 1)
(wg-load "~/.emacs.d/workgroups.dat")
(add-hook 'kill-emacs-hook 'wg-update-all-workgroups-and-save)

;; ** Transpose-frame
(require 'transpose-frame)

;; ** Org-reveal
(require 'ox-reveal)
(setq org-reveal-root "file:///C:/Dropbox/common/emacs.d/lisp/org-reveal/reveal.js-2.6.1")

;; * Python
(load-library "python")
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
                    (set-variable 'py-indent-offset 4)
                    (set-variable 'indent-tabs-mode nil))))

;; Highlight character at "fill-column" position.
(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'python-mode-hook
          (lambda () (interactive)
            (column-marker-1 fill-column)))

;; Setup for Flymake code checking.
(require 'flymake)
(load-library "flymake-cursor")

;; Script that flymake uses to check code. This script must be
;; present in the system path.
(setq pycodechecker "pychecker")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))
(add-hook 'python-mode-hook 'flymake-mode)

;; Remove trailing whitespace manually by typing C-t C-w.
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-t C-w")
                           'delete-trailing-whitespace)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))
;; * LaTeX

(setq TeX-save-query nil) ;;autosave before compiling
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)


;; * Org-mode
(require 'ox-mediawiki)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(setq org-agenda-files (list "~/Dropbox/org/personal.org"))
; set org-mode as the default mode for .org, .org_archive, and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; ** Latex
(setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

;; ** Clean view
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(defun prettier-org-code-blocks-upper ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\(\+BEGIN_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                                       nil))) 
                            ("\\(\+END_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                                       nil))))))
(defun prettier-org-code-blocks-lower ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\(^[[:space:]]*#\\+begin_src .*[\r\n]\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) "")
                                       nil)))
                            ("\\(^[[:space:]]*#\\+end_src[\r\n]\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) "")
                                       nil))))))
(add-hook 'org-mode-hook 'prettier-org-code-blocks-lower)
(add-hook 'org-mode-hook 'prettier-org-code-blocks-upper)

;; ** Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (emacs-lisp . t) (ditaa . t) (sh . t)))
(setq org-confirm-babel-evaluate nil)
;(setq org-babel-sh-command "C:/Windows/SysWOW64/cmd.exe")
(setq org-babel-sh-command "C:/cygwin/bin/sh.exe")

;; ** Org Key bindings
(global-set-key (kbd "<apps>") (kbd "C-c '"))
(global-set-key (kbd "C-<apps>") (kbd "C-c C-v p"))
(global-set-key (kbd "M-<apps>") (kbd "C-c C-v n"))
(global-set-key (kbd "<f2>") 'outline-previous-visible-heading)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)


;; * Auxiliary functions

;; ** Reverting
(defun revert-buffer-keep-undo (&rest -)
  "Revert buffer but keep undo history."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-visited-file-modtime (visited-file-modtime))
    (set-buffer-modified-p nil)))
;(setq revert-buffer-function 'revert-buffer-keep-undo)
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

;; ** Copying
(defun copy-whole-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))
(fset 'copy-src-block
   [?\C-  ?\C-  ?\C-c ?\C-p down down ?\C-  ?\C-c ?\C-n up ?\M-w ?\C-u ?\C-  ?\C-u ?\C- ])
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

;; ** Misc
; Windows explorer to go to the file in the current buffer
(defun open-folder-in-explorer ()  
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
  (interactive)  
  (w32-shell-execute 
    "open" "explorer"  
    (concat "/e,/select," (convert-standard-filename buffer-file-name))
  )
)
; cmd prompt in the directory of the file in the current buffer
(defun open-cmd-at-folder ()  
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
  (interactive)  
  (w32-shell-execute 
    "open" "cmd"  
    (concat "/K cd \"" (file-name-directory (buffer-file-name)) "\"")
  )
)
; Browser to view the file in the current buffer
(defun view-buffer-in-firefox ()  
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
  (interactive)  
  (w32-shell-execute 
    "open" "firefox"  
    (convert-standard-filename buffer-file-name)
  )
)
; Save macro
(defun save-macro (name)                  
    "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro    
     (kmacro-name-last-macro name)         ; use this name for the macro    
     (find-file (user-init-file))                   ; open ~/.emacs or other user init file 
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro 
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

(fset 'clone-previous-line
   [up ?\C-a ?\C-k ?\C-y down ?\C-a ?\C-y ?\C-a])
(fset 'copy-line
   "\C-a\C-k\C-y")
(fset 'copy-sexp
   [?\C-  C-M-right escape ?w C-M-left])

;; ** Buffer Navigation
(fset 'other-window-reverse
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("-1o" 0 "%d")) arg)))
(global-set-key [(ctrl shift tab)] 'other-window-reverse)
(defun switch-to-buffer-other-window-horizontal()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'switch-to-buffer)
)
(defun switch-to-buffer-other-window-vertical()
  "Select a buffer in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'switch-to-buffer)
)
(defun find-file-other-window-vertical()
  "Edit a file in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'find-file)
)
(defun find-file-other-window-horizontal()
  "Edit a file in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'find-file)
)
(defun delete-window-switch-previous()
  "Delete a window and give focus to the previous window"
  (interactive)
  (call-interactively 'delete-window)
  (call-interactively 'other-window-reverse)
)
(defun find-file-other-window-other-dir()
  "Edit a file in other (next) window, using the same other window's file directory"
  (interactive)
  (call-interactively 'other-window)
  (call-interactively 'find-file)
)
(defun find-file-other-window-reverse-other-dir()
  "Edit a file in other (previous) window, using the same other window's file directory"
  (interactive)
  (call-interactively 'other-window-reverse)
  (call-interactively 'find-file)
)
(defun expand-window()
  "Update current workgroup config, then delete other windows"
  (interactive)
  (call-interactively 'wg-update-workgroup)
  (delete-other-windows)
)

(defun set-face-font-large()
  (interactive)
  (set-face-font 'default "Consolas-18.0:antialias=natural")
  (toggle-full-screen)
)

(defun set-face-font-small()
  (interactive)
  (set-face-font 'default "Consolas-14.0:antialias=natural")
  (toggle-full-screen)
)

(defun my-org-next-next ()
  (interactive)
  (forward-word)
  (when (re-search-forward "\\*+\\s-+NEXT" nil t)
    (org-reveal t))
  (org-back-to-heading))


;; * Auxiliary key bindings

;; ** Misc
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
; Translate the problematic keys to the function key Hyper, 
; then bind this to the desired ctrl-i behavior
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'grep)
(global-set-key (kbd "C-M-j")  'copy-sexp)
(global-set-key (kbd "C-z")  'copy-line)
(global-set-key (kbd "C-S-z")  'clone-previous-line)
(global-set-key (kbd "C-x C-a")  'copy-whole-buffer)
(global-set-key (kbd "M-s M-a")  'copy-src-block)
(global-set-key (kbd "M-s M-a")  'copy-src-block)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; ** Resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
;(global-set-key [C-kp-add] 'text-scale-increase)
;(global-set-key [C-kp-subtract] 'text-scale-decrease)
(global-set-key [C-kp-add] 'set-face-font-large)
(global-set-key [C-kp-subtract] 'set-face-font-small)

;; ** Navigation
(global-set-key [(shift tab)] 'indent-relative)
(global-set-key [(ctrl tab)] 'other-window)
(global-set-key (kbd "C-x C-\\ C-<right>") 'wg-switch-right)
(global-set-key (kbd "C-x C-\\ C-<left>") 'wg-switch-left)
(global-set-key (kbd "C-x C-\\ C-/") 'wg-switch-to-index-0)
(global-set-key (kbd "<f1>") 'wg-echo-time)
;(global-set-key (kbd "C-`") 'wg-revert-workgroup)
;(global-set-key (kbd "C-~") 'wg-update-workgroup)
(global-set-key (kbd "C-0") 'delete-window-switch-previous)
;(global-set-key (kbd "C-`") 'switch-to-buffer)
(global-set-key (kbd "C-`") 'helm-for-files)
(global-set-key (kbd "C-~") 'helm-find-files)
;(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-1") 'expand-window)
(global-set-key (kbd "C-!") 'wg-revert-workgroup)
;(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-2") 'switch-to-buffer-other-window-vertical)
;(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-3") 'switch-to-buffer-other-window-horizontal)
(global-set-key (kbd "C-@") 'find-file-other-window-vertical)
(global-set-key (kbd "C-#") 'find-file-other-window-horizontal)
(global-set-key (kbd "C-4") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-$") 'find-file-other-window)
(global-set-key (kbd "C-5") 'find-file-other-window-other-dir)
(global-set-key (kbd "C-%") 'find-file-other-window-reverse-other-dir)


















	   



	   






























;; * To be sorted

;; ** I don't know
;;(setq initial-frame-alist '((screen-gamma . 0.3))) 
;;(setq default-frame-alist '((screen-gamma . 0.3)))
;(put 'downcase-region 'disabled nil)
;(put 'upcase-region 'disabled nil)
;(put 'set-goal-column 'disabled nil)
;(require 'org-latex)
;(setq org-export-latex-packages-alist
;        '(("latin1" "inputenc")
;          ("T1" "fontenc")
;          ("" "lmodern")
;          ("" "fixltx2e")
;          ("" "graphicx")
;          ("" "longtable")
;          ("" "float")
;          ("" "wrapfig")
;          ("" "soul")
;          ("" "textcomp")
;          ("" "marvosym")
;          ("" "wasysym")
;          ("" "latexsym")
;          ("" "amssymb")
;          ("" "hyperref" )
;          ("" "color" )
;          ("" "listings" )))
;(require 'ox-beamer)
;(add-to-list 'org-latex-classes
;             '("beamer"
;               "\\documentclass\[presentation\]\{beamer\}"
;               ("\\section\{%s\}" . "\\section*\{%s\}")
;               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
;               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; ** Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e85dd0d1b43cc1d725db627298c2753b0c3e90dc0b195e80f09f97a4e1e5660c" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" default)))
 '(dired-listing-switches "-alh")
 '(doc-view-continuous t)
 '(doc-view-dvipdf-program (executable-find "dvipdfm"))
 '(doc-view-ghostscript-program (executable-find "gswin32c"))
 '(doc-view-resolution 160)
 '(grep-command "grep.exe -nH -ri . -e ")
 '(grep-find-command (quote ("C:/cygwin/bin/find.exe . -type f -exec C:/cygwin/bin/grep.exe -nH -ri . -e  {} \";\"" . 40)))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -n <R> {} NUL \";\"")
 '(grep-highlight-matches nil)
 '(grep-template "grep <X> <C> -n <R> <F>")
 '(grep-use-null-device t)
 '(ls-lisp-emulation nil)
 '(org-agenda-files nil)
 '(org-export-backends (quote (ascii beamer html icalendar latex md freemind)))
 '(org-latex-default-packages-alist (quote (("utf8" "inputenc" t) ("T1" "fontenc" t) ("" "lmodern" nil) ("english" "babel" nil) ("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "rotating" nil) ("normalem" "ulem" t) ("" "amsmath" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "amssymb" t) ("backend=bibtex,style=authoryear,natbib=true,bibencoding=utf8" "biblatex" nil) "\\tolerance=1000")))
 '(org-latex-pdf-process (quote ("latexmk -f -pdf %f")))
 '(org-latex-tables-centered nil)
 '(org-latex-with-hyperref nil)
 '(org-remove-highlights-with-change t)
 '(org-reveal-title-slide-template "<h1>%t</h1>
<h2>%a</h2>
<h2>%d</h2>")
 '(org-speed-commands-user (quote (("S" . widen) ("+" . org-columns-quit) ("N" org-speed-move-safe (quote my-org-next-next)))))
 '(org-src-fontify-natively t)
 '(preview-auto-cache-preamble nil)
 '(preview-gs-options (quote ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-scale-function 2.0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ** Org stuff

;; applications for opening links
(add-to-list 'org-file-apps
             '("\\.docx" . "winword.exe")
)
;; set custom Key Bindings
; used very often
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb) ; switch to org file
(global-set-key (kbd "<f11>") 'org-clock-goto) ; goto currently clocked item
(global-set-key (kbd "C-c c") 'org-capture) ; capture a task
; used often
(global-set-key (kbd "C-<f11>") 'org-clock-in) ; clock in a task (show menu with prefix)
(global-set-key (kbd "<f9> g") 'gnus) ; gnus - I check mail regularly
;(global-set-key (kbd "<f5>") 'bh/org-todo) ; show todo items for this subtree
(global-set-key (kbd "<S-f5>") 'bh/widen) ; widen
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
;(global-set-key (kbd "C-S-<f12>") 'bh/save-then-publish)
;(global-set-key (kbd "C-c l") 'org-store-link)
; used sometimes
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> r") 'boxquote-region) ; boxquote selected region
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp) ; insert inactive timestamp
(global-set-key (kbd "<f9> v") 'visible-mode) ; toggle visible mode (for showing/editing links)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> i") 'bh/punch-in)
(global-set-key (kbd "<f9> o") 'bh/punch-out)
(global-set-key (kbd "<f9> O") 'bh/make-org-scratch)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)
; used rarely
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "~/.emacs.d/tmp/scratch.org")
  (gnus-make-directory "~/.emacs.d/tmp"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; TODO keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; TODO selection (C-c C-t KEY)
(setq org-use-fast-todo-selection t)
; disable cycling through the todo states, since it skips setting timestamps and entering notes
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; TODO state triggers
; triggers that automatically assign tags to tasks to filter tasks in the agenda views conveniently
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/.emacs.d/org")

;; Setup org-capture
(global-set-key "\C-cc" 'org-capture)
(setq org-default-notes-file "~/.emacs.d/org/refile.org")
; Capture templates for
; phone calls (p), meetings (m), emails I need to respond to (r), new TODO tasks (t)
; new notes (n), interruptions (j) and new habits (h)
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/.emacs.d/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/.emacs.d/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/.emacs.d/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/.emacs.d/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/.emacs.d/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/.emacs.d/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/.emacs.d/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/.emacs.d/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
; Define a global key to capture a particular template
;(define-key global-map "\C-cx"
; (lambda () (interactive) (org-capture nil "x")))

; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; Refile setup
; <f12> -> SPC -> scroll down to second section of the block agenda: Tasks to Refile, or <F12> -> r
; for bulk refiling, m to mark, B r to refile
; for refiling as subtasks of the current clocking task, C-2 C-c C-w
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
;;;;;(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; set agenda files directory list
(setq org-agenda-files (quote ("~/.emacs.d/org")))
;; Disable C-c [ and C-c ] in org-mode
;; to prevent messing up my list of directories in the org-agenda-files variable
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c["    'undefined)
             (org-defkey org-mode-map "\C-c]"    'undefined))
          'append)


;; set agenda custom views
; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
; Compact the block agenda view
(setq org-agenda-compact-blocks t)
; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Project Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(priority-down todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING/!"
                           ((org-agenda-overriding-header (if (marker-buffer org-agenda-restrict-begin) "Project Subtasks" "Standalone Tasks"))
                            (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-agenda-skip-function 'bh/skip-stuck-projects)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                ;; (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                ;; (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;; (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;; (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil))))))
; setup to allow / RET to filter tasks removing hold/farm tasks and subtasks
(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))
(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


;; CLOCK setup
; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))
; makes time editing use discrete minute intervals (no rounding) increments
(setq org-time-stamp-rounding-minutes (quote (1 1)))
; shows 1 minute clocking gaps
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))
; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; estimates setup (C-c C-x C-c, then c to collapse)
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
; (pull up the agenda for the appropriate time frame, then l R to add the log report)
(setq org-agenda-log-mode-items (quote (closed state)))

;; TAGS setup
; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))
; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))
; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(require 'bbdb)
(require 'bbdb-com)

(global-set-key (kbd "<f9> p") 'bh/phone-call)

;; PHONE handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

;; GTD
(setq org-agenda-span 'day)
; disable the default org-mode stuck projects agenda view with the following setting
(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;; ARCHIVE setup
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")
(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;; Reminder setup
; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)
; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)
; Activate appointments so we get notifications
(appt-activate t)
; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; SPEED commands
(setq org-use-speed-commands t)


(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))


(setq org-agenda-include-diary t)
(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)
(add-hook 'org-mode-hook 
          (lambda ()
             (local-set-key "\M-n" 'outline-next-visible-heading)
             (local-set-key "\M-p" 'outline-previous-visible-heading)
;            ;; table
             (local-set-key "\M-\C-w" 'org-table-copy-region)
             (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
             (local-set-key "\M-\C-l" 'org-table-sort-lines)
            ;; display images
             (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; fix tab
             (local-set-key "\C-y" 'yank)
            ;; yasnippet (allow yasnippet to do its thing in org files)
            ;(org-set-local 'yas/trigger-key [tab])
            ;(define-key yas/keymap [tab] 'yas/next-field-group)
))

;; ** Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "s-`") 'helm-semantic-or-imenu)
;(setq helm-locate-command "locate %s -e -A --regex %s")
