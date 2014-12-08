;; ** GAMS

(require 'gams)

;;needed for correct coloring in multiline regions of code
(setq jit-lock-chunk-size 50000)

(setq gams-indent-on t)
(setq gams-indent-number 4)
(setq gams-template-file "~/.emacs.d/lisp/gams/gams-template.txt")
(setq gams:process-command-option "ll=0 lo=3 pw=32767 ps=0")
(setq gams-statement-upcase nil) ; Use upper case for GAMS statements
(setq gams-dollar-control-upcase nil) ; Use upper case for dollar operations.
(setq gams-close-double-quotation-always t)
(setq gams-close-single-quotation-always t)
(setq gams-eolcom-symbol-default' "#")
;(setq font-lock-support-mode '((gams-mode . nil) (t . jit-lock-mode)))

;; OS specific configuration
(cond ((eq window-system 'w32)
       (setq gams:process-command-name "C:/GAMS/win32/24.2/gams.exe")
       (setq gams-system-directory "C:/GAMS/win32/24.2/")
       (setq gams-docs-directory "C:/GAMS/win32/24.2/docs/")
       (setq gams-docs-view-program "C:/Program Files (x86)/Adobe/Acrobat 11.0/Acrobat/AcroRd32.exe")
       (setq gams-lxi-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi.exe")
       (setq gams-lxi-import-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi-import.exe")
       (setq gams-ol-external-program "~/.emacs.d/lisp/gams/external/gamsolc.exe")
       (t
       (setq gams:process-command-name "gams")
       (setq gams-system-directory "/opt/gams/24.2")
       (setq gams-docs-directory "/opt/gams/24.2/docs/")
       (setq gams-docs-view-program "evince")
       (setq gams-lxi-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi")
       (setq gams-lxi-import-command-name "~/.emacs.d/lisp/gams/lxi/gamslxi-import")
       (setq gams-ol-external-program "~/.emacs.d/lisp/gams/external/gamsolc")
)))
