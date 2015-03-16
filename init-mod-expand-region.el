
(require 'expand-region)

(require 'html-mode-expansions)
(require 'latex-mode-expansions)
(require 'octave-expansions)
(require 'python-mode-expansions)
(require 'text-mode-expansions)
(require 'the-org-mode-expansions)

;(defun er/add-text-mode-expansions ()
;  (make-variable-buffer-local 'er/try-expand-list)
;  (setq er/try-expand-list (append
;                            er/try-expand-list
;                            '(mark-paragraph
;                              mark-page)))
;  ;(setq er/try-expand-list '(mark-paragraph
;  ;                            mark-page))
;)
;(er/enable-mode-expansions 'python-mode 'er/add-text-mode-expansions)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
