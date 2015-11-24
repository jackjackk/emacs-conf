
(require 'elpy)

(elpy-enable)

(elpy-use-ipython)

(fset 'send-par-to-python
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 67108896 134217832 3 3 21 67108896 21 67108896] 0 "%d")) arg)))
(define-key elpy-mode-map (kbd "C-'") 'send-par-to-python)
