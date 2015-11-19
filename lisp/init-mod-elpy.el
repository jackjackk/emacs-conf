
(require 'elpy)

(elpy-enable)

(define-key elpy-mode-map (kbd "C-'") 'send-par-to-python)
