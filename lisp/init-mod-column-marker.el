
(require 'column-marker)

(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 fill-column)))
