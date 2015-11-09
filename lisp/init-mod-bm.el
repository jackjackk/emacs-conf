(require 'bm)

(global-set-key (kbd "<C-f1>") 'bm-toggle)
(global-set-key (kbd "<C-f2>")   'bm-next)
(global-set-key (kbd "<C-f3>") 'bm-previous)

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(setq bm-in-lifo-order t)

(setq temporary-bookmark-p t)
