;; * multiple-cursors

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
