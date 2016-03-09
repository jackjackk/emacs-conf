(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

(defun is-magit-buffer (buffer)
  (let ((name (buffer-name buffer)))
    (and (= ?* (aref name 0))
         (not (string-match "^\\*magit\\*" name)))))
 
(defun kill-magit-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (and (is-magit-buffer buffer) (kill-buffer buffer))))

(defun magit-quit-session ()
  "Kill magit buffers and reduce current window"
  (interactive)
  (kill-magit-buffers)
  (delete-window-switch-previous))
      
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(global-set-key [f9] 'magit-status)
