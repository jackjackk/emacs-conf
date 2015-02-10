(require 'org-pomodoro)

(global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)
(defun custom-org-agenda-mode-pomodoro-defaults ()
  (org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "O" 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro))
(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-pomodoro-defaults 'append)

(add-to-list 'org-speed-commands-user 
             '("I" org-pomodoro))
(add-to-list 'org-speed-commands-user 
             '("O" org-pomodoro))