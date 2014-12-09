;; * Org-mode

(require 'org-habit)
(add-to-list 'load-path "~/.emacs.d/org-helpers")
(require 'org-helpers)

;; ** General
;; Set org-mode as the default mode for .org, .org_archive, and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; ** Navigation
;; Use speed commands
(setq org-use-speed-commands t)
(defun ded/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))
(add-to-list 'org-speed-commands-user
             '("n" ded/org-show-next-heading-tidily))
(defun ded/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))
(add-to-list 'org-speed-commands-user 
             '("p" ded/org-show-previous-heading-tidily))


;; ** Agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/Dropbox/org"))
(defun custom-org-agenda-mode-defaults ()
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer))
(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)
(setq org-agenda-custom-commands
      '(("a" "Agenda"
       ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-agenda-files '("~/Dropbox/org/capture.org"))
                    (org-agenda-skip-function
                     '(oh/agenda-skip :headline-if-restricted-and '(todo)))))
          (tags-todo "-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project habit scheduled deadline)))))
          (tags-todo "-WAITING-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-CANCELLED/!-NEXT-HOLD-WAITING"
                     ((org-agenda-overriding-header "Available Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Currently Active Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/Dropbox/org/capture.org"))))
        ("#" "Stuck Projects" tags-todo "-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-CANCELLED/!-NEXT-HOLD-WAITING"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Currently Active Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
              (org-agenda-sorting-strategy '(category-keep))
              (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
          (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))

;; ** Capture
(setq org-capture-templates
      '(("r" "Todo" entry (file+headline "~/Dropbox/org/capture.org" "Capture")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         (file "~/.org/templates/review"))))
(define-key global-map "\C-cr"
  (lambda () (interactive) (org-capture nil "r")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))

;; ** Tasks
;; Add a time stamp to the task when moved to DONE
(setq org-log-done 'time)
;; Sequence of action steps (C-c C-t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(o)" "|" "CANCELLED(c@/!)")))
(setq org-todo-keyword-faces
      '(("TODO"  :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE"  :foreground "forest green" :weight bold)))

;; ** Latex
(setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

;; ** Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (emacs-lisp . t) (ditaa . t) (sh . t)))
(setq org-confirm-babel-evaluate nil)
(cond ((eq window-system 'w32)
       (setq org-babel-sh-command "C:/cygwin/bin/sh.exe"))
       (t
        (setq org-babel-sh-command "sh")))

;; ** Clean view
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(defun prettier-org-code-blocks-upper ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\(\+BEGIN_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?�)
                                       nil))) 
                            ("\\(\+END_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?�)
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

;; ** Org Key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<M-menu>") (kbd "C-c '"))
(global-set-key (kbd "<C-menu>") (kbd "C-c C-v p"))
(global-set-key (kbd "<C-M-menu>") (kbd "C-c C-v n"))
(global-set-key (kbd "<M-apps>") (kbd "C-c '"))
(global-set-key (kbd "<C-apps>") (kbd "C-c C-v p"))
(global-set-key (kbd "<C-M-apps>") (kbd "C-c C-v n"))
(global-set-key (kbd "<f2>") 'outline-previous-visible-heading)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
