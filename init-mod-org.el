
(require 'org)
(require 'org-habit)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-use-speed-commands t)

(defun custom-org-mode-defaults ()
"Executed as org-mode-hook."
(electric-indent-mode -1)
(org-defkey org-mode-map (kbd "M-p") 'org-metaup)
(org-defkey org-mode-map (kbd "M-n") 'org-metadown)
(org-defkey org-mode-map (kbd "C-p") 'org-babel-previous-src-block)
(org-defkey org-mode-map (kbd "C-n") 'org-babel-next-src-block)
(org-shifttab 2))
(add-hook 'org-mode-hook 'custom-org-mode-defaults)

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

(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(o)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO"      :foreground "red"     :weight bold)
        ("NEXT"      :foreground "#e9c062" :weight bold) ; "blue"?
        ("DONE"      :foreground "forest green" :weight bold)
        ("WAITING"   :foreground "#fd9b3b" :weight bold)
        ("HOLD"      :foreground "#9b859d" :weight bold)
        ("SOMEDAY"   :foreground "#808080" :weight bold)
        ("CANCELLED" :foreground "#9eb9a7" :weight bold)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist-query-resume nil)

(setq org-clock-history-length 30)

(setq org-clock-in-resume t)

(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (oh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (oh/is-project-p))
      "TODO"))))

(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)

(setq org-clock-out-remove-zero-time-clocks t)

(setq org-clock-out-when-done t)

(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

(setq org-clock-report-include-clocking-task t)

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))
(defvar bh/organization-task-id "b0605007-6a44-4446-abab-528d429b1483")

(setq bh/keep-clock-running nil)
(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(global-set-key (kbd "<f11>") 'org-clock-in)
(global-set-key (kbd "S-<f11>") 'org-clock-out)
(global-set-key (kbd "M-<f11>") 'bh/punch-in)
(global-set-key (kbd "M-S-<f11>") 'bh/punch-out)
(global-set-key (kbd "C-<f11>") 'org-clock-goto)

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))
(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))
(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :indent t :narrow 80)))

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))))

(setq org-agenda-log-mode-items (quote (closed state)))

(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-files '("~/org"))

(add-to-list 'load-path "~/.emacs.d/org-helpers")
(require 'org-helpers)

(defun custom-org-agenda-mode-defaults ()
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer))
(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

(setq org-agenda-custom-commands
      '(("a" "Agenda"
       ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-agenda-files '("~/org/capture.org"))
                    (org-agenda-skip-function
                     '(oh/agenda-skip :headline-if-restricted-and '(todo)))))
          (tags-todo "/!-CANCELLED-HOLD-WAITING"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project habit scheduled deadline)))))
          (tags-todo "/NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "/!-CANCELLED-NEXT-HOLD-WAITING"
                     ((org-agenda-overriding-header "Available Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "/!-CANCELLED"
                     ((org-agenda-overriding-header "Currently Active Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/org/capture.org"))))
        ("#" "Stuck Projects" tags-todo "/!-CANCELLED-HOLD-WAITING"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "/NEXT"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "/!-CANCELLED-NEXT-HOLD-WAITING"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "/!-CANCELLED"
         ((org-agenda-overriding-header "Currently Active Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
              (org-agenda-sorting-strategy '(category-keep))
              (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "/!WAITING|HOLD"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
          (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))

(add-hook 'org-agenda-after-show-hook 'show-all)



(define-key global-map "\C-cc" 'org-capture)

(setq org-directory "~/org")

(setq org-default-notes-file "~/org/capture.org")

(setq org-capture-templates
      (quote (("t" "todo" entry (file "capture.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "capture.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "capture.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "capture.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "capture.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "capture.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "capture.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(define-key global-map (kbd "<M-f9>")
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map (kbd "<M-S-f9>")
  (lambda () (interactive) (org-capture nil "r")))
(define-key global-map (kbd "<C-f9>")
  (lambda () (interactive) (org-capture nil "j")))
(define-key global-map (kbd "<C-S-f9>")
  (lambda () (interactive) (org-capture nil "n")))

;; * Org-mode

(require 'org-habit)

;; ** Agenda


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
;; this will use emacs syntax higlighting in your #+BEGIN_SRC
;; <language> <your-code> #+END_SRC code blocks.
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python :session :results silent\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))
;(setq org-babel-python-command "~/anaconda/bin/ipython --no-banner --classic --no-confirm-exit")
(setq org-babel-python-command "~/anaconda/bin/python")

;; ** Clean view
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(defun prettier-org-code-blocks-upper ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\(\+BEGIN_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                                       nil))) 
                            ("\\(\+END_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
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

;; ** Links
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key "\C-c L" 'org-insert-link-global)
(setq org-return-follows-link t) ; <RET> will also follow the link at point

;; ** Refile
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; ** Org Key bindings
(global-set-key (kbd "<f2>") (kbd "C-c '"))
(global-set-key (kbd "<C-menu>") (kbd "C-c C-v p"))
(global-set-key (kbd "<C-M-menu>") (kbd "C-c C-v n"))
(global-set-key (kbd "<M-apps>") (kbd "C-c '"))
(global-set-key (kbd "<C-apps>") (kbd "C-c C-v p"))
(global-set-key (kbd "<C-M-apps>") (kbd "C-c C-v n"))
(global-set-key (kbd "<f1>") 'outline-previous-visible-heading)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
