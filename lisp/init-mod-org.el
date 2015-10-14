(setq org-export-allow-bind-keywords t)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-to-list 'org-modules 'org-habit)

(add-to-list 'org-global-properties '("STYLE_ALL" . "habit"))

(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(add-hook 'org-mode-hook
      '(lambda ()
         (add-to-list 'org-export-snippet-translation-alist
               '("l" . "latex"))))

(global-set-key (kbd "C-S-s") 'org-babel-tangle)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (ditaa . t)
   (sh . t)
   (org . t)
   ))

(setq org-confirm-babel-evaluate nil)

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

(require 'org)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-use-speed-commands t)

(setq org-pretty-entities t)

(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(defun custom-org-mode-defaults ()
"Executed as org-mode-hook."
(electric-indent-mode -1)
(org-defkey org-mode-map (kbd "M-p") 'org-metaup)
(org-defkey org-mode-map (kbd "M-n") 'org-metadown)
(org-defkey org-mode-map (kbd "C-p") 'org-babel-previous-src-block)
(org-defkey org-mode-map (kbd "C-n") 'org-babel-next-src-block)
(org-defkey org-mode-map (kbd "<prior>") 'org-previous-link)
(org-defkey org-mode-map (kbd "<next>") 'org-next-link)
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
(setq org-clock-persist-query-resume nil)
(org-clock-persistence-insinuate)

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
(global-set-key (kbd "C-S-<f11>") '(lambda () (interactive) (org-clock-in '(4)) ))

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

(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00"))))

(setq org-agenda-log-mode-items (quote (closed state)))

(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-agenda-files '("~/org"))

(add-to-list 'load-path "~/.emacs.d/lisp/org-helpers")
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
                       '(oh/agenda-skip :subtree-if '(inactive habit))) ; project habit scheduled deadline)))
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

(plist-put org-format-latex-options :scale 2)

(setq org-babel-python-command "ipython2 --pylab=qt5 --pdb --nosep --classic 
--no-banner --no-confirm-exit")

(global-set-key (kbd "<C-escape>") (kbd "C-c '"))

(define-key global-map "\C-cc" 'org-capture)

(setq org-directory "~/org")

(setq org-default-notes-file "~/org/capture.org")

(setq org-capture-templates
      (quote (("t" "todo" entry (file "capture.org")
               "* TODO %?" :clock-in t :clock-resume t)
              ("p" "I was procrastinating" entry (file+headline "~/org/personal-procrastination.org" "I was procrastinating instead of")
               "* %? %i\n- [ ] Think how the task is relevant and meaningful to your interests and goals\n- [ ] Set reasonable standards to be expected from you in completing the task\n- [ ] Give the right weight to others' evaluation of your work\n- [ ] Be aware of the personal resources you can leverage on to do the job\n- [ ] Decide how much you really want to invest in the task\n- [ ] Feel ready to accept the responsibilities involved?\n- [ ] Spend enough time in defining the outcome and developing the steps needed to accomplish that outcome" :clock-in t :clock-resume t)
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
(cond ((eq window-system 'w32)
       (setq org-babel-sh-command "C:/cygwin/bin/sh.exe"))
       (t
        (setq org-babel-sh-command "sh")))
;; this will use emacs syntax higlighting in your #+BEGIN_SRC
;; <language> <your-code> #+END_SRC code blocks.
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
;; add <p for python expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))
;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
             '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))
;; add <pv for python expansion with value
(add-to-list 'org-structure-template-alist
             '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))
;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>"))
;; add <sh for shell
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" "<src lang=\"shell\">\n?\n</src>"))
;(setq org-babel-python-command "~/anaconda/bin/ipython --no-banner --classic --no-confirm-exit")

;; ** Clean view
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(defun prettier-org-code-blocks-upper ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\(\+BEGIN_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?Â¦)
                                       nil))) 
                            ("\\(\+END_SRC\\)"
                             (0 (progn (compose-region (match-beginning 1) (match-end 1) ?Â¦)
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
; Allow refile to create parent tasks with confirmation
;(setq org-refile-allow-creating-parent-nodes (quote confirm))
; Use IDO for both buffer and file completion and ido-everywhere to t
;(setq org-completion-use-ido t)
;(setq ido-everywhere t)
;(setq ido-max-directory-size 100000)
;(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
;(setq ido-default-file-method 'selected-window)
;(setq ido-default-buffer-method 'selected-window)
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
(global-set-key (kbd "<C-apps>") (kbd "C-c C-v p"))
(global-set-key (kbd "<C-M-apps>") (kbd "C-c C-v n"))
(global-set-key (kbd "<f1>") 'outline-previous-visible-heading)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
