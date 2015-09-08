
(require 'helm)
(require 'helm-config)

(setq helm-moccur-always-search-in-current t)

(cond ( (eq window-system 'w32)
        (setq helm-locate-command "es %s %s")
        (setq helm-for-files-preferred-list
              '(helm-source-buffers-list
                helm-source-recentf
                ;; helm-source-bookmarks
                ;; helm-source-file-cache
                helm-source-files-in-current-dir
                ;; helm-source-locate
                helm-source-moccur)))
      (t
        (setq helm-locate-command "locate %s -e -A --regex %s")
        (setq helm-for-files-preferred-list
              '(helm-source-buffers-list
                helm-source-recentf
                ;; helm-source-bookmarks
                ;; helm-source-file-cache
                helm-source-files-in-current-dir
                helm-source-locate
                helm-source-moccur))))

; Open helm buffer inside current window, not occupy whole other window.
(setq helm-split-window-in-side-p           t)
; Move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source     t)
; Search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp        t)
; Scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount                    8)
; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
(setq helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(global-set-key (kbd "C-`") 'helm-for-files)
(global-set-key (kbd "C-~") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-M-`") 'helm-semantic-or-imenu)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(defun switch-to-buffer-other-window-vertical()
  "Select a buffer in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-2") 'switch-to-buffer-other-window-vertical)

(defun find-file-other-window-vertical()
  "Edit a file in a window obtained by vertically splitting the current one"
  (interactive)
  (split-window-vertically)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-@") 'find-file-other-window-vertical)

(defun switch-to-buffer-other-window-horizontal()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-3") 'switch-to-buffer-other-window-horizontal)

(defun find-file-other-window-horizontal()
  "Edit a file in a window obtained by horizontally splitting the current one"
  (interactive)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-#") 'find-file-other-window-horizontal)

(defun my-switch-to-buffer-other-window()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-4") 'my-switch-to-buffer-other-window)

(defun my-find-file-other-window()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-$") 'my-find-file-other-window)

(defun my-switch-to-buffer-other-window-reverse()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window-reverse)
  (call-interactively 'helm-for-files)
)
(global-set-key (kbd "C-5") 'my-switch-to-buffer-other-window-reverse)

(defun my-find-file-other-window-reverse()
  "Select a buffer in a window obtained by horizontally splitting the current one"
  (interactive)
  (call-interactively 'other-window-reverse)
  (call-interactively 'helm-find-files)
)
(global-set-key (kbd "C-%") 'my-find-file-other-window-reverse)

(helm-mode 1)
(semantic-mode 1)
