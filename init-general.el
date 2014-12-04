;; * General behaviour

;; ** Change backup behavior to save in a directory
;;Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)
