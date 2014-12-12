;; * OS integration

;; Inspired by https://github.com/kosh04/emacs-lisp/blob/master/gnome-util.el

; Windows explorer to go to the file in the current buffer
(defun open-folder-in-explorer ()  
  "Call when editing a file in a buffer. Open explorer in the current directory and select the current file"  
  (interactive)  
  (cond ((eq window-system 'w32)
         (w32-shell-execute 
          "open" "explorer"  
          (concat "/e,/select," (convert-standard-filename buffer-file-name))
          ))
        (t
         (call-process "nautilus" nil 0 nil
                       (expand-file-name
                        (or dir default-directory)))
         ;;(let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
         ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. with nautilus
        ))
)

; cmd prompt in the directory of the file in the current buffer
(defun open-cmd-at-folder ()  
  "Call when editing a file in a buffer. Open a shell in the current directory and select the current file"  
  (interactive)  
  (cond ((eq window-system 'w32)
         (w32-shell-execute 
          "open" "cmd"  
          (concat "/K cd \"" (file-name-directory (buffer-file-name)) "\"")))
        (t
         (call-process "gnome-terminal" nil 0 nil default-directory)))
)

; Browser to view the file in the current buffer
(defun view-buffer-in-firefox ()  
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
  (interactive)  
  (cond ((eq window-system 'w32)
         (w32-shell-execute 
          "open" "firefox"  
          (convert-standard-filename buffer-file-name)))
         (t
          (call-process "firefox" nil 0 nil (expand-file-name
                        (or dir default-directory)))))
)


(defvar open-program-name
  (cond ((eq window-system 'w32)
         "open")
        (t
         (if (executable-find "xdg-open")
             "xdg-open"
           "gnome-open"))
        ))

(defun shell-execute (filename &optional directory params)
"Open filename in shell"
(setq filename (expand-file-name filename))
(let ((default-directory (or directory default-directory)))
  (apply #'call-process open-program-name
         nil 0 nil filename params)))
