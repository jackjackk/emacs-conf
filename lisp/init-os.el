
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

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))

(add-hook 'dired-mode-hook
      '(lambda ()
         (define-key dired-mode-map (kbd "<C-return>") 'dired-open-file)))

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
