;; ** Capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cr"
  (lambda () (interactive) (org-capture nil "r")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))

(setq org-default-notes-file "~/Dropbox/org/capture.org")

(setq org-capture-templates
      '(("r" "Todo" entry (file+headline "~/Dropbox/org/capture.org" "Capture")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         (file "~/.org/templates/review"))))
