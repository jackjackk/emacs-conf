(require 'toc-org nil t)
(add-hook 'org-mode-hook 'toc-org-enable)

(add-to-list 'org-tag-alist '("TOC" . ?T))
