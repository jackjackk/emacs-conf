
(require 'org-ref)

(setq reftex-default-bibliography '("~/org/research-references.bib"))

(setq org-ref-bibliography-notes "~/org/research-references.org"
      org-ref-default-bibliography '("~/org/research-references.bib")
      org-ref-pdf-directory "~/archive/zotero")

(setq helm-bibtex-bibliography "~/org/research-references.bib")
(setq helm-bibtex-library-path "~/archive/zotero")

(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)
