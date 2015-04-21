
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'cl)
(defun package-list-installed-p ()
  (loop for package in package-list
        when (not (package-installed-p package)) do (return nil)
        finally (return t)))

(unless (package-list-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done."))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package))
  (load-library (format "init-mod-%s.el" package)))

(dolist (package other-package-list)
  (add-to-list 'load-path (format "~/.emacs.d/%s" package))
  (load-library (format "init-mod-%s.el" package)))
