;; * Packages

;; ** List the repositories
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; ** Activate all the packages (in particular autoloads)
(package-initialize)

;; ** Fetch the list of packages available 
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
  (message "%s" " done.")
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))
    (load-library (format "init-mod-%s.el" package))))
