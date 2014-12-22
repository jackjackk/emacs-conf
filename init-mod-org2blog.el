;; ** org2blog

(require 'org2blog-autoloads)

;; in my ~/.netrc/ I have:
;; machine myblog login my_username password my_password
(require 'auth-source) ;; or nothing if already in the load-path

(let (credentials)
  ;; only required if your auth file is not already in the list of auth-sources
  (add-to-list 'auth-sources "~/.netrc")
  (setq credentials (auth-source-user-and-password "myblog"))
  (setq org2blog/wp-blog-alist
        `(("My-Blog"
           :url "http://jackjackk.com/blog/xmlrpc.php"
           :username ,(car credentials)
           :password ,(cadr credentials)
           :default-title "Default title"
           :default-categories ("org2blog" "emacs")
           :tags-as-categories nil))))

;;   implemented as HTML styling. Your pick!
(setq org2blog/wp-use-sourcecode-shortcode 't)
;; removed light="true"
(setq org2blog/wp-sourcecode-default-params nil)
;; target language needs to be in here
(setq org2blog/wp-sourcecode-langs
      '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
        "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
        "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
        "vb" "xml"
        "sh" "emacs-lisp" "lisp" "lua"))
