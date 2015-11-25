
(cond ((eq window-system 'w32)
       (setq python-shell-interpreter "ipython"))
      (t
       (setq python-shell-interpreter "ipython2")
       ))
(setq python-shell-interpreter-args "console --matplotlib=qt")

;(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
;(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

;
;(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
;(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
;(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;(fset 'execute_ipython_src_code_around_pointer
;   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([28 C-S-iso-lefttab 134217790 37 112 97 115 116 101 return C-S-iso-lefttab] 0 "%d")) arg)))
;(global-set-key (kbd "C-|")  'execute_ipython_src_code_around_pointer)
