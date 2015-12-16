
(setq python-shell-interpreter "jupyter")
(setq python-shell-interpreter-args "console")

(fset 'my-python-refactor-commas
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 201326629 92 40 91 94 32 17 10 93 92 41 44 92 40 91 94 32 17 10 93 92 41 return 92 49 44 32 92 50 return 33 21 67108896 21 67108896] 0 "%d")) arg)))

(setq python-shell-prompt-detect-failure-warning nil)

(setq python-shell-prompt-input-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

;
;(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
;(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
;(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;(fset 'execute_ipython_src_code_around_pointer
;   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([28 C-S-iso-lefttab 134217790 37 112 97 115 116 101 return C-S-iso-lefttab] 0 "%d")) arg)))
;(global-set-key (kbd "C-|")  'execute_ipython_src_code_around_pointer)
