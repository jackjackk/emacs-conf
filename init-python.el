
(setq python-shell-interpreter "ipython2")
(setq python-shell-interpreter-args "")

(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

(setenv "PYTHONPATH" "/home/jack/github/gdxpy")

(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
