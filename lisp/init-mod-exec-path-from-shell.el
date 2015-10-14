
(cond ((eq window-system 'w32))
       (t
        (exec-path-from-shell-copy-env "PATH")
        (exec-path-from-shell-copy-env "PYTHONPATH")))
