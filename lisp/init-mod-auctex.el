
(defun LaTeX-math-overbar ()
  "Insert \\overbar{}."
  (interactive)
  (insert "\\overbar{}")
  (backward-char 1))
(defun LaTeX-math-underbar ()
  "Insert \\ubar{}."
  (interactive)
  (insert "\\ubar{}")
  (backward-char 1))
(defun LaTeX-math-Beta ()
  "Insert \\Beta."
  (interactive)
  (insert "\\Beta"))
(defun LaTeX-math-Eta ()
  "Insert \\Eta."
  (interactive)
  (insert "\\Eta"))

(setq LaTeX-math-list '((?_ LaTeX-math-underbar nil)
                        (?= LaTeX-math-overbar nil)
                        (?B LaTeX-math-Beta nil)
                        (?H LaTeX-math-Eta nil)))
