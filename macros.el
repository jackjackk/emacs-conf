
(fset 'cite2link
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217828 40 91 91 99 105 116 101 58 right 134217739 delete backspace 25 93 91 21 25 134217827 134217828 delete 32 134217830 93 93 41] 0 "%d")) arg)))


(fset 'tcite2link
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217828 91 91 116 101 120 116 99 105 116 101 58 right 134217739 delete backspace 25 93 91 21 25 134217827 134217828 delete 32 40 134217830 41 93 93] 0 "%d")) arg)))

