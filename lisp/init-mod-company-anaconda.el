(require 'company-anaconda)

(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))
