;; init-edit.el
;;
;; This file contains configuration for editing text.
;;
;; Author: Wang Haibo
;; init-edit.el
;;
;; This file contains configuration for editing text.
;;
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(provide 'init-edit)
