;; init-edit.el
;;
;; This file contains configuration for editing text.
;;
;; Author: Wang Haibo
;; init-edit.el
;;
;; This file contains configuration for editing text.
;;
(use-package hungry-delete)
(global-hungry-delete-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package wgrep
  :ensure t)

;; Paredit configuration
;;
;; This section contains configuration for Paredit, a major mode for editing
;; Lisp code.
;;
(use-package paredit
  :ensure t
  :hook (prog-mode . paredit-mode)
  :config
  (defun paredit/space-for-delimiter-p (endp delm)
    (or (member 'font-lock-keyword-face (text-properties-at (1- (point))))
        (not (derived-mode-p 'basic-mode
                            'c++-mode
                            'c-mode
                            'coffee-mode
                            'csharp-mode
                            'd-mode
                            'dart-mode
                            'go-mode
                            'java-mode
                            'js-mode
                            'lua-mode
                            'objc-mode
                            'pascal-mode
                            'python-mode
                            'r-mode
                            'ruby-mode
                            'rust-mode
                            'typescript-mode))))
  (add-to-list 'paredit-space-for-delimiter-predicates 'paredit/space-for-delimiter-p))

(provide 'init-edit)
