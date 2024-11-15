;; Auto completion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))

;; Add extensions
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package vterm
;;     :ensure t)

;; (use-package tabnine
;;   :custom
;;   (tabnine-wait 1)
;;   (tabnine-minimum-prefix-length 0)
;;   ;; (tabnine-executable-args (list "--log-level" "Error" "--no-lsp" "false"))
;;   :hook
;;   (on-first-input . tabnine-start-process)
;;   (prog-mode . tabnine-mode)
;;   (text-mode . tabnine-mode)
;;   (kill-emacs . tabnine-kill-process)
;;   :config
;;   (define-key tabnine-completion-map [tab] nil)
;;   (define-key tabnine-completion-map (kbd "M-f") #'tabnine-accept-completion-by-word)
;;   (define-key tabnine-completion-map (kbd "M-<return>") #'tabnine-accept-completion-by-line)
;;   (define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
;;   (define-key tabnine-completion-map (kbd "M-[") #'tabnine-next-completion)
;;   (define-key tabnine-completion-map (kbd "M-]") #'tabnine-previous-completion)
;;   )

(provide 'init-corfu)
