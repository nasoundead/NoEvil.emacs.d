(use-package toml-mode
  :ensure t)
;; (use-package rustic
;;   :ensure t
;;   :init
;;   (setq rustic-lsp-client 'eglot))
;; (add-hook 'rust-mode-hook 'eglot-ensure)

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :hook
  (rust-mode-hook . eglot-ensure)
  )

(provide 'init-rust)
