(use-package toml-mode
  :ensure t)

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;; (use-package rust-mode
;;   :init
;;   (setq rust-mode-treesitter-derive t)
;;   :hook
;;   (rust-mode-hook . eglot-ensure)
;; )

(provide 'init-rust)
