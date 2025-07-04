(add-to-list 'load-path "~/.emacs.d/lisp/")

;; (require 'init-package)
(require 'init-straight)
(require 'init-better-defaults)
(require 'init-edit)
(require 'init-search)
(require 'init-navi)
(require 'init-font)
(require 'init-ui)
(require 'init-dired)
(require 'init-neotree)
(require 'init-fanyi)
(require 'init-project)
(require 'init-hydra)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-highlight)
(require 'init-fold)
(require 'init-snippet)
(require 'init-vsc)
(require 'init-org)
(require 'init-treesitter)
(require 'init-lsp)
;; (require 'init-eglot)
;; (require 'init-lsp-bridge)
(require 'init-rust)
(require 'init-python)


(setq custom-file
    (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)
