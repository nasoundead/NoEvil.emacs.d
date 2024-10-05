(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package)
(require 'init-better-defaults)
(require 'init-edit)
(require 'init-search)
(require 'init-navi)
(require 'init-font)
(require 'init-ui)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-highlight)
(require 'init-fold)
(require 'init-treesitter)
(require 'init-elgot)
(require 'init-rust)


(setq custom-file
    (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)
