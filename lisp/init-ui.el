(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :straight (nerd-icons.el
	     :type git
	     :host github
	     :repo "rainstormstudio/nerd-icons.el"
	     :files (:defaults "data")
       )
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(provide 'init-ui)
