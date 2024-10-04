(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (use-package nyan-mode
    :ensure t
    :hook (doom-modeline-mode . nyan-mode)
    :config
    (setq nyan-animate-nyancat t
	  nyan-wavy-trail t)))

(use-package nerd-icons)

(provide 'init-ui)
