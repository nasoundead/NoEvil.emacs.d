(use-package corfu
  :ensure t
  :demand t
  :custom
  (corfu-auto t)
  (corfu-max-width 110)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  (corfu-echo-documentation t)
  :bind (:map corfu-map
              ("C-d" . corfu-info-documentation)
              ("M-." . corfu-info-location))
  :hook
  (after-init . global-corfu-mode))

(provide 'init-corfu)
