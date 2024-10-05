(require 'package)
(setq package-archives '
    ;; (("gnu"    .  "https://elpa.gnu.org/packages/")
    ;;  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ;;  ("melpa"  . "https://melpa.org/packages/"))
    (("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
      ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
      ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
)
(package-initialize) ;; You might already have this line

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Update packages
(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :init
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
    (defalias 'package-upgrade-all #'auto-package-update-now)))

(provide 'init-package)
