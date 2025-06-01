;; (use-package magit
;; ;;   :config
;;   ;; (setq magit-completing-read-function 'ivy-completing-read)
;;   )
;; (use-package magit-popup)
;;; Pop up last commit information of current line
;; (use-package git-messenger
;;   :commands git-messenger:copy-message
;;   :bind (("C-x v p" . git-messenger:popup-message)
;;          :map git-messenger-map
;;          ("m" . git-messenger:copy-message))
;;   :init
;;   ;; Use magit-show-commit for showing status/diff commands
;;   (setq git-messenger:use-magit-popup t)
;;   )

;; (use-package git-timemachine)

(provide 'init-vsc)