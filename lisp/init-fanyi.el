;; Default, comment out the providers you don't need.
;; (use-package fanyi
;;   :ensure t
;;   :custom
;;   (fanyi-providers '(;; 海词
;;                     ;;  fanyi-haici-provider
;;                      ;; 有道同义词词典
;;                      fanyi-youdao-thesaurus-provider
;;                      ;; Etymonline
;;                     ;;  fanyi-etymon-provider
;;                      ;; Longman
;;                     ;;  fanyi-longman-provider
;;                      ))
;;     :config
;;     (setq fanyi-auto-select nil))

(use-package bing-dict
  :ensure t
  :config
  (global-set-key (kbd "C-c f") 'bing-dict-brief))

(provide 'init-fanyi)