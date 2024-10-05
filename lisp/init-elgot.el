(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
        ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols)))

  ;; Emacs LSP booster
  (when (and emacs/>=29p (executable-find "emacs-lsp-booster"))
    (unless (package-installed-p 'eglot-booster)
      (and (fboundp #'package-vc-install)
          (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
    (use-package eglot-booster
      :ensure nil
      :autoload eglot-booster-mode
      :init (eglot-booster-mode 1))))


(defcustom centaur-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'centaur
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
        (defun ,intern-pre (info)
          (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                    "org-src-babel.tmp"))
          (pcase centaur-lsp
            ('eglot
            (when (fboundp 'eglot-ensure)
              (eglot-ensure)))
            ('lsp-mode
            (when (fboundp 'lsp-deferred)
              ;; Avoid headerline conflicts
              (setq-local lsp-headerline-breadcrumb-enable nil)
              (lsp-deferred)))
            (_
            (user-error "LSP:: invalid `centaur-lsp' type"))))
        (put ',intern-pre 'function-documentation
            (format "Enable `%s' in the buffer of org source block (%s)."
                    centaur-lsp (upcase ,lang)))

        (if (fboundp ',edit-pre)
            (advice-add ',edit-pre :after ',intern-pre)
          (progn
            (defun ,edit-pre (info)
              (,intern-pre info))
            (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defconst org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
    "The supported programming languages for interactive Babel.")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-elgot)
