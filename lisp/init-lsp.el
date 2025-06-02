(use-package lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :hook ((prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
			  (lsp-deferred))))
	 ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
	 (lsp-mode . (lambda ()
		       ;; Integrate `which-key'
		       (lsp-enable-which-key-integration)

		       ;; Format and organize imports
		       (add-hook 'before-save-hook #'lsp-format-buffer t t)
		       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
	  ("C-c C-d" . lsp-describe-thing-at-point)
	  ([remap xref-find-definitions] . lsp-find-definition)
	  ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-keymap-prefix "C-c l"
	      lsp-keep-workspace-alive nil
	      lsp-signature-auto-activate nil
	      lsp-modeline-code-actions-enable nil
	      lsp-modeline-diagnostics-enable nil
	      lsp-modeline-workspace-status-enable nil

	      lsp-semantic-tokens-enable t
	      lsp-inlay-hint-enable t
	      lsp-progress-spinner-type 'progress-bar-filled

		  lsp-headerline-breadcrumb-enable nil

	      lsp-enable-file-watchers nil
	      lsp-enable-folding nil
	      lsp-enable-symbol-highlighting nil
	      lsp-enable-text-document-color nil

	      lsp-enable-indentation t
	      lsp-enable-on-type-formatting nil

	      ;; For diagnostics
	      lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)
          )
  :config
  (use-package consult-lsp
    :bind (:map lsp-mode-map
	    ("C-M-." . consult-lsp-symbols))))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
	 :map lsp-ui-mode-map
	 ("s-<return>" . lsp-ui-sideline-apply-code-actions)
	 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-delay 0.1
	lsp-ui-doc-show-with-cursor (not (display-graphic-p))
	lsp-ui-imenu-auto-refresh 'after-save
	lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
			      ,(face-foreground 'font-lock-string-face)
			      ,(face-foreground 'font-lock-constant-face)
			      ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
	  (if (facep 'posframe-border)
	      (face-background 'posframe-border nil t)
	    (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
	  (-let* ((win-width (frame-width))
		  (lsp-ui-peek-list-width (/ (frame-width) 2))
		  (string (-some--> (-zip-fill "" src1 src2)
			    (--map (lsp-ui-peek--adjust win-width it) it)
			    (-map-indexed 'lsp-ui-peek--make-line it)
			    (-concat it (lsp-ui-peek--make-footer)))))
	    (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
	    (posframe-show lsp-ui-peek--buffer
			   :string (mapconcat 'identity string "")
			   :min-width (frame-width)
			   :internal-border-color (face-background 'posframe-border nil t)
			   :internal-border-width 1
			   :poshandler #'posframe-poshandler-frame-center))
	(funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
	  (progn
	    (when (bufferp lsp-ui-peek--buffer)
	      (posframe-hide lsp-ui-peek--buffer))
	    (setq lsp-ui-peek--last-xref nil))
	(funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
	(goto-char 1)
	(while (setq next (next-single-property-change (or next 1) 'markdown-hr))
	  (when (get-text-property next 'markdown-hr)
	    (goto-char next)
	    (setq bolp (bolp)
		  before (char-before))
	    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
	    (setq after (char-after (1+ (point))))
	    (insert
	     (concat
	      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
	      (propertize "\n" 'face '(:height 0.5))
	      (propertize " "
			  ;; :align-to is added with lsp-ui-doc--fix-hr-props
			  'display '(space :height (1))
			  'lsp-ui-doc--replace-hr t
			  'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
	      ;; :align-to is added here too
	      (propertize " " 'display '(space :height (1)))
	      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))


(defcustom naso-lsp 'lsp-mode
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'naso
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

;; Enable LSP in org babel
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
        (defun ,intern-pre (info)
          (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                    "org-src-babel.tmp"))
          (pcase naso-lsp
            ('eglot
            (when (fboundp 'eglot-ensure)
              (eglot-ensure)))
            ('lsp-mode
            (when (fboundp 'lsp-deferred)
              ;; Avoid headerline conflicts
              (setq-local lsp-headerline-breadcrumb-enable nil)
              (lsp-deferred)))
            (_
            (user-error "LSP:: invalid `naso-lsp' type"))))
        (put ',intern-pre 'function-documentation
            (format "Enable `%s' in the buffer of org source block (%s)."
                    naso-lsp (upcase ,lang)))

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


(provide 'init-lsp)