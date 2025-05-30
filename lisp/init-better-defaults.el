;关闭工具栏，tool-bar-mode即为一个MinorMode
(tool-bar-mode -1)
;;关闭文件滑动控件
(scroll-bar-mode -1)
(setq use-file-dialog nil)
;;更改光标的样式（不能生效，解决方案见第二集）
(setq cursor-type 'bar)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore) ;; 关闭提示音
;;关闭启动画面
(setq inhibit-startup-message t)
(setq make-backup-files nil) ;; 不生成备份文件
(setq auto-save-default nil) ;; 不生成自动保存文件
(global-auto-revert-mode 1) ;; 自动加载文件
(pixel-scroll-precision-mode)

(delete-selection-mode 1)
;; (electric-pair-mode 1)
(global-hl-line-mode)

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :init
      (setq linum-highlight-in-all-buffersp t)
      (custom-set-faces
       `(linum-highlight-face
         ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))))

(when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

(use-package aggressive-indent
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))
    (add-hook hook #'aggressive-indent-mode)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure t
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package pcre2el
  :ensure t)
(use-package ialign
  :ensure t
  :init
  (setq ialign-pcre-mode t)
  (setq ialign-initial-group -1)
  (setq ialign-initial-repeat t)
  (setq ialign-initial-regexp "([ ,=])"))

(use-package crux
  :ensure t
  :bind (
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-<return>" . crux-smart-open-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c M-u" . crux-upcase-region)
         ("C-c M-l" . crux-downcase-region)
         ("C-c M-c" . crux-capitalize-region)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ))

(use-package move-dup
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode))

(provide 'init-better-defaults)
