;; (use-package emacs
;;   :init
;;   (set-face-attribute 'default nil
;;     ;; :font "InconsolataGo Nerd Font"
;;     :font "FantasqueSansM Nerd Font"
;;     ;; :font "EnvyCodeR Nerd Font"
;;     ;; :font "FiraCode Nerd Font"s
;;     :height 130))


;; Fonts
(defun font-installed-p (font-name)
 "Check if font with FONT-NAME is available."
 (find-font (font-spec :name font-name)))

(defun centaur-setup-fonts ()
 "Setup fonts."
 (when (display-graphic-p)
 ;; Set default font
 (cl-loop for font in '("Jetbrains Mono" "Cascadia Code" "Fira Code"
          "SF Mono" "Hack" "Source Code Pro" "Menlo"
          "Monaco" "DejaVu Sans Mono" "Consolas")
 when (font-installed-p font)
 return (set-face-attribute 'default nil
                 :family font
                 :height (cond (sys/macp 130)
                             (sys/win32p 110)
                             (t 100))))


 ;; Specify font for all unicode characters
 (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol" "DejaVu Sans Mono" "IBM Plex Serif")
 when (font-installed-p font)
 return (if (< emacs-major-version 27)
     (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

 ;; Emoji
 (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
 when (font-installed-p font)
 return (cond
   ((< emacs-major-version 27)
    (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
   ((< emacs-major-version 28)
    (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
    (t
     (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

 ;; Specify font for Chinese characters
 (cl-loop for font in '("Microsoft Yahei" "LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
          "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
 when (font-installed-p font)
 return (progn
    (setq face-font-rescale-alist `((,font . 1.3)))
    (set-fontset-font t 'han (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)


(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
           all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
              all-the-icons-faicon all-the-icons-fileicon
              all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty))
    )

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)

(provide 'init-font)
