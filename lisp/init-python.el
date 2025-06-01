;; Python: pyright
(use-package lsp-pyright
    :preface
    ;; Use yapf to format
    (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
        (call-process "yapf" nil nil nil "-i" buffer-file-name)))
    :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
    :init (when (executable-find "python")
            (setq lsp-pyright-python-executable-cmd "python")))

(defun python/run-current-file (&optional directory)
  "Execute the current python file."
  (interactive
   (list (or (and current-prefix-arg
                  (read-directory-name "Run in directory: " nil nil t))
             default-directory)))
  (when (buffer-file-name)
    (let* ((command (or (and (boundp 'executable-command) executable-command)
                        (concat "python " (buffer-file-name))))
           (default-directory directory)
           (compilation-ask-about-save nil))
      (executable-interpret (read-shell-command "Run: " command)))))

;;   (defun maple/run-python ()
;;     (interactive)
;;     (or (python-shell-get-process) (call-interactively 'run-python))
;;     (if (region-active-p)
;;         (python-shell-send-region (region-beginning) (region-end) t)
;;       (python-shell-send-buffer t)))

(with-eval-after-load 'python 
  (define-key python-mode-map [f5] 'python/run-current-file))

(provide 'init-python)