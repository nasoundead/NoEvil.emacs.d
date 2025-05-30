;; (require 'project)

;; (defun project-root-override (dir)
;;   "Determine if DIR is a non-Git project."
;;   (catch 'ret
;;     (let ((pr-flags '((".project.el")
;;                       ("go.mod" "Cargo.toml" "pom.xml" "package.json") ;; higher priority
;;                       ("Makefile" "README.org" "README.md"))))
;;       (dolist (current-level pr-flags)
;;         (dolist (f current-level)
;;           (when-let ((root (locate-dominating-file dir f)))
;;             (throw 'ret (cons 'local root))))))))
;; (add-hook 'project-find-functions #'project-root-override)
;; (setq project-find-functions '(project-root-override project-try-vc))

;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.
(defun project-root-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (add-hook 'project-find-functions #'project-root-override))

;; (define-key project-prefix-map (kbd "b") #'+project-blink-search)
;; (define-key project-prefix-map (kbd "m") #'+project-magit)
(define-key project-prefix-map (kbd "d") #'+project-dired)
(define-key project-prefix-map (kbd "f") #'project-find-file)
(define-key project-prefix-map (kbd "F") #'project-find-dir)
(define-key project-prefix-map (kbd "g") #'+project-rg)
;; (define-key project-prefix-map (kbd "t") #'multi-vterm-project)

(setq project-switch-commands nil)
;; (add-to-list 'project-switch-commands '(+project-blink-search "BlinkSearch") t)
;; (add-to-list 'project-switch-commands '(+project-magit "Magit") t)
(add-to-list 'project-switch-commands '(+project-dired "Dired") t)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find dir") t)
(add-to-list 'project-switch-commands '(+project-rg "Ripgrep") t)
;; (add-to-list 'project-switch-commands '(multi-vterm-project "Vterm") t)


;;;###autoload
(defun +project-rg ()
  (interactive)
  (let ((default-directory (project-root (project-current nil))))
    (consult-ripgrep default-directory)))

;;;###autoload
;; (defun +project-magit ()
;;   (interactive)
;;   (require 'magit)
;;   (magit-status (project-root (project-current nil))))

;;;###autoload
(defun +project-dired ()
  (interactive)
  (let ((default-directory (project-root (project-current nil))))
    (dired default-directory)))

;;;###autoload
;; (defun +project-blink-search ()
;;   (interactive)
;;   (let ((default-directory (project-root (project-current nil))))
;;     (require 'blink-search)
;;     (blink-search)))

;;;###autoload
(defun +project-recentf ()
  (interactive)
  (find-file (completing-read "Recentf in project"
		       (-filter (lambda (it)
				  (s-contains? (car (-take-last 1 (project-current))) it)) recentf-list)
		       )))

(defun +project-info ()
  (interactive)
  (message "%s" (project-current t)))

(defun +add-dot-project ()
  (interactive)
  (let* ((root-dir (read-directory-name "Root: "))
         (f (expand-file-name ".project.el" root-dir)))
    (message "Create %s..." f)
    (make-empty-file f)))


;; use fd to find file
(defun -project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'-project-files-in-directory
          (or dirs (list (project-root-override project)))))

(provide 'init-project)