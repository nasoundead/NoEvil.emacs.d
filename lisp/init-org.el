(defconst sea-org-directory
 (expand-file-name "~/org/")
 "org dir")

(use-package org
 ;; :mode (("\\.org$" . org-mode))
 :ensure org-plus-contrib
 :bind (("C-c a" . org-agenda)
       ("C-c b" . org-switchb)
       ("C-c x" . org-capture))
 :hook ((org-babel-after-execute org-mode) . org-redisplay-inline-images)
 :config
 (defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
	  (template-file (concat sea-site-lisp-dir "/template/template.docx")))
   (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
   (message "Convert finish: %s" docx-file)))

 (use-package jupyter
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
							 (:session . "py"))
	  org-babel-default-header-args:jupyter-R '((:async . "yes"))))
 ;; active Org-babel languages
 ;; ------------------------------------------------------------------------
 ;; Babel
 (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively nil)

 (defconst load-language-alist
  '((emacs-lisp . t)
   (python     . t)
   (js         . t)
   (css        . t)
   (C          . t)
   (java       . t)
   (plantuml   . t)
   (jupyter    . t)
   )
  "Alist of org ob languages.")

 ;; ob-sh renamed to ob-shell since 26.1.
 (cl-pushnew '(shell . t) load-language-alist)

 (use-package ob-go
  :init (cl-pushnew '(go . t) load-language-alist))

 (use-package ob-rust
  :init (cl-pushnew '(rust . t) load-language-alist))


 (use-package plantuml-mode
  :init
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (cl-pushnew '(plantuml . t) load-language-alist)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
 (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir))
 (defun sea/plantuml-install()
  (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
   (unless (file-exists-p org-plantuml-jar-path)
	(url-copy-file url org-plantuml-jar-path))))
 (add-hook 'org-mode-hook #'(lambda () (eval-after-load 'ob-plantuml (sea/plantuml-install))))

 (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)

 ;; Rich text clipboard
 (use-package org-rich-yank
  :bind (:map org-mode-map
	 ("C-M-y" . org-rich-yank)))

 (use-package valign
  :custom (valign-fancy-bar t)
  :hook (org-mode . valign-mode))

 ;; Table of contents
 (use-package toc-org
  :hook (org-mode . toc-org-mode))

 ;; Auto-toggle Org LaTeX fragments
 (use-package org-fragtog
  :diminish
  :hook (org-mode . org-fragtog-mode))

 ;; Preview
 (use-package org-preview-html
  :diminish
  :bind (:map org-mode-map
	 ("C-c C-h" . org-preview-html-mode))
  :init (when (featurep 'xwidget-internal)
	 (setq org-preview-html-viewer 'xwidget)))

 ;; Presentation
 (use-package org-tree-slide
  :diminish
  :functions (org-display-inline-images
		org-remove-inline-images)
  :bind (:map org-mode-map
	 ("<f7>" . org-tree-slide-mode)
	 :map org-tree-slide-mode-map
	 ("<left>" . org-tree-slide-move-previous-tree)
	 ("<right>" . org-tree-slide-move-next-tree)
	 )
  :hook ((org-tree-slide-play . (lambda ()
				    (text-scale-increase 4)
				    (org-display-inline-images)
				    (read-only-mode 1)))
	(org-tree-slide-stop . (lambda ()
				    (text-scale-increase 0)
				    (org-remove-inline-images)
				    (read-only-mode -1))))
  :init (setq org-tree-slide-header nil
		org-tree-slide-slide-in-effect t
		org-tree-slide-heading-emphasis nil
		org-tree-slide-cursor-init t
		org-tree-slide-modeline-display 'outside
		org-tree-slide-skip-done nil
		org-tree-slide-skip-comments t
		org-tree-slide-skip-outline-level 3))

 (use-package org-bars
  :straight (:host github
		     :repo "tonyaldon/org-bars")
  :hook (org-mode . org-bars-mode)
  )

 (use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :init (setq org-fancy-priorities-list
		(if (and (display-graphic-p) (char-displayable-p ?🅐))
		   '("🅐" "🅑" "🅒" "🅓")
		 '("high" "medium" "low" "optional"))))
 ;; ui enhance
 (defun enhance-ui-for-orgmode ()
  "enhance ui for orgmode."
;;   (when sea-prettify-org-symbols-alist
;;    (if prettify-symbols-alist
;; 	  (push sea-prettify-org-symbols-alist prettify-symbols-alist)
;; 	(setq prettify-symbols-alist sea-prettify-org-symbols-alist)))
;;   (prettify-symbols-mode)
  (toggle-truncate-lines))
 (add-hook 'org-mode-hook #'enhance-ui-for-orgmode)

 ;; To speed up startup, don't put to init section
 (setq
  org-modules nil                 ; Faster loading
  org-directory sea-org-directory
  org-capture-templates
  `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
    "*  %^{Title} %?\n%U\n%a\n")
   ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
    "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
   ("n" "Note" entry (file ,(concat org-directory "/note.org"))
    "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
   ("j" "Journal" entry (file+olp+datetree
			    ,(concat org-directory "/journal.org"))
    "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
   ("b" "Book" entry (file+olp+datetree
			 ,(concat org-directory "/book.org"))
    "* Topic: %^{Description}  %^g %? Added: %U"))

  org-todo-keywords
  '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
   )
  org-todo-keyword-faces '(("HANGUP" . warning)
			     )
  org-priority-faces '((?A . error)
			 (?B . warning)
			 (?C . success))

  org-M-RET-may-split-line '((:headline . nil))
  ;; Agenda styling
  org-agenda-block-separator ?─
  org-agenda-time-grid
  '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  org-agenda-current-time-string
  "⭠ now ─────────────────────────────────────────────────"

  org-tags-column -80
  org-log-done 'time
  org-catch-invisible-edits 'smart
  org-startup-indented t
  org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
  org-pretty-entities nil
  org-src-fontify-natively t
  org-eldoc-breadcrumb-separator " → "
  org-confirm-babel-evaluate nil
  org-hide-emphasis-markers t
  org-special-ctrl-a/e t
  org-html-coding-system 'utf-8
  org-html-doctype "html5"
  org-html-head  "<link rel='stylesheet' type='text/css' href='https://gongzhitaao.org/orgcss/org.css'/> "
  ;;  如何在 Source Block 中像在语言 mode 中一样的缩进
  org-src-tab-acts-natively t
  org-src-preserve-indentation nil
  ;; code block 默认折叠展示
  org-hide-block-startup t

  org-startup-numerated t
  )

 ;; Add new template
 (add-to-list 'org-structure-template-alist '("n" . "note"))

 (let* ((variable-tuple
	(cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
	      ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
	      ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
	      ((x-list-fonts "FiraCode Nerd Font")   '(:font "FiraCode Nerd Font"))
	      ((x-list-fonts "Verdana")         '(:font "Verdana"))
	      ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
	      (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.05))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.08))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.10))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.11))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.12))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.22))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.2 :underline nil))))))

;;  (use-package deft)
 ;; (use-package emacsql)
 (use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
  ;; 自定义默认模板
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
	 :if-new
	 (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
		"#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
	 :immediate-finish t)))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n c" . org-roam-capture)
   ("C-c n d" . org-roam-dailies-map)
   ("C-c n u" . org-roam-ui-mode))
  :config
  (org-roam-setup)
  ;;--------------------------
  ;; Handling file properties for ‘LAST_MODIFIED’
  ;;--------------------------
  (defun pv/org-find-time-file-property (property &optional anywhere)
   "Return the position of the time file PROPERTY if it exists.

    When ANYWHERE is non-nil, search beyond the preamble."
   (save-excursion
	(goto-char (point-min))
	(let ((first-heading
	  (save-excursion
	    (re-search-forward org-outline-regexp-bol nil t))))
	  (when (re-search-forward (format "^#\\+%s:" property)
				   (if anywhere nil first-heading)
				   t)
	(point)))))

  (defun pv/org-has-time-file-property-p (property &optional anywhere)
   "Return the position of time file PROPERTY if it is defined.

      As a special case, return -1 if the time file PROPERTY exists but
      is not defined."
   (when-let ((pos (pv/org-find-time-file-property property anywhere)))
	(save-excursion
	  (goto-char pos)
	  (if (and (looking-at-p " ")
	       (progn (forward-char)
		   (org-at-timestamp-p 'lax)))
	  pos
	-1))))
  (defun pv/org-set-time-file-property (property &optional anywhere pos)
   "Set the time file PROPERTY in the preamble.

      When ANYWHERE is non-nil, search beyond the preamble.

      If the position of the file PROPERTY has already been computed,
      it can be passed in POS."
   (when-let ((pos (or pos
			  (pv/org-find-time-file-property property))))
	(save-excursion
	  (goto-char pos)
	  (if (looking-at-p " ")
	  (forward-char)
	(insert " "))
	  (delete-region (point) (line-end-position))
	  (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
	(insert now)))))

  (defun pv/org-set-last-modified ()
   "Update the LAST_MODIFIED file property in the preamble."
   (when (derived-mode-p 'org-mode)
	(pv/org-set-time-file-property "last_modified")))

  :hook
  (before-save . pv/org-set-last-modified) ; 保存文件时调用
  )

;;   (use-package org-roam-ui
;;    :custom
;;    (org-roam-ui-sync-theme t)
;;    (org-roam-ui-follow t)
;;    (org-roam-ui-update-on-save t))

 (use-package org-download
   :after org
   :defer nil
   :custom
   (org-download-method 'directory)
   (org-download-image-dir "images")
   (org-download-heading-lvl nil)
   (org-download-timestamp "%Y%m%d-%H%M%S_")
   (org-image-actual-width 300)
   (when (eq system-type 'windows-nt)
  (setq org-download-screenshot-method "convert clipboard: %s"))
   (org-download-annotate-function 'ignore)
   ;; :bind
   ;; ("C-M-y" . org-download-screenshot)
   :bind (:map org-mode-map
	 ([f2] . org-download-screenshot))
   :config
   (require 'org-download)))

(use-package org-appear
 :hook (org-mode . org-appear-mode)
 :config
 (setq org-appear-autoemphasis t
	org-appear-autosubmarkers t
	org-appear-autolinks nil))

(use-package svg-tag-mode
 :hook (org-mode . svg-tag-mode)
 :config
 (defun mk/svg-checkbox-empty()
   (let* ((svg (svg-create 14 14)))
     (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
     (svg-image svg :ascent 'center)
     ))

 (defun mk/svg-checkbox-filled()
   (let* ((svg (svg-create 14 14)))
    (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
    (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
		:stroke-color 'black :stroke-width 1 :fill 'black)
    (svg-image svg :ascent 'center)
    ))
 (defun mk/svg-checkbox-toggle()
   (interactive)
   (save-excursion
     (let* ((start-pos (line-beginning-position))
	   (end-pos (line-end-position))
	   (text (buffer-substring-no-properties start-pos end-pos))
	   (case-fold-search t)  ; Let X and x be the same in search
	   )
	(beginning-of-line)
	(cond ((string-match-p "\\[X\\]" text)
	       (progn
		 (re-search-forward "\\[X\\]" end-pos)
		 (replace-match "[ ]")))
	   ((string-match-p "\\[ \\]" text)
	       (progn
		 (search-forward "[ ]" end-pos)
		 (replace-match "[X]")))
	      ))))

 (defun svg-progress-percent (value)
   (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

 (defun svg-progress-count (value)
   (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
     (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

 (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
 (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
 (defconst day-re "[A-Za-z]\\{3\\}")
 (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

 (setq svg-tag-action-at-point 'edit)

 (setq svg-lib-icon-collections
	`(("bootstrap" .
	   "https://icons.getbootstrap.com/assets/icons/%s.svg")
	  ("simple" .
	   "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
	  ("material" .
	   "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
	  ("octicons" .
	   "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
	  ("boxicons" .
	   "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

 (setq svg-tag-tags
	`(
	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ( (lambda (tag)
				(svg-tag-make tag :face 'org-priority
					      :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; Checkbox
	  ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
			(lambda () (interactive) (mk/svg-checkbox-toggle))
			"Click to toggle."
			))
	  ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
				 (lambda () (interactive) (mk/svg-checkbox-toggle))
				 "Click to toggle."))

	  ;; Active date (with or without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (with or without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

	  ;; Keywords
	  ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-todo :margin 0 :radius 5))))
	  ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
						 :face 'org-todo :margin 0 :radius 5))))
	  ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-done :margin 0 :radius 5))))

	  ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))

	  ;; beautify pagebreak in orgmode
	  ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
							  :stroke 0 :scale 1 :padding 0))))

	  )))


(provide 'init-org)