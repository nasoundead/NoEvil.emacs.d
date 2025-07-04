;;; init-highlight.el -*- lexical-binding: t; -*-
;; -*- lexical-binding: t -*-
;; Copyright (C) 2024 Bruce Wong

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;; Highlight symbols
(use-package symbol-overlay
  :ensure t
  :custom-face
  (symbol-overlay-face-1 ((t (:inherit 'highlight))))
  (symbol-overlay-face-2 ((t (:inherit 'font-lock-builtin-face :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit 'warning :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit 'font-lock-constant-face :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit 'error :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit 'dired-mark :inverse-video t :bold nil))))
  (symbol-overlay-face-7 ((t (:inherit 'success :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit 'dired-symlink :inverse-video t :bold nil))))
  :bind (([C-f3] . symbol-overlay-put)
         ([f3] . symbol-overlay-jump-next)
         ([S-f3] . symbol-overlay-jump-prev)
         ([M-f3] . symbol-overlay-remove-all))
  :hook (
         ;; (prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)
         )
  :init (setq symbol-overlay-idle-time 0.01))

;; Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :hook ((emacs-lisp-mode web-mode css-mode) . rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :ensure t
  :custom-face (hl-todo ((t (:box t :bold t))))
  :bind (:map hl-todo-mode-map
              ([C-S-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . "#cd5c5c") hl-todo-keyword-faces))
  (cl-pushnew '("WORKAROUND" . "#d0bf8f") hl-todo-keyword-faces))

;; Highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :defines desktop-minor-mode-table
  :commands diff-hl-magit-post-refresh
  :custom-face
  (diff-hl-change ((t (:background "#46D9FF"))))
  (diff-hl-delete ((t (:background "#ff6c6b"))))
  (diff-hl-insert ((t (:background "#98be65"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Set fringe style
  (setq diff-hl-draw-borders nil)
  (setq fringes-outside-margins t)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure t
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab)))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))


(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'column)
 ;;  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  :hook ((org-indent-mode-hook)
      . (lambda ()
    (when highlight-indent-guides-mode
      (highlight-indent-guides-mode -1)))))

(use-package page-break-lines
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . page-break-lines-mode) )

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
