;; init-snippet.el --- Initialize snippet configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

;;; Commentary:
;;
;; Snippet configurations.
;;

;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Yasnippet Completion At Point Function
(use-package consult-yasnippet
  :ensure t)

;; Auto Yasnippet
(use-package auto-yasnippet
  :ensure t
  :config
  (global-set-key (kbd "C-c C-y w")   #'aya-create)
  (global-set-key (kbd "C-c C-y TAB") #'aya-expand)
  (global-set-key (kbd "C-c C-y SPC") #'aya-expand-from-history)
  (global-set-key (kbd "C-c C-y d")   #'aya-delete-from-history)
  (global-set-key (kbd "C-c C-y c")   #'aya-clear-history)
  (global-set-key (kbd "C-c C-y n")   #'aya-next-in-history)
  (global-set-key (kbd "C-c C-y p")   #'aya-previous-in-history)
  (global-set-key (kbd "C-c C-y s")   #'aya-persist-snippet)
  (global-set-key (kbd "C-c C-y o")   #'aya-open-line))


(provide 'init-snippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-snippet.el ends here
