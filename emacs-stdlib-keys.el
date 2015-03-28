;;; emacs-stdlib-keys.el --- Emacs keybindings that add some
;;; functionality over the basic keybindings.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20150327
;; URL: https://github.com/akroshko/emacs-stdlib
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;;
;; These form a useful and relatively intuitive extension of the Emacs
;; keybindings.  There will likely be more keybindings moved here in
;; the future.
;;
;; Features that might be required by this library:
;;
;; Generally only requires a basic Emacs installation as well as
;; functions from the emacs-stdlib library.
;; TODO: Want to list requirements eventually, but see
;; emacs-config.el for some potential requires.
;;
;;; Code:

;; remap and add some standard functionality
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-set-key [f11] 'apk:toggle-fullscreen)
(global-set-key [f12] 'apk:toggle-menubar)
(global-set-key (kbd "M-b") (lambda (arg)
                              (interactive "p")
                              (forward-symbol (- arg))))
(global-set-key (kbd "M-f") 'forward-symbol)
(global-set-key (kbd "M-i") 'indent-for-tab-command)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-\\") 'apk:whack-whitespace)
;; XXXX good key, but I now use a hyper key
;; (global-set-key (kbd "C-x M-x") 'menu-bar-open)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c w") 'compare-windows)
(global-set-key (kbd "C-h a") 'apropos)
;; steve yegge suggestion
;; (global-set-key (kbd "C-w") 'backward-kill-word)
;; (global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x M-c") (lambda () (interactive)
                                  (when (fboundp 'gnus-group-exit)
                                      (gnus-group-exit))
                                  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)
(global-set-key (kbd "C-x r \\") 'delete-whitespace-rectangle)
;; zap up to char
(requiring-package (misc)
  (global-set-key (kbd "s-z") 'zap-up-to-char))
;; misc keys
;; TODO what do these do again?
(define-key minibuffer-local-completion-map
  " " 'self-insert-command)
(define-key minibuffer-local-must-match-map
  " " 'self-insert-command)

;; org-agenda
;; XXXX do these conflict with any of my modes?
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; TODO do I care about this anymore?
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c L") 'org-insert-link-global)

(requiring-package (flyspell)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(requiring-package (org)
  ;; want key to be the same everywhere
  (global-set-key (kbd "C-c C-o") 'org-open-at-point-global))

(provide 'emacs-stdlib-keys)
