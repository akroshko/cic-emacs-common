;;; emacs-stdlib-super-hyper-keys.el --- Emacs keybindings
;;; incorporating a super or hyper key that add a lot of functionality
;;; over the basic keybindings.
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
;; Uses the super and hyper keys to add a lot of higher-level
;; functionality in a relatively intuative way to the basic Emacs
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs development
;; org keys
(global-set-key (kbd "s-<return>") 'apk:org-insert-two-level)
(global-set-key (kbd "s-*") (lambda ()
                              (interactive)
                              (org-table-recalculate '(16))))
(global-set-key (kbd "s-c w") 'wdired-change-to-wdired-mode)
;; generic emacs development
(global-set-key (kbd "s-e b") (lambda () (interactive) (eval-buffer) (message "Evaluated buffer.")))
(global-set-key (kbd "s-e d") (lambda () (interactive) (let ((d (eval-defun nil))) (funcall d))))
(global-set-key (kbd "s-e i") (lambda () (interactive) (ielm)))
(global-set-key (kbd "s-e j") (lambda () (interactive) (switch-to-buffer "*Jaws*")))
(global-set-key (kbd "s-e m") (lambda () (interactive) (switch-to-buffer "*Messages*")))
(global-set-key (kbd "s-e e") (apk:toggle-variable debug-on-error
                                               "Debug on error enabled."
                                               "Debug on error disabled."))
(global-set-key (kbd "s-e s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic search/replace stuff
(global-set-key (kbd "s-f d") 'find-name-dired)
(global-set-key (kbd "s-f f") 'find-grep)
(global-set-key (kbd "s-f g") 'rgrep)
;; TODO better key for this
(global-set-key (kbd "s-f w") 'apk:search-word-other-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; h==help
(global-set-key (kbd "s-h a") 'info-apropos)
(global-set-key (kbd "s-h f") 'find-function)
;; put c-U to just go to front of manual
(global-set-key (kbd "s-h e") 'info-lookup-symbol)
(global-set-key (kbd "s-h k") 'find-function-on-key)
(global-set-key (kbd "s-h l") 'find-library)
;; TODO better key for both normal manual and searching
;;      is C-h == normal emacs and s-h == elisp good?
(global-set-key (kbd "s-h o") (lambda () (interactive) (info "org")))
(global-set-key (kbd "s-h r") (lambda () (interactive) (info "elisp")))
;; TODO better name/key for this
(global-set-key (kbd "s-h u") 'apropos-value)
(global-set-key (kbd "s-h v") 'find-variable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; j==jump
(global-set-key (kbd "s-j 9") (lambda () (interactive) (switch-to-buffer "*Collection*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; O == Open, meaning I'm Opening Outside of emacs
(global-set-key (kbd "s-o c") 'apk:browse-url-at-point-conkeror)
(global-set-key (kbd "s-o f") 'apk:browse-url-at-point-firefox)
(global-set-key (kbd "s-o w") 'apk:browse-url-at-point-w3m)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remote
(global-set-key (kbd "s-r m") (lambda () (interactive)
                                (if (eq major-mode 'dired-mode)
                                    (call-interactively 'remote-dired-mode)
                                  (call-interactively 'remote-url-mode))))
(global-set-key (kbd "s-r r") 'remote-open)
(global-set-key (kbd "s-r s-r") 'remote-open)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view
(global-set-key (kbd "s-v l") 'hl-line-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x=fix
(global-set-key (kbd "s-x c") (apk:toggle-variable case-fold-search
                                               "Case fold search enabled."
                                               "Case fold search disabled."))
(global-set-key (kbd "s-x w") 'apk:fix-whitespace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyper keys
;; are there cooler things than this?
;; time date stamp???
;; add control U?
(global-set-key (kbd "H->") 'next-buffer)
(global-set-key (kbd "H-<") 'previous-buffer)
(global-set-key (kbd "H-}") 'apk:next-file-dired)
(global-set-key (kbd "H-{") (lambda () (interactive) (apk:next-file-dired -1)))
(global-set-key (kbd "H-)") 'apk:org-end-of-next-heading)
(global-set-key (kbd "H-(") (lambda () (interactive) (apk:org-end-of-next-heading -1)))
(global-set-key (kbd "H-]") 'other-frame)
(global-set-key (kbd "H-[") '(lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "H-;") 'menu-bar-open)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window management
(global-set-key (kbd "H-p") 'windmove-up)
(global-set-key (kbd "H-n") 'windmove-down)
(global-set-key (kbd "H-f") 'windmove-right)
(global-set-key (kbd "H-b") 'windmove-left)
(global-set-key (kbd "H-d") 'apk:org-heading-timestamp)
;; TODO is this good, kinda makes sense, to put it on quit/refresh key
(global-set-key (kbd "H-<return>") 'buffer-menu)
(global-set-key (kbd "H-<backspace>") 'delete-frame)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other cool keys
(global-set-key (kbd "H-0") (lambda () (interactive) (text-scale-adjust 0)))
(global-set-key (kbd "H--") 'text-scale-decrease)
(global-set-key (kbd "H-=") 'text-scale-increase)
(global-set-key (kbd "H-+") 'text-scale-increase)
(global-set-key (kbd "H-\\") 'indent-sexp)
;; navigation of files
(global-set-key (kbd "H-i") 'imenu)
(global-set-key (kbd "H-y") 'copy-file-name-to-clipboard)
;; not sure I think this is good at all
(global-set-key (kbd "H-t") 'org-todo)
(global-set-key (kbd "C-H-t") (lambda () (interactive) (org-todo 'nextset)))
(global-set-key (kbd "M-H-t") (lambda () (org-todo '(4))))

(requiring-package (dired-x)
  (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode))

(provide 'emacs-stdlib-super-hyper-keys)
