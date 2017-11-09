;;; emacs-stdlib-super-hyper-keys.el --- Emacs keybindings
;;; incorporating a super or hyper key that add a lot of functionality
;;; over the basic keybindings.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20160525
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
;; functionality in a relatively intuitive way to the basic Emacs
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

(define-minor-mode emacs-stdlib-super-keys-mode
  ;; "Some standard keys bound to super."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-[")        'cic:previous-buffer-same-mode)
            (define-key map (kbd "s-]")        'cic:next-buffer-same-mode)
            ;; great for scanning
            (define-key map (kbd "s-}")        'cic:next-file-dired-pagedown)
            (define-key map (kbd "s-{")        'cic:previous-file-dired-pageup)
            ;; org keys
            ;; TODO: using for something else now
            ;; (define-key map (kbd "s-<return>") 'cic:org-insert-two-level)
            (define-key map (kbd "s-%")        'cic:query-replace-case-sensitive)
            (define-key map (kbd "s-*")        'cic:recalculate)
            (define-key map (kbd "s-=")        (lambda () (interactive) (what-cursor-position t)))
            ;; TODO: e for edit for now, e=emacs a lot for me
            (define-key map (kbd "s-c e")      'wdired-change-to-wdired-mode)
            ;; generic emacs development
            (define-key map (kbd "s-e b")      'cic:elisp-eval-buffer)
            (define-key map (kbd "s-e d")      'cic:elisp-eval-call-defun)
            (define-key map (kbd "s-e i")      'ielm)
            ;; TODO: these aren't in line with my current scheme
            (define-key map (kbd "s-e j")      'cic:elisp-pp-capture-buffer)
            (define-key map (kbd "s-e m")      'cic:elisp-messages-buffer)
            (define-key map (kbd "s-e e")      'cic:elisp-debug-on-error)
            (define-key map (kbd "s-e s")      'cic:elisp-scratch-buffer)
            (define-key map (kbd "s-f w")      'cic:search-word-other-window)
            ;; h==help
            ;; TODO: these badly need a menu
            (define-key map (kbd "s-h a")      'info-apropos)
            (define-key map (kbd "s-h f")      'find-function)
            ;; put c-u to just go to front of manual
            (define-key map (kbd "s-h e")      'info-lookup-symbol)
            (define-key map (kbd "s-h k")      'find-function-on-key)
            (define-key map (kbd "s-h l")      'find-library)
            (define-key map (kbd "s-h o")      'cic:help-org)
            (define-key map (kbd "s-h r")      'cic:help-elisp)
            ;; TODO better name/key for this u neq v
            (define-key map (kbd "s-h u")      'apropos-value)
            (define-key map (kbd "s-h v")      'find-variable)
            ;; m==system manager
            (define-key map (kbd "s-m p")      'cic:create-password-insert)
            (define-key map (kbd "s-m M-p")    'cic:create-password-insert-select)
            (define-key map (kbd "s-v")        'cic:yank-primary)
            ;; j==jump
            ;; O == Open, meaning I'm Opening Outside of emacs
            (define-key map (kbd "s-o c")      'cic:browse-url-at-point-conkeror)
            (define-key map (kbd "s-o f")      'cic:browse-url-at-point-firefox)
            (define-key map (kbd "s-o g")      'cic:browse-url-at-point-gnome-web)
            (define-key map (kbd "s-o w")      'cic:browse-url-at-point-w3m)
            ;; TODO: move this
            (define-key map (kbd "H-;")        'cic:ansi-term-localhost-popup)
            ;; decide alternate key for this....
            ;; (define-key map (kbd "M-0")        'cic:ansi-term-localhost)
            (define-key map (kbd "s-c 1")      'cic:ansi-term-ipython)
            (define-key map (kbd "s-c 2")      'cic:ansi-term-sage)
            ;; view
            ;; requires??? changing becaus s-v used for other things
            ;; (define-key map (kbd "s-v l") 'hl-line-mode)
            ;; x=fix
            ;; zap up to char
            ;; TODO: something else here due to other keys I'm trying
            ;; TODO: decide if I want to toggle global default value or ???
            (define-key map (kbd "s-5")        'toggle-case-fold-search)
            ;; (cic:toggle-variable case-fold-search
            ;;  "Case fold search enabled."
            ;;  "Case fold search disabled."))
            ;; (define-key map (kbd "s-x w") 'cic:fix-whitespace)
            ;; zap up to char
            ;; TODO: disabled for now due to conflict with other keys I'm trying
            ;; (requiring-package (misc)
            ;;   (define-key map (kbd "s-z") 'zap-up-to-char))
            map))

(define-minor-mode emacs-stdlib-hyper-keys-mode
  ;; "Some standard keys bound to hyper."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; Hyper keys
            ;; are there cooler things than this?
            ;; time date stamp???
            ;; add control U?
            (define-key map (kbd "H->")          'next-buffer)
            (define-key map (kbd "H-<")          'previous-buffer)
            (define-key map (kbd "H-)")          'cic:org-end-of-next-heading)
            (define-key map (kbd "H-(")          'cic:org-end-of-prev-heading)
            (define-key map (kbd "H-}")          'cic:next-file-dired)
            (define-key map (kbd "H-{")          'cic:previous-file-dired)
            (define-key map (kbd "H-$")          'cic:flyspell-here)
            (define-key map (kbd "H-<return>")   'cic:flyspell-here)
            (define-key map (kbd "H-S-<return>") 'flyspell-goto-next-error)
            (define-key map (kbd "H-,")          'cic:wordlist-current-word)
            (define-key map (kbd "H-\\")         'indent-sexp)
            (define-key map (kbd "H-b")          'ido-switch-buffer)
            (define-key map (kbd "H-B")          'cic:switch-buffer-new-window-below)
            (define-key map (kbd "H-SPC")        'ido-switch-buffer)
            (define-key map (kbd "H-S-SPC")      'cic:switch-buffer-new-window-below)
            (define-key map (kbd "H-g")          'cic:kill-transient-windows)
            ;; TODO: disabled while I get new muscle memory
            ;; (define-key map (kbd "H-h")          'cic:cycle-with-last-buffer)
            (define-key map (kbd "H-m")          'cic:term-toggle-modes)
            ;; (define-key map (kbd "H-k")          'cic:delete-window)
            ;; TODO: temporary while I build new muscle memory
            ;; (define-key map (kbd "H-0")          'cic:delete-window)
            ;; TODO: keep these but maybe temporary while I build new muscle memory
            (define-key map (kbd "H-l")          'cic:delete-window-below)
            (define-key map (kbd "H-K")          'cic:delete-frame)
            ;; TODO: temporary while I build new muscle memory
            (define-key map (kbd "H-x")          'save-buffer)
            (define-key map (kbd "H-y")          'cic:copy-file-name-to-clipboard)
            ;; universal align
            (define-key map (kbd "H-q")          'align-current)
            map))
;; TODO: uncomment this
;; (define-key ido-buffer-completion-map (kbd "H-SPC") 'ido-next-match)


;; ensure that I can override some keys in dired
(define-minor-mode emacs-stdlib-hyper-keys-non-dired-mode ()
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "H-d") 'cic:insert-date-time-stamp)
            (define-key map (kbd "H-D") 'cic:insert-date-time-stamp)
            (define-key map (kbd "H-i") 'cic:outline)
            ;; (define-key map (kbd "H-d") 'cic:insert-current-time)
            ;; (define-key map (kbd "H-D") 'cic:insert-current-timestamp)
            map))

(add-hook 'after-change-major-mode-hook 'cic:enable-emacs-stdlib-hyper-keys-non-dired-mode)
;; XXXX: for auctex latex mode, apparently does not run above hook
(add-hook 'find-file-hook 'cic:enable-emacs-stdlib-hyper-keys-non-dired-mode)

(defun wdired-change-to-dired-mode--disable-hyper (orig-fun &rest args)
  (let ((ret (apply orig-fun args)))
    (emacs-stdlib-hyper-keys-non-dired-mode 0)
    ret))
(advice-add 'wdired-change-to-dired-mode :around #'wdired-change-to-dired-mode--disable-hyper)

(defun cic:enable-emacs-stdlib-hyper-keys-non-dired-mode ()
  ;; TODO: make a list for these eventually
  (cond ((or (eq major-mode 'dired-mode) (eq major-mode 'image-mode))
         (emacs-stdlib-hyper-keys-non-dired-mode 0))
        (t
         (emacs-stdlib-hyper-keys-non-dired-mode t))))

;; TODO: update so I can enter a date (and/or time) into
;;       comments anywhere
;;       used to have key for this

(define-minor-mode emacs-stdlib-hyper-keys-all-mode
  ;;"Some standard keys bound to hyper that should work in all
  ;; modes including minibuffer."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "H-]") 'cic:next-window-frame)
            (define-key map (kbd "H-[") 'cic:prev-window-frame)
            ;; window management
            ;; TODO: replace
            ;; (define-key map (kbd "H-a") 'other-window)
            (define-key map (kbd "H-p") 'windmove-up)
            (define-key map (kbd "H-n") 'windmove-down)
            ;; other cool keys
            (define-key map (kbd "H-0") 'cic:text-scale-neutral)
            (define-key map (kbd "H--") 'text-scale-decrease)
            (define-key map (kbd "H-=") 'text-scale-increase)
            (define-key map (kbd "H-+") 'text-scale-increase)
            ;; TODO: decided this was better used for other things
            ;; (define-key map (kbd "H-<return>") 'buffer-menu)
            (define-key map (kbd "H-<backspace>") 'delete-frame)
            map))

;; ensure only certain keys work in minibuffer
(defun cic:stdlib-super-hyper-keys-minibuffer-setup-hook ()
  (emacs-stdlib-super-keys-mode 0)
  (emacs-stdlib-hyper-keys-mode 0))
(add-hook 'minibuffer-setup-hook 'cic:stdlib-super-hyper-keys-minibuffer-setup-hook)

(provide 'emacs-stdlib-super-hyper-keys)
