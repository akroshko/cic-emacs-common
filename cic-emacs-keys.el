;;; cic-emacs-keys.el --- Emacs keybindings that add some
;;; functionality over the basic keybindings.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20190417
;; URL: https://github.com/akroshko/cic-emacs-common
;;
;; This file is NOT part of GNU Emacs.
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
;; functions from the cic-emacs-common library.
;; TODO: Want to list requirements eventually, but see
;; cic-emacs-config.el for some potential requires.
;;
;;; Code:

(define-minor-mode cic-emacs-keys-mode
  :global t
  ;; :lighter "Some standard keys."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<up>")       'cic:page-up)
            (define-key map (kbd "S-<down>")     'cic:page-down)
            ;; remap and add some standard functionality
            (define-key map [f11]                'cic:toggle-fullscreen)
            ;; TODO: trialing this
            (define-key map (kbd "M-'")          'cic:kill-transient-windows)
            (define-key map (kbd "C-j")          'indent-new-comment-line)
            (define-key map (kbd "M-s")          'cic:backward-symbol)
            (define-key map (kbd "M-g w")        'toggle-truncate-lines)
            (define-key map (kbd "M-g M-w")      'toggle-truncate-lines)
            (define-key map (kbd "M-i")          'indent-for-tab-command)
            (define-key map (kbd "M-/")          'dabbrev-expand)
            (define-key map (kbd "M-=")          'cic:count-words-region-or-buffer)
            (define-key map (kbd "C-\\")         'cic:whack-whitespace)
            (define-key map (kbd "C-<")          'cic:other-window-previous)
            (define-key map (kbd "C->")          'cic:other-window-next)
            (define-key map (kbd "C-c f")        'find-file-at-point)
            (define-key map (kbd "C-c o")        'occur)
            (define-key map (kbd "C-c w")        'compare-windows)
            (define-key map (kbd "C-;")          'menu-bar-open)
            (define-key map (kbd "<F1> a")       'apropos)
            (define-key map (kbd "C-x M-c")      'save-buffers-kill-emacs)
            (define-key map (kbd "C-x C-b")      'buffer-menu)
            (define-key map (kbd "C-x r i")      'string-insert-rectangle)
            (define-key map (kbd "C-x r \\")     'delete-whitespace-rectangle)
            (define-key map (kbd "s-v")          'cic:toggle-hl-line-mode-and-visuals)
            (define-key map (kbd "M-g h")        'cic:toggle-hl-line-mode-and-visuals)
            ;; text size keys
            (define-key map (kbd "C--")          'text-scale-decrease)
            (define-key map (kbd "C-=")          'cic:text-scale-neutral)
            (define-key map (kbd "C-+")          'text-scale-increase)
            ;; great for scanning through files
            (define-key map (kbd "s-]")          'cic:next-file-dired-pagedown)
            (define-key map (kbd "s-[")          'cic:previous-file-dired-pageup)
            (define-key map (kbd "s-}")          'cic:next-buffer-same-mode)
            (define-key map (kbd "s-{")          'cic:previous-buffer-same-mode)
            ;; org keys
            (define-key map (kbd "s-%")          'cic:query-replace-case-sensitive)
            (define-key map (kbd "s-*")          'cic:recalculate)
            ;; TODO: fix this
            (define-key map (kbd "s-=")          (lambda () (interactive) (what-cursor-position t)))
            (define-key map (kbd "s-a a")        'apropos)
            (define-key map (kbd "s-a s-a")      'apropos)
            ;; TODO: choose one of these
            (define-key map (kbd "s-a c")        'apropos-command)
            (define-key map (kbd "s-a x")        'apropos-command)
            (define-key map (kbd "s-a v")        'apropos-value)
            (define-key map (kbd "s-c e")        'wdired-change-to-wdired-mode)
            (define-key map (kbd "s-<return>")   'ispell-word)
            (define-key map (kbd "s-,")          'cic:wordlist-current-word-no-flyspell)
            ;; generic emacs development
            (define-key map (kbd "s-e b")        'cic:elisp-eval-buffer)
            (define-key map (kbd "s-e i")        'ielm)
            (define-key map (kbd "s-e m")        'cic:elisp-messages-buffer)
            (define-key map (kbd "s-e e")        'cic:elisp-debug-on-error)
            (define-key map (kbd "s-e s")        'cic:elisp-scratch-buffer)
            ;; h == help
            (define-key map (kbd "s-h a")        'info-apropos)
            (define-key map (kbd "s-h c")        'list-colors-display)
            (define-key map (kbd "s-h e")        'org-entities-help)
            (define-key map (kbd "s-h f")        'describe-function)
            (define-key map (kbd "s-h F")        'find-function)
            (define-key map (kbd "s-h k")        'describe-key)
            (define-key map (kbd "s-h K")        'find-function-on-key)
            (define-key map (kbd "s-h l")        'find-library)
            (define-key map (kbd "s-h r")        'cic:info-jump-select)
            (define-key map (kbd "s-h v")        'describe-variable)
            (define-key map (kbd "s-h V")        'find-variable)
            (define-key map (kbd "s-j p")        'cic:elisp-pp-capture-buffer)
            ;; s-m == system manager
            (define-key map (kbd "s-m p")        'cic:create-password-insert)
            ;; TODO: shift key instead?
            (define-key map (kbd "s-m M-p")      'cic:create-password-insert-select)
            ;; o == Open, meaning I'm opening outside of Emacs
            (define-key map (kbd "s-o c")        'cic:browse-url-at-point-conkeror)
            (define-key map (kbd "s-o f")        'cic:browse-url-at-point-firefox)
            (define-key map (kbd "s-o g")        'cic:browse-url-at-point-chromium)
            (define-key map (kbd "s-o w")        'cic:browse-url-at-point-w3m)
            ;; TODO: move this and rename
            (define-key map (kbd "s-p p s")      (lambda () (interactive) (profiler-start 'cpu)))
            (define-key map (kbd "s-p p r")      'profiler-report)
            (define-key map (kbd "s-5")          'toggle-case-fold-search)
            (define-key map (kbd "s-0")          'cic:copy-file-name-to-kill-ring)
            ;; TODO: I dont' use some of these much
            (define-key map (kbd "H->")          'next-buffer)
            (define-key map (kbd "H-<")          'previous-buffer)
            (define-key map (kbd "H-)")          'cic:org-end-of-next-heading)
            (define-key map (kbd "H-(")          'cic:org-end-of-prev-heading)
            (define-key map (kbd "H-}")          'cic:next-file-dired)
            (define-key map (kbd "H-{")          'cic:previous-file-dired)
            (define-key map (kbd "H-$")          'cic:flyspell-word)
            (define-key map (kbd "H-<return>")   'cic:flyspell-word)
            (define-key map (kbd "H-S-<return>") 'flyspell-goto-next-error)
            (define-key map (kbd "H-,")          'cic:wordlist-current-word)
            (define-key map (kbd "H-\\")         'indent-sexp)
            (define-key map (kbd "H-b")          'ido-switch-buffer)
            (define-key map (kbd "H-SPC")        'set-mark-command)
            ;; standard keys
            (define-key map (kbd "H-x")          'cic:kill-region-only-active)
            ;; TODO: don't really like this, it doesn't "flow" because too long of a reach
            (define-key map (kbd "H-g")          'cic:kill-transient-windows)
            (define-key map (kbd "H-z")          'scroll-down-command)
            (define-key map (kbd "H-l")          'downcase-word)
            (define-key map (kbd "H-u")          'upcase-word)
            ;; universal align
            (define-key map (kbd "H-q")          'align-current)
            map))
(global-unset-key (kbd "<C-down-mouse-1>"))
(define-key isearch-mode-map (kbd "M-/") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-?") 'isearch-repeat-backward)

(define-minor-mode cic-emacs-keys-non-term-mode
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") 'next-error)
            (define-key map (kbd "M-p") 'previous-error)
            map))

;; TODO: what other modes need this
(define-minor-mode cic-emacs-keys-non-image-mode
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; these make working on a laptop or tablet type computer great
            ;; but only for text files
            (define-key map (kbd "<up>")     'cic:move-up)
            (define-key map (kbd "<down>")   'cic:move-down)
            ;; TODO: make a symbol? just use scroll up/down
            (define-key map (kbd "C-<up>")   'scroll-down)
            (define-key map (kbd "C-<down>") 'scroll-up)
            map))

(define-minor-mode cic-emacs-keys-non-dired-mode
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-o")     'cic:occur-in-buffer)
            (define-key map (kbd "H-d") 'cic:insert-date-time-stamp)
            map))

(defun wdired-change-to-dired-mode--disable-keys (orig-fun &rest args)
  (let ((ret (apply orig-fun args)))
    (cic-emacs-keys-non-dired-mode 0)
    ret))
(advice-add 'wdired-change-to-dired-mode :around #'wdired-change-to-dired-mode--disable-keys)

(requiring-package (org)
  (define-minor-mode cic-emacs-keys-org-mode
    :global t
    ;; :lighter "Some standard keys for org-mode."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c a")      'org-agenda)
              ;; want key to be the same everywhere
              (define-key map (kbd "C-c C-o")    'org-open-at-point-global)
              (define-key map (kbd "C-<return>") 'cic:org-mode-control-return)
              map)
    ;; make org-mode calendar navigation more convienient without needing arrow keys
    ;; capitals not need
    (define-key org-read-date-minibuffer-local-map (kbd "F")   'cic:org-calendar-forward)
    (define-key org-read-date-minibuffer-local-map (kbd "B")   'cic:org-calendar-backward)
    (define-key org-read-date-minibuffer-local-map (kbd "P")   'cic:org-calendar-backward-week)
    (define-key org-read-date-minibuffer-local-map (kbd "N")   'cic:org-calendar-forward-week)
    (define-key org-read-date-minibuffer-local-map (kbd "M-F") 'cic:org-calendar-forward-month)
    (define-key org-read-date-minibuffer-local-map (kbd "M-B") 'cic:org-calendar-backward-month))
  (cic-emacs-keys-org-mode t)

  (define-minor-mode cic-emacs-keys-non-org-mode
    :global t
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-<return>") 'cic:comment-new-line-no-split)
              (define-key map (kbd "M-<return>") 'indent-new-comment-line)
              map))
  (cic-emacs-keys-non-org-mode t)
  (defun cic:comment-new-line-no-split ()
    (interactive)
    (end-of-line)
    (insert "\n")
    (comment-dwim nil))
  (defun cic:disable-emacs-keys-non-org-mode ()
    (cic-emacs-keys-non-org-mode 0))
  (add-hook 'org-mode-hook 'cic:disable-emacs-keys-non-org-mode)
  ;; TODO: make this function more universal?
  (add-hook 'special-mode-hook 'cic:disable-emacs-keys-non-org-mode))

(defun cic:emacs-keys-image-mode-setup-hook ()
  ;; TODO: which other ones?
  (when (derived-mode-p 'image-mode)
    (cic-emacs-keys-non-image-mode 0)))
(add-hook 'find-file-hook 'cic:emacs-keys-image-mode-setup-hook)

(defun cic:emacs-keys-term-setup-hook ()
  (cic-emacs-keys-non-term-mode 0))
(add-hook 'term-mode-hook 'cic:emacs-keys-term-setup-hook)

(defun cic:emacs-keys-dired-setup-hook ()
  (cic-emacs-keys-non-dired-mode 0))
(add-hook 'dired-mode-hook 'cic:emacs-keys-dired-setup-hook)

(defun cic:emacs-keys-minibuffer-setup-hook ()
  (cic-emacs-keys-mode 0)
  (cic-emacs-keys-org-mode 0)
  (cic-emacs-keys-non-term-mode 0))

(add-hook 'minibuffer-setup-hook 'cic:emacs-keys-minibuffer-setup-hook)

(defun cic:backward-symbol (&optional arg)
  (interactive "P")
  (if arg
      (forward-symbol (- arg))
    (forward-symbol (- 1))))

;; misc keys
;; TODO why are these here again?
(define-key minibuffer-local-completion-map
  " " 'self-insert-command)
(define-key minibuffer-local-must-match-map
  " " 'self-insert-command)

;; org-agenda
;; TODO do these conflict with any of my modes?
(global-set-key (kbd "C-c l") 'org-store-link)
;; do I really want control as intert?
(global-set-key (kbd "C-c c-l") 'org-insert-link-global)

;; TODO may not need this anymore, has it been superceded
(requiring-package (flyspell)
  (defun cic:flyspell-init-text ()
    "Inititalize flyspell for text modes."
    (flyspell-mode t)
    (flyspell-buffer))

  (defun cic:flyspell-init-prog ()
    "Inititalize flyspell from programming modes."
    (flyspell-prog-mode)
    (flyspell-buffer))
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(defun cic:org-calendar-forward ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-day 1)))
(defun cic:org-calendar-backward ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-day 1)))
(defun cic:org-calendar-backward-week ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-week 1)))
(defun cic:org-calendar-forward-week ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-week 1)))
(defun cic:org-calendar-forward-month ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-month 1)))
(defun cic:org-calendar-backward-month ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-month 1)))

(define-minor-mode emacs-keys-non-dired-org-mode ()
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-c i") 'cic:outline)
            map))

(add-hook 'after-change-major-mode-hook 'cic:enable-emacs-keys-non-dired-org-mode)
;; XXXX: for auctex latex mode, apparently does not run above hook
(add-hook 'find-file-hook 'cic:enable-emacs-keys-non-dired-org-mode)

(defun cic:enable-emacs-keys-non-dired-org-mode ()
  ;; TODO: make a list for these eventually
  (cond ((derived-mode-p 'dired-mode 'image-mode 'line-items-mode 'org-mode 'wdired-mode)
         (emacs-keys-non-dired-org-mode 0))
        (t
         (emacs-keys-non-dired-org-mode t))))

;; TODO: update so I can enter a date (and/or time) into
;;       comments anywhere
;;       used to have key for this

;; TODO: do I use this?
(define-minor-mode cic-emacs-keys-all-mode
  ;;"Some standard keys that should work in all modes including
  ;; minibuffer."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; TODO: not sure I want to keep these
            (define-key map (kbd "H-]") 'cic:next-window-frame)
            (define-key map (kbd "H-[") 'cic:prev-window-frame)
            ;; window management
            ;; TODO: these can be better
            (define-key map (kbd "H-p") 'windmove-up)
            (define-key map (kbd "H-n") 'windmove-down)
            (define-key map (kbd "H-<backspace>") 'delete-frame)
            map))

(provide 'cic-emacs-keys)
