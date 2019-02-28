;;; emacs-stdlib-keys.el --- Emacs keybindings that add some
;;; functionality over the basic keybindings.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20190228
;; URL: https://github.com/akroshko/emacs-stdlib
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
;; functions from the emacs-stdlib library.
;; TODO: Want to list requirements eventually, but see
;; emacs-config.el for some potential requires.
;;
;;; Code:

(define-minor-mode emacs-stdlib-keys-mode
  :global t
  ;; :lighter "Some standard keys."
  :keymap (let ((map (make-sparse-keymap)))
            ;; TODO: not in use for now
            ;; (define-key map (kbd "S-SPC") (lambda () (interactive) (switch-to-buffer (other-buffer))))

            (define-key map (kbd "S-<up>")   'cic:page-up)
            (define-key map (kbd "S-<down>") 'cic:page-down)
            ;; remap and add some standard functionality
            (define-key map [f11]            'cic:toggle-fullscreen)
            ;; M- most convienient for laptop, control most convienient for touch typing
            ;; (define-key map (kbd "M-,")      'scroll-down-command)
            ;; (define-key map (kbd "M-.")      'scroll-up-command)
            (define-key map (kbd "C-,")      'scroll-down-command)
            (define-key map (kbd "C-.")      'scroll-up-command)
            (define-key map (kbd "C-j")      'indent-new-comment-line)
            ;; avoid accidental C-w from deleting many things
            (define-key map (kbd "C-w")      'cic:kill-region-only-active)
            (define-key map (kbd "M-b")      'cic:backward-symbol)
            ;; copy/yank
            (define-key map (kbd "M-c")      'kill-ring-save-whole-word-or-region)
            ;;;
            (define-key map (kbd "M-e")      'other-frame)
            (define-key map (kbd "M-g w")    'toggle-truncate-lines)
            (define-key map (kbd "M-g M-w")  'toggle-truncate-lines)
            (define-key map (kbd "M-g k")    'cic:window-kill-above)
            (define-key map (kbd "M-g K")    'cic:window-kill-below)
            (define-key map (kbd "M-g j")    'cic:split-window-above)
            (define-key map (kbd "M-g J")    'split-window-below)
            (define-key map (kbd "M-v")      'yank)
            (define-key map (kbd "M-w")      'kill-ring-save-whole-word-or-region)
            ;; this is good, right? not destructive if I miss shift
            (define-key map (kbd "M-W")      'delete-window)
            ;; move
            (define-key map (kbd "M-f")      'forward-symbol)
            (define-key map (kbd "M-i")      'indent-for-tab-command)
            ;; TODO: replace
            ;; TODO: made this a global key
            ;; (define-key map (kbd "M-l")      'cic:goto-previous-mark)
            ;; TODO: interferes with prior/next keys in terminal mode
            ;; (define-key map (kbd "M-[")      'cic:decrease-indent)
            ;; (define-key map (kbd "M-]")      'cic:increase-indent)
            (define-key map (kbd "M-/")      'dabbrev-expand)
            (define-key map (kbd "M-,")      'cic:x-force-maximized)
            (define-key map (kbd "M-=")      'cic:count-words-region-or-buffer)
            ;; (define-key map (kbd "M-?")      'isearch-backward)
            (define-key map (kbd "C-\\")     'cic:whack-whitespace)
            (define-key map (kbd "C-<")      'cic:other-window-previous)
            (define-key map (kbd "C->")      'cic:other-window-next)
            (define-key map (kbd "C-c f")    'find-file-at-point)
            (define-key map (kbd "C-c o")    'occur)
            (define-key map (kbd "C-c w")    'compare-windows)
            (define-key map (kbd "C-;")      'menu-bar-open)
            (define-key map (kbd "C-h a")    'apropos)
            ;; steve yegge suggestion
            ;; (global-set-key (kbd "C-w")     'backward-kill-word)
            ;; (global-set-key (kbd "C-x C-k") 'kill-region)
            ;; TODO: disable swap point and mark, make something better, why did nil not work for me here?
            ;; (define-key map (kbd "C-x C-x")   (lambda (interactive)))
            (define-key map (kbd "C-x C-SPC") (lambda (interactive)))
            (define-key map (kbd "C-x M-c")  'cic:save-buffers-kill-emacs)
            (define-key map (kbd "C-x C-b")  'buffer-menu)
            ;; (define-key map (kbd "C-x C-b")  'cic:switch-buffer-new-window-below)
            (define-key map (kbd "C-x r i")  'string-insert-rectangle)
            (define-key map (kbd "C-x r \\") 'delete-whitespace-rectangle)
            ;; (define-key map (kbd "C-v")      'hl-line-mode)
            ;; (define-key map (kbd "C-v")      'cic:hl-line-mode-or-disable-visuals)
            ;; TODO: can probably use a more complex key
            (define-key map (kbd "s-v")      'cic:hl-line-mode-or-disable-visuals)
            (define-key map (kbd "M-g h")    'cic:hl-line-mode-or-disable-visuals)
            ;; disable normal movement keys will eventually remap
            map)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  ;; (global-unset-key (kbd "C-v"))
  )
;; (define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "M-a") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-/") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-?") 'isearch-repeat-backward)

(define-minor-mode emacs-stdlib-keys-non-term-mode
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") 'next-error)
            (define-key map (kbd "M-p") 'previous-error)
            ;; backup keys (make non-capitilized next/previous/error
            ;; do for other modes
            ;; (define-key map (kbd "s-N") 'next-error)
            ;; (define-key map (kbd "s-P") 'previous-error)
            map))

;; TODO: what other modes need this
(define-minor-mode emacs-stdlib-keys-non-image-mode
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

(requiring-package (org)
  (define-minor-mode emacs-stdlib-keys-org-mode
    :global t
    ;; :lighter "Some standard keys for org-mode."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c a")      'org-agenda)
              (define-key map (kbd "C-c c")      'org-capture)
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
  (emacs-stdlib-keys-org-mode t)

  (define-minor-mode emacs-stdlib-keys-non-org-mode
    :global t
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-<return>") 'cic:comment-new-line-no-split)
              (define-key map (kbd "M-<return>") 'indent-new-comment-line)
              map))
  (emacs-stdlib-keys-non-org-mode t)
  (defun cic:comment-new-line-no-split ()
    (interactive)
    (end-of-line)
    (insert "\n")
    (comment-dwim nil))
  (defun cic:disable-emacs-stdlib-keys-non-org-mode ()
    (emacs-stdlib-keys-non-org-mode 0))
  (add-hook 'org-mode-hook 'cic:disable-emacs-stdlib-keys-non-org-mode)
  ;; TODO: make this function more universal?
  (add-hook 'special-mode-hook 'cic:disable-emacs-stdlib-keys-non-org-mode))

(defun cic:stdlib-keys-image-mode-setup-hook ()
  ;; TODO: which other ones?
  (when (derived-mode-p 'image-mode)
    (emacs-stdlib-keys-non-image-mode 0)))
(add-hook 'find-file-hook 'cic:stdlib-keys-image-mode-setup-hook)

(defun cic:stdlib-keys-term-setup-hook ()
  (emacs-stdlib-keys-non-term-mode 0))
(add-hook 'term-mode-hook 'cic:stdlib-keys-term-setup-hook)

(defun cic:stdlib-keys-minibuffer-setup-hook ()
  (emacs-stdlib-keys-mode 0)
  (emacs-stdlib-keys-org-mode 0)
  (emacs-stdlib-keys-non-term-mode 0)
  ;; screws up minibuffer
  ;; (local-set-key (kbd "m-v") 'kill-ring-save-whole-word-or-region)
  ;; (local-set-key (kbd "m-v") 'yank)
  )

(add-hook 'minibuffer-setup-hook 'cic:stdlib-keys-minibuffer-setup-hook)

(defun cic:backward-symbol (&optional arg)
  (interactive "P")
  (if arg
      (forward-symbol (- arg))
    (forward-symbol (- 1))))

(defun cic:save-buffers-kill-emacs ()
  (interactive)
  ;; TODO: why do I have my own symbol for this?
  ;;       fix or elaborate?
  ;;       I think I wanted some arg default?
  (save-buffers-kill-emacs))

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

(provide 'emacs-stdlib-keys)
