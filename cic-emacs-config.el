;;; cic-emacs-config.el --- My generic .emacs settings.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20190414
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
;; These are common settings I immediately load from a stub .emacs
;; file.  All featuers required by this library should be present by
;; default in Emacs.
;;
;; Although my emacs.el stub generally contains the requiring-package
;; macro, which I include here if necessary for completeness.
;;
;;; Code:

;; make sure warnings do not pop up, set this first
(setq warning-minimum-level :error)

(unless (fboundp 'requiring-package)
  (defvar load-errors-p nil
    "Whether or not there were errors loading on startup.")
  (defmacro* requiring-package ((package &key error-if-fail) &rest forms)
    "Require package but log error instead terminating load."
    `(catch 'requiring-package-fail
       (condition-case error-string
           (progn
             (require ',package)
             ,@forms)
         (error
          (let ((msg (format  "Failed to load package %s "
                              (symbol-name ',package))))
            (setq load-errors-p t)
            (with-current-buffer (get-buffer-create "*Load log*")
              (insert msg "\n")
              (insert (format "The error was: %s\n" error-string)))
            (if ,error-if-fail
                (error msg)
              (throw 'requiring-package-fail nil)))))))
  (put 'requiring-package 'lisp-indent-function 1))

;; make sure I only use the chosen theme in a terminal that is not connected to the daemon
;; TODO: it does not seem easy to pop up terminal Emacs client with a dark theme when graphical clients are running light themes
(unless (or (daemonp) (and (fboundp 'server-running-p) (server-running-p)) (display-graphic-p))
  ;; this is in emacs by default and OK for now
  ;; TODO: this is not perfect, test whether themes exist?
  (load-theme 'tango-dark))

;; set proper fonts, characters, and colors
(global-font-lock-mode t)
(setq initial-frame-alist nil
      default-frame-alist nil)
;; XXXX: on my machine (AMD APU with video built into CPU) "Liberation
;; Mono-8" (and Courier) are much faster than "DejaVu Sans Mono-7" for
;; dired with 2000-4000 files, with "DejaVu Sans Mono-7" taking up to
;; 0.5s redraw.  The "Monospace Regular" that comes when "emacs -Q" is
;; run is also quite slow
(add-to-list 'initial-frame-alist
             ;; '(font . "DejaVu Sans Mono-7")
             '(font . "Liberation Mono-8")
             ;; '(font . "Droid Sans Mono-8")
	     ;; '(font . "Noto Sans Mono CJK JP Regular-7")
             ;; '(font . "Anonymous-8")
             )
(add-to-list 'default-frame-alist
             ;; '(font . "DejaVu Sans Mono-7")
             '(font . "Liberation Mono-8")
             ;; '(font . "Droid Sans Mono-8")
	     ;; '(font . "Noto Sans Mono CJK JP Regular-7")
             ;; '(font . "Anonymous-8")
             )
;; this fixes many coding problems I used to have
(prefer-coding-system 'utf-8)
;; TODO: the following breaks zip files.... and other things that read bytes into buffer....
;;       need a finer grained way of fixing it
;;       see https://emacs.stackexchange.com/questions/10146/cant-open-zip-files-in-emacs
;; (setq coding-system-for-read  'utf-8
;;       coding-system-for-write 'utf-8)

(setq-default cursor-type 'box)

;; bluish/reddish modeline
;; TODO: test for deamonp here
(defun cic:configure-modeline-color ()
  (if (and (fboundp 'server-running-p) (server-running-p))
      (progn
        (set-face-background 'mode-line "#6699ff")
        (set-face-background 'modeline-inactive "#ffaa88"))
    (progn
      (set-face-background 'mode-line "#00ffff")
      (set-face-background 'modeline-inactive "#ff00ff"))))
(add-hook 'before-make-frame-hook 'cic:configure-modeline-color)
;; run for when starting without server
(cic:configure-modeline-color)

;; this seems like a nice default on all systems
(setq default-scroll-bar-width 10)
(setq-default indicate-buffer-boundaries '((t . left)))

;; more intuitive percentage in mode-line
(setcar mode-line-position
        '(:eval (format "%4.1f%%%%" (/ (point) 0.01 (point-max)))))

;; disable superfluous mail check in modeline and make time look like what I want
(setq display-time-string-forms '((if (and (not display-time-format) display-time-day-and-date)
                                      (format-time-string "%a %b %e " now)
                                    "")
                                  (propertize
                                   (format-time-string
                                    (or display-time-format
                                        (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                    now)
                                   'help-echo
                                   (format-time-string "%a %b %e, %Y" now))
                                  load))

;; highlight in olive green, visible on many of my modes
(set-face-attribute 'region nil :background "#c0ff3e")
; quiet, please! No dinging!
(setq visible-bell t)
;; avoid as much window splitting as possible
(setq split-height-threshold 0
      max-mini-window-height 0.75)
;; truncate lines
(setq truncate-lines t
      line-move-visual nil)
;; limit display lines
;; this prevents extremely large buffers from loading slowly
(setq line-number-display-limit 262144)
;; don't need prefix for popping mark
(setq set-mark-command-repeat-pop t)
;; remove the "waiting time", dialog box, and other annoyances at startup
(modify-frame-parameters nil '((wait-for-wm . nil)))
(setq inhibit-splash-screen t
      use-dialog-box nil
      echo-keystrokes 0.4)
(tool-bar-mode 0)
;; XXXX: reuse a frame if buffer is already displayed there
(setq-default display-buffer-reuse-frames t)
;; add column numbers to modeline
(column-number-mode t)
;; add date/time to mode line
(setq display-time-day-and-date t
      display-time-24hr-format t)
;; display time in modeline
(display-time)
;; disable backup and autosave
(setq backup-inhibited t
      auto-save-default nil)
(requiring-package (autorevert)
  (global-auto-revert-mode t)
  ;; TODO: don't indicate about reverting for now, could this cause me problems?
  ;;       selective autorevert?
  (setq auto-revert-verbose nil))
(setq tags-revert-without-query 1)
;; try not to warn about large files unless really really necessary
(setq large-file-warning-threshold 100000000000)
;; TODO: can become a problem with some files used for capturing and logging
;;       should only do for large files that are not text (e.g. org, above a certain threshold)
(defun cic:large-file-read-only-hook ()
  "If a file is over a given size (default 100mb), make the buffer
read only."
  (when (> (buffer-size) (* 1024 1024 100))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
;; TODO: experimental disable...
;; (add-hook 'find-file-hooks 'cic:large-file-read-only-hook)
;; TODO: conflicts with ido and C-x d, and much more!
;; (ffap-bindings)
;; set an appropriate tmp directory
;; XXXX: this directory might have to be explicitely created
(setq temporary-file-directory "~/.emacs.d/tmp/")
;; clipboard and tooltips
(setq x-select-enable-primary t
      x-select-enable-clipboard t
      x-gtk-use-system-tooltips nil
      kill-do-not-save-duplicates t
      history-delete-duplicates t
      select-active-regions nil)
(auto-compression-mode 1)
;; case
(setq sort-fold-case t)
;; locale information, just a generic Canadian location as default
;; this is then reset elsewhere to my actual location
(unless (and (boundp 'calendar-latitude) calendar-latitude)
  (setq calendar-latitude 45.4214))
(unless (and (boundp 'calendar-longitude) calendar-longitude)
  (setq calendar-longitude -75.6919))
(unless (and (boundp 'calendar-location-name) calendar-location-name)
  (setq calendar-location-name "Ottawa, Canada"))
(setq calendar-week-start-day 1)
;; set up some queries to be nice and verbose
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
;; enable disabled commands that I like and get rid of nuisance commands
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
;; tabs
(setq-default indent-tabs-mode nil
              ;; I only use tabs in external code and this makes viewing included .el code much easier
              tab-width 8)
(show-paren-mode t)
;; making scrolling and moving nice
(setq scroll-margin 3
      scroll-step 0
      ;; think I want this 15 so I can see around searches
      scroll-conservatively 15)
;; TODO: how does this affect LaTeX?
(setq comment-auto-fill-only-comments t
      ;; I like this for quoting
      comment-empty-lines t)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
;; https://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we
  are not at the end of the line, then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts
  comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-dwim-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search
(setq search-upper-case nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; also search, but also for debugging
(setq next-error-recenter '(4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keys
;; TODO: move these to a special key remapping file, this makes them easy to disable if needed
;; these use C-esdf for movement and remap many common keys
;; CUDA functions are mapped to Hyper, which left Control on my systems so when
;; I use other computers I have muscle memory for these shortcuts
;; unset default keys, will set these as something else eventually
(global-set-key (kbd "C-M-b")              nil)
(global-set-key (kbd "C-n")                nil)
(global-set-key (kbd "C-p")                nil)
(global-set-key (kbd "M-b")                nil)
(global-set-key (kbd "C-w")                nil)
(global-set-key (kbd "C-x C-SPC")          nil)
;; unset the facemenu key
(global-set-key (kbd "M-o")                nil)
(global-set-key (kbd "M-f")                'forward-symbol)
(global-set-key (kbd "M-w")                'other-window)
(global-set-key (kbd "H-c")                'kill-ring-save-whole-word-or-region)
(global-set-key (kbd "H-v")                'yank)
(global-set-key (kbd "M-v")                'yank-pop)
(global-set-key (kbd "H-f")                'isearch-forward)
(global-set-key (kbd "M-H-f")              'isearch-forward-regexp)
(global-set-key (kbd "H-r")                'isearch-backward)
(global-set-key (kbd "M-H-r")              'isearch-backward-regexp)
(global-set-key (kbd "C-,")                'scroll-down-command)
(global-set-key (kbd "C-.")                'scroll-up-command)
(global-set-key (kbd "C-h")                'delete-char)
(global-set-key (kbd "M-h")                'kill-word)
(global-set-key (kbd "C-a")                'move-beginning-of-line)
;; new movement keys
(global-set-key (kbd "C-e")                'previous-line)
(global-set-key (kbd "C-d")                'next-line)
(global-set-key (kbd "C-s")                'backward-char)
(global-set-key (kbd "C-M-s")              'backward-sexp)
(global-set-key (kbd "C-r")                'move-end-of-line)
(global-set-key (kbd "C-y")                'scroll-down-command)
(global-set-key (kbd "C-b")                'scroll-down-command)
(define-key isearch-mode-map (kbd "H-f")   'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-H-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "H-r")   'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-H-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "H-v")   'isearch-yank-kill)
(define-key isearch-mode-map (kbd "M-v")   'isearch-yank-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-specific configuration, these should all be builtin to emacs
;; special hooks to delete trailing whitespace and clean up files misc
;; includes from standard emacs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq bibtex-align-at-equal-sign t
      bibtex-field-delimiters 'double-quotes
      bibtex-text-indentation 17
      bibtex-contline-indentation 19)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calc
;; TODO: I haven't use calc in a while so I may revist this
(setq calc-multiplication-has-precedence nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
(requiring-package (dired-aux))
(requiring-package (dired-x)
  (setq dired-omit-files-p t
        ;; these help avoid unwanted file operations
        delete-by-moving-to-trash t
        dired-keep-marker-rename nil
        dired-dwim-target t
        dired-omit-files "^\\.+\\|^\\.$\\|^\\.\\.$"
        ;; TODO: would like ".out" but only for latex directories
        ;; TODO: set most of these with auctex config maybe?
        dired-omit-extensions '("_flymake.aux" "_flymake.log" "_flymake.pdf" "_flymake.pdfsync" "_flymake.py"
                                "_.log" "_.pdf" "_.pdfsync"  "_.prv" "_.tex"
                                ".aux" ".bbl" ".blg" ".bst" ".fdb_latexmk" ".fls" ".lof" ".lot" ".pdfsync" ".snm" ".synctex.gz" ".toc"
                                ".pyd" ".pyc" ".sage.py"))
  (setq dired-listing-switches "--group-directories-first -ahlv")
  (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
  ;; TODO: I may not want to keep this key remapping
  (define-key dired-mode-map (kbd "r")   'revert-buffer)
  (define-key dired-mode-map (kbd "g")   nil)
  (defun cic:dired-mode-minor-modes ()
    (dired-omit-mode 1)
    (hl-line-mode 1))
  ;; set omit by default
  (add-hook 'dired-mode-hook 'cic:dired-mode-minor-modes))

(requiring-package (wdired)
  (setq wdired-use-dired-vertical-movement 'sometimes
        wdired-confirm-overwrite t
        ;; I do not want link targets editable!!!
        wdired-allow-to-redirect-links nil))

(requiring-package (info)
  ;; make info navigate like other programs and modes I use
  ;; will probably want more key remapings
  (define-key Info-mode-map (kbd "b") 'Info-scroll-down))

(add-hook 'package-menu-mode-hook 'cic:init-hl-line-mode)
(add-hook 'occur-mode-hook 'cic:init-hl-line-mode)
(defun cic:init-hl-line-mode ()
    (hl-line-mode 1))
(requiring-package (ispell)
  (setq ispell-program-name "aspell"
        ;; I do really write in Canadian, except for all the z's I use
        ispell-dictionary "canadian"
        ispell-extra-args '("--sug-mode=ultra")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp mode
;; https://stackoverflow.com/questions/18289329/how-to-highlight-all-the-functions-name-in-emacs-lisp-mode
;; TODO: fix up sometime to really get all functions currently loaded into Emacs, but not mark let bindings, etc.
(defface font-lock-func-face
    '((nil (:foreground "#7F0055" :weight bold))
      (t (:bold t :italic t)))
  "Font Lock mode face used for function calls."
  :group 'font-lock-highlighting-faces)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
    1 'font-lock-func-face)))
;; marking
;; should override company mode quickhelp, done elsewhere
;; TODO: make both work
(define-key emacs-lisp-mode-map (kbd "C-x C-h") 'mark-defun)
(define-key emacs-lisp-mode-map (kbd "M-,")     'find-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;; it's useful to be able to switch ido on/off for debugging
(setq cic:ido-enable t)
(when cic:ido-enable
  (setq ido-create-new-buffer 'always
        ido-file-extensions-order '(".el" ".org" ".py" ".sage" ".tex" ".txt")
        ido-ignore-extensions t)
  (unless (boundp 'ido-ignore-files)
    (setq ido-ignore-files nil))
  (add-to-list 'ido-ignore-files "\\`_region_"))
;; TODO: this is not great because each add-to-list has to check entire list
;;       add and then remove duplicates
(add-to-list 'completion-ignored-extensions ".aux")
(add-to-list 'completion-ignored-extensions ".bbl")
(add-to-list 'completion-ignored-extensions ".dvi")
(add-to-list 'completion-ignored-extensions ".fdb_latexmk")
(add-to-list 'completion-ignored-extensions ".fls")
(add-to-list 'completion-ignored-extensions ".org.archive")
(add-to-list 'completion-ignored-extensions ".pdfsync")
(add-to-list 'completion-ignored-extensions ".sage.py")
(add-to-list 'completion-ignored-extensions ".synctex.gz")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu
(requiring-package (imenu)
  (setq imenu-auto-rescan t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-mode
;; TODO: find out how to animate images by default
(setq image-animate-loop t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
(requiring-package (misc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-comment
(requiring-package (newcomment))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(requiring-package (org)
  (requiring-package (ox-publish))
  ;; helper functions required for org-mode initialization
  (add-to-list 'auto-mode-alist '("\\.org\\.archive" . org-mode))
  ;; remap some standard org-mode
  (define-key org-mode-map (kbd "C-S-<return>") 'org-insert-subheading)
  (define-key org-mode-map (kbd "C-,") 'scroll-down-command)
  (define-key org-mode-map (kbd "C-.") 'scroll-up-command)
  (define-key org-mode-map (kbd "M-h") 'kill-word)
  ;; TODO: take out once this is verified to not be needed
  (requiring-package (org-compat))
  (setq org-src-lang-modes '(("elisp"     . emacs-lisp)
                             ("sql"       . sql)
                             ("python"    . python-mode)
                             ("sage"      . sage-shell:sage-mode)
                             ("screen"    . shell-script)
                             ("bash"      . sh)
                             ("shell"     . sh)
                             ("C"         . c)
                             ("cpp"       . c++)
                             ("C++"       . c++)
                             ("java-mode" . java-mode)
                             ("sqlite"    . sql)
                             ("calc"      . fundamental)
                             ("asymptote" . asy)
                             ("dot"       . fundamental)))
  (setq org-fontify-quote-and-verse-blocks t)
  ;; I like these faces
  (set-face-foreground 'org-block "dark slate blue")
  (set-face-foreground 'org-quote "deep pink")
  (set-face-foreground 'org-code "deep pink")
  (set-face-background 'org-code "light blue")
  ;; TODO: yellowish, would be great for BEGIN_QUOTE
  ;; (set-face-foreground 'org-quote "#8b6508")
  (setq org-archive-location "%s.archive::"
        org-todo-keywords    '((sequence "NOTE(!@)"
                                         "REFILE(!@)"
                                         ;;;;;;;;;;;;;;;;;;;;
                                         "TODO(!@)"
                                         "NEXT(!@)"
                                         "INPROGRESS(!@)"
                                         "PRIORITY(!@)"
                                         "WAITING(!@)"
                                         "|"
                                         "DONE(!@)"
                                         "DUPLICATE(!@)"
                                         "CANT(!@)"
                                         "INVALID(!@)"))
        ;; TODO: probably want a slightly different color contrast for note
        org-todo-keyword-faces '(("NOTE"             . (:foreground "yellow"      :background "dark green" :weight bold))
                                 ("REFILE"           . (:foreground "red"         :background "cyan"       :weight bold))
                                 ;;;;;;;;;;;;;;;;;;;;
                                 ("TODO"             . "firebrick")
                                 ("NEXT"             . "orange red")
                                 ("INPROGRESS"       . (:foreground "magenta"     :background "gold"       :weight bold))
                                 ("PRIORITY"         . (:foreground "light blue"  :background "red"        :weight bold))
                                 ("WAITING"          . (:foreground "magenta"     :background "gold"       :weight bold))
                                 ;;;;;;;;;;;;;;;;;;;;
                                 ("DONE"             . (:foreground "dark orange" :background "blue"       :weight bold))
                                 ("DUPLICATE"        . (:foreground "yellow"      :background "blue"       :weight bold))
                                 ("CANT"             . (:foreground "yellow"      :background "blue"       :weight bold))
                                 ;; XXXX: does not work well with green highlighting
                                 ;; ("DONE"             . (:foreground "green" :background "blue" :weight bold))
                                 ("INVALID"          . (:foreground "yellow"      :background "blue"       :weight bold)))
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-use-property-inheritance t
        ;; clean up org agenda
        org-agenda-todo-list-sublevels nil
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-ctrl-k-protect-subtree nil
        org-cycle-global-at-bob t
        org-cycle-include-plain-lists nil
        org-fontify-whole-heading-line t
        org-cycle-level-after-item/entry-creation nil
        org-ellipsis "➤➤➤"
        ;; org-ellipsis "⤵"
        ;; org-ellipsis "▼"
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        org-fontify-emphasized-text t)
  ;; having a function to enable images works much better on extremely large files because it allows explicit disabling
  ;; otherwise there is code in org-mode that is difficult to selectively disable and time-consuming for large files
  (defun org-image-enable ()
    ;; XXXX: yes this eq is on purpose, in order to let derived modes set their own behaviour
    (when (eq major-mode 'org-mode)
      (org-display-inline-images)))
  ;; disable inline images by default, then toggle them on in the hook above
  (setq org-startup-with-inline-images nil
        org-image-actual-width 128)
  ;; this allows the file local variable org-image-actual-width to take effect
  (put 'org-image-actual-width 'safe-local-variable 'integerp)
  (add-hook 'hack-local-variables-hook 'org-image-enable)
  ;; literal hyperlinks setup
  ;; (add-hook 'org-mode-hook 'org-list-highlight-setup)
  (add-hook 'org-mode-hook 'org-literal-hyperlinks-setup)
  (defun org-literal-hyperlinks-setup ()
    (unless (and buffer-file-name (string-match "help\\.org" buffer-file-name))
      (remove-from-invisibility-spec '(org-link))
      (org-restart-font-lock)))
  (add-hook 'org-mode-hook 'org-indent-mode-setup)
  (defun org-indent-mode-setup ()
    ;; XXXX: yes this eq is on purpose, rather than using derived-mode-p, in order to let derived modes set their own behaviour
    (when (eq major-mode 'org-mode)
      (org-indent-mode 1)))
  ;; most recent notes is always at the top
  (setq org-reverse-note-order t)
  (setq org-agenda-dim-blocked-tasks t
        ;; show 10 days by default
        org-agenda-span 10
        org-deadline-warning-days 0
        ;; how many days early a deadline item will appear
        ;; show days with no tasks, so "free days" can be seen
        org-agenda-show-all-dates t
        ;; deadlines that are completed will not show up
        org-agenda-skip-deadline-if-done t
        ;; scheduled events will not show up
        org-agenda-skip-scheduled-if-done t
        ;; always begin on current day
        org-agenda-start-on-weekday nil
        ;; org-agenda custom commands, see above newartisans.com link
        ;; keyboard shortcuts for day-agenda, week-agenda, 21-day agenda
        org-agenda-custom-commands '(("w" "Agenda for 21 days" agenda "" ((org-agenda-span 21)))))
  (add-hook 'org-capture-mode-hook
            'delete-other-windows))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell mode
; commint
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq shell-file-name "/bin/bash")
(setq comint-scross-to-bottom-on-input t    ;; always insert at the bottom
      comint-scroll-to-bottom-on-output nil ;; always add output at the bottom
      comint-scroll-show-maximum-output t   ;; scroll to show max possible output
      comint-input-ignoredups t             ;; no duplicates in command history
      comint-completion-addsuffix t         ;; insert space/slash after file completion
      comint-buffer-maximum-size 20000      ;; max length of the buffer in lines
      comint-prompt-read-only nil           ;; if this is t, it breaks shell-command and many other things
      comint-get-old-input (lambda () "")   ;; what to run when i press enter on a
                                            ;; line above the current prompt
      comint-input-ring-size 5000           ;; max shell history size
      protect-buffer-bury-p nil)
;; this tends to work best for shells in Emacs
(setenv "PAGER" "cat")
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; track directory when cding in a shell
(defun cic:track-shell-directory/procfs ()
    (shell-dirtrack-mode 0)
    (add-hook 'comint-preoutput-filter-functions
              (lambda (str)
                (prog1 str
                  (when (string-match comint-prompt-regexp str)
                    (cd (file-symlink-p
                         (format "/proc/%s/cwd" (process-id
                                                 (get-buffer-process
                                                  (current-buffer)))))))))
              nil t))
(add-hook 'shell-mode-hook 'cic:track-shell-directory/procfs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp-mode
(setq tramp-default-method       "ssh"
      ;; this detects my standard bash prompt
      tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify
; set up buffer uniquify so I can identify buffers better
(requiring-package (uniquify)
  ;; it would be ideal to force the first parent directory to be included
  (setq uniquify-buffer-name-style 'forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; winner-mode
(requiring-package (winner)
  (winner-mode 1))

(requiring-package (vc)
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (add-hook 'find-file-hook 'cic:remove-vc-modeline))

(defun cic:remove-vc-modeline ()
  (setq mode-line-format (remove '(vc-mode vc-mode) mode-line-format)))

;; https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; TODO: another alternative
;; (eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing
;; https://www.emacswiki.org/emacs/CupsInEmacs
(setq lpr-command "gtklp"
      ps-lpr-command "gtklp"
      ;; TODO: 6 point font is nice I think
      ps-font-size (cons 6 6)
      ps-landscape-mode nil)
;; (setq ps-landscape-mode t)

;; TODO: move command later, add to a key
;; TODO: h==hardcopy for now, reevaluate later, make something else
(global-set-key (kbd "s-c h") 'print-landscape-region)
(defun print-landscape ()
  (interactive)
  (let ((ps-font-size (cons 6 6))
        (ps-landscape-mode t)
        (ps-paper-type 'letter))
    (if (region-active-p)
        (ps-print-region (region-beginning) (region-end))
      (ps-print-region (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
(defvar cic:already-added-modeline
  nil
  "Indicate whether additional modeline indicators have already
  been added.")

(defvar cic:modeline-dirty
  nil
  "Indicate whether modeline is abnormal.")

;; this makes the modeline red if "*Load log* buffer exists
;; this can quickly allow seeing if there's a problem rather than working for hours with a problem present
(unless cic:already-added-modeline
  ;; mode-line-stuff
  (unless (some 'identity (mapcar (lambda (e) (ignore-errors (string-match "case:" e))) mode-line-format))
    (setq-default mode-line-format (append mode-line-format (list
                                                             '(:eval (cond ((get-buffer "*Load log*")
                                                                            (set-face-background 'mode-line "#ff0000")
                                                                            (setq cic:modeline-dirty t)
                                                                            "load:ERR")
                                                                           (t
                                                                            (when cic:modeline-dirty
                                                                              (cic:configure-modeline-color)
                                                                              (setq cic:modeline-dirty nil))
                                                                            "load:OK ")))
                                                             "case:"
                                                             '(:eval (if case-fold-search
                                                                         "insensitive "
                                                                       (propertize "sensitive   " 'font-lock-face '(:foreground "yellow"))))))))
  ;; XXXX: remove this line for debugging the above....
  (setq cic:already-added-modeline t))

(provide 'cic-emacs-config)
