;;; emacs-config.el --- My generic .emacs settings.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20150914
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
;; These are common settings I use in my .emacs file, which is
;; typically symlinked to a stub that finds and loads this file.
;;
;; Features that might be required by this library:
;;
;; My emacs.el stub generally contains the requiring-package macro,
;; which I include here if necessary for completeness.
;;
;;; Code:

(unless (fboundp 'requiring-package)
    (defvar load-errors-p nil
      "Whether or not there were errors loading on startup.")
    (defmacro* requiring-package ((package &key error-if-fail) &rest forms)
      "Require package but log error instead terminating load."
      `(catch 'requiring-package-fail
         (progn
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
                  (throw 'requiring-package-fail nil))))))))
(put 'requiring-package 'lisp-indent-function 1))

;; set proper fonts, characters, and colors
(global-font-lock-mode t)
(setq initial-frame-alist nil
      default-frame-alist nil)
(add-to-list 'initial-frame-alist
             '(font . "DejaVu Sans Mono-7"))
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-7"))
;; this fixes many coding problems I used to have
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
;; bluish/reddish modeline
(set-face-background 'mode-line "#6699cc")
(set-face-background 'modeline-inactive "#ffaa88")
; quiet, please! No dinging!
(setq visible-bell t)
;; avoid as much window splitting as possible
;; XXXX think I've mostly solved this issue by using the popwin package
(setq split-height-threshold 0
      max-mini-window-height 0.75)
;; show point a little better
(setq-default cursor-type 'box)
;; this is essential for viewing images
;; (setq-default cursor-in-non-selected-windows 'nil)
;; remove the "waiting time", dialog box, and other annoyances at startup
(modify-frame-parameters nil '((wait-for-wm . nil)))
(setq inhibit-splash-screen t
      use-dialog-box nil
      echo-keystrokes 0.4)
(tool-bar-mode 0)
;; XXXX reuse a frame if buffer is already displayed there
(setq-default display-buffer-reuse-frames t)
;; add column numbers to modeline
(column-number-mode t)
;; add date/time to mode line
(setq display-time-day-and-date t
      display-time-24hr-format t)
;; display time in modeline
(display-time)
;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)
(global-auto-revert-mode t)
(setq tags-revert-without-query 1)
;; try not to warn about large files unless necessary
(setq large-file-warning-threshold 100000000000)
;; http://stackoverflow.com/questions/18316665/how-to-improve-emacs-performace-when-view-large-file
(defun cic:large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hooks 'cic:large-file-read-only-hook)
;; set an appropriate tmp directory
;; XXXX: this directory might have to be explicitely created
(setq temporary-file-directory "~/.emacs.d/tmp/")

(auto-compression-mode 1)
;; locale information, just a generic Canadian location as default
(unless (and (boundp 'calendar-latitude) calendar-latitude)
  (setq calendar-latitude 45.4214))
(unless (and (boundp 'calendar-longitude) calendar-longitude)
  (setq calendar-longitude -75.6919))
(unless (and (boundp 'calendar-location-name) calendar-location-name)
  (setq calendar-location-name "Ottawa, Canada"))
(setq calendar-week-start-day 1)
;; set up some queries to be nice
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
;; enable disabled commands and get rid of nusinance commands
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
;; tabs
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq indent-line-function 'insert-tab)
(show-paren-mode t)
;; making scrolling and moving nice
(setq scroll-margin 3
      scroll-step 0
      ;; TODO maybe wish to make this 10 or 0???
      scroll-conservatively 5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search
(setq search-upper-case nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-specific hooks, these should all be builtin to emacs
;; special hooks to delete trailing whitespace and clean up files
;; misc includes from standard emacs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq bibtex-align-at-equal-sign t
      bibtex-field-delimiters 'double-quotes
      bibtex-text-indentation 17
      bibtex-contline-indentation 19)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calc
(setq calc-multiplication-has-precedence nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
(requiring-package (dired-x)
  (setq dired-omit-files-p t
        ;; these help avoid unwanted file operations
        delete-by-moving-to-trash t
        dired-keep-marker-rename nil
        dired-dwim-target t
        dired-omit-files "^\\.+\\|^\\.$\\|^\\.\\.$"
        ;; TODO: would like ".out" but only for latex
        dired-omit-extensions '("_flymake.aux" "_flymake.log" "_flymake.pdf" "_flymake.pdfsync" "_flymake.py"
                                "_.log" "_.pdf" "_.pdfsync"  "_.prv" "_.tex"
                                ".aux" ".bbl" ".blg" ".bst" ".fdb_latexmk" ".fls" ".lof" ".lot" ".pdfsync" ".snm" ".synctex.gz" ".toc"
                                ".pyd" ".pyc" ))
  (setq dired-listing-switches "--group-directories-first -alh")
  ;; set omit by default
  (add-hook 'dired-mode-hook '(lambda ()  (dired-omit-mode 1))))
(requiring-package (flyspell)
  (setq ispell-program-name "aspell"
        ;; I do really write in Canadian, except for all the z's I use
        ispell-dictionary "canadian"
        ispell-extra-args '("--sug-mode=ultra"))
  (setq flyspell-issue-message-flag nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp mode
;; https://stackoverflow.com/questions/18289329/how-to-highlight-all-the-functions-name-in-emacs-lisp-mode
;; TODO: fix up sometime to really get all functions but not let, etc.
(defface font-lock-func-face
    '((nil (:foreground "#7F0055" :weight bold))
      (t (:bold t :italic t)))
  "Font Lock mode face used for function calls."
  :group 'font-lock-highlighting-faces)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
    1 'font-lock-func-face)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(setq ido-create-new-buffer 'always
      ido-file-extensions-order '(".el" ".org" ".py" ".sage" ".tex" ".txt")
      ido-ignore-extensions t)
(unless (boundp 'ido-ignore-files)
  (setq ido-ignore-files nil))
(add-to-list 'ido-ignore-files "\\`_region_")
(add-to-list 'completion-ignored-extensions ".aux")
(add-to-list 'completion-ignored-extensions ".bbl")
(add-to-list 'completion-ignored-extensions ".dvi")
(add-to-list 'completion-ignored-extensions ".fdb_latexmk")
(add-to-list 'completion-ignored-extensions ".fls")
(add-to-list 'completion-ignored-extensions ".org.archive")
(add-to-list 'completion-ignored-extensions ".pdfsync")
(add-to-list 'completion-ignored-extensions ".synctex.gz")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu
(requiring-package (imenu))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-comment
(requiring-package (newcomment))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(requiring-package (org)
  (requiring-package (ox-publish))
  ;; helper functions required for org-mode initialization
  ;; http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/
  ;; prompt for notes with each change state
  (add-to-list 'auto-mode-alist '("\\.org\\.archive" . org-mode))
  (setq org-archive-location "%s.archive::"
        org-todo-keywords
        '((sequence "TODO(!@)" "NEXT(!@)" "WAITING(!@)" "|" "DONE(!@)" "INVALID(!@)"))
        org-todo-keyword-faces '(("TODO" . "firebrick")
                                 ("NEXT" . "orange red")
                                 ("WAITING" . "magenta")
                                 ("DONE" . "green")
                                 ("INVALID" . (:foreground "black" :background "yellow" :weight bold)))
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-use-property-inheritance t
        ;; clean up org agenda
        org-agenda-todo-list-sublevels nil
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-ctrl-k-protect-subtree nil)
  ;; TODO: not sure why this works, if it works, and if I still need it
  (add-hook 'org-mode-hook (lambda ()  (when (display-graphic-p)
                                    (setq org-startup-with-inline-images t
                                          ;; XXXX: want images look reasonable on most systems
                                          ;; TODO: set differently for different screens
                                          org-image-actual-width '(400)))))
  ;; TODO: clean this up!!!!
  (add-hook 'org-mode-hook
            (lambda ()
              (font-lock-add-keywords 'org-mode
                                      '(("^\\s-*\\(\\+ .*\\)$" . ;; org-headline-done
                                         ;; font-lock-warning-face
                                         font-lock-keyword-face
                                         )))))
  ;; literal hyperlinks
  (add-hook 'org-mode-hook (lambda ()
                             (org-remove-from-invisibility-spec '(org-link))
                             (org-restart-font-lock)))
  ;; (setq org-log-done 'time)
  ;; (setq org-log-done 'note)
  ;; most recent notes is always at the top
  (setq org-reverse-note-order t)
  (setq org-agenda-dim-blocked-tasks t)
  ;; show 7 days by default
  (setq org-agenda-span 10)
  ;; how many days early a deadline item will appear
  (setq org-deadline-warning-days 0)
  ;; show days with no tasks, so "free days" can be seen
  (setq org-agenda-show-all-dates t)
  ;; deadlines that are completed will not show up
  (setq org-agenda-skip-deadline-if-done t)
  ;; scheduled events will not show up
  (setq org-agenda-skip-scheduled-if-done t)
  ;; always begin on current day
  (setq org-agenda-start-on-weekday nil)
  ;; org-agenda custom commands, see above newartisans.com link
  ;; keyboard shortcuts for day-agenda, week-agenda, 21-day agenda
  (setq org-agenda-custom-commands
        '(("w" "Agenda for 21 days" agenda "" ((org-agenda-span 21)))))
  (add-hook 'org-capture-mode-hook
            'delete-other-windows))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell mode
;; http://www.emacswiki.org/emacs/AnsiColor
; commint
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; good shell stuff
(setq shell-file-name "/bin/bash")
; from websites
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
(setq tramp-default-method "ssh")
;; this detects my standard bash prompt
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;; disable password prompts for some of my scripts where it is automagic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify
; set up buffer uniquify so I can identify buffers better
(requiring-package (uniquify)
  ;; it would be ideal to force the first parent directory to be included
  (setq uniquify-buffer-name-style 'forward))

(provide 'emacs-config)
