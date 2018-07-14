;;; emacs-stdlib-site.el --- Some configure that I use for common
;;; external packages.
;;
;; Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu, Aug 27, 2015
;; Version: 20180627
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
;; This provides some configuration that I use for common external
;; packages.  It is not loaded by default but can be loaded by using
;; (require 'emacs-stdlib-site).
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site specific settings
(requiring-package (cl-lib))

;; TODO: prevent dbus loading if already loaded, maybe?
;; TODO: fix this
;; (requiring-package (dbus)
;;   (dbus-get-unique-name :system)
;;   (dbus-get-unique-name :session)
;;   (dbus-init-bus :system)
;;   (dbus-init-bus :session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; things that don't require a require
;; TODO: move this
(requiring-package (man)
  (define-key Man-mode-map (kbd ".") 'scroll-up)
  (define-key Man-mode-map (kbd ",") 'scroll-down)
  (define-key Man-mode-map (kbd "b") 'scroll-down))

(setq browse-url-browser-function 'browse-url-conkeror
      browse-url-generic-program "conkeror")
(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(defun command-line-diff (switch)
  "Allow diffs from the command line.
TODO broken, provided a diff cleanup function too!"
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
;; Usage: emacs -diff file1 file2

;; TODO: move automode elsewhere?
;; TODO: see if I can pattern bash* to automode alist
(add-to-list 'auto-mode-alist '("bash_profile_agents" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_functions"    . sh-mode))
(add-to-list 'auto-mode-alist '("bash_library"        . sh-mode))
(add-to-list 'auto-mode-alist '("bash_logout"         . sh-mode))
(add-to-list 'auto-mode-alist '("psqlrc"              . sql-mode))

(requiring-package (sh-script)
  ;; TODO: find another thing to run executable-interpret
  (define-key sh-mode-map (kbd "C-c C-x") nil)
  ;; my aliases often have
  (add-to-list 'sh-assignment-regexp '(bash . "\\<\\([[:alnum:]_-]+\\)\\(\\[.+\\]\\)?\\+?=")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom elisp code and keys
;; easypg
(when (file-exists-p "~/.gpg-agent-info.env")
  (let ((gpg-agent
        ;; read the file
         (s-trim-full (with-temp-buffer
                  (insert-file-contents "~/.gpg-agent-info.env")
                  (buffer-string)))))
    ;; set the variable
    (setenv "GPG_AGENT_INFO" gpg-agent)))
(setq epa-file-select-keys 'silent
      epg-gpg-program "/usr/bin/gpg2")
(epa-file-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; acejump
;; TODO: don't use it anymore, but might at some point
;; (requiring-package (ace-jump-mode)
;;   ;; (global-set-key (kbd "<menu>") 'ace-jump-mode)
;;   (global-set-key (kbd "S-SPC") 'ace-jump-mode)
;;   (eval-after-load "ace-jump-mode"
;;     '(ace-jump-mode-enable-mark-sync))
;;   ;; TODO: maybe do super space for this
;;   ;; TODO: maybe do easy
;;   (define-key global-map (kbd "C-c SPC") 'ace-jump-mode-pop-mark))

;; advice for tex-command-master
(defvar cic:current-build-filename
  nil
  "Stores the current build filename for asyncronous processes and sentinels.")

(defvar cic:current-source-filename
  nil
  "Stores the current source filename for things such as includeonly.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
;; https://www.gnu.org/software/auctex/manual/auctex/Advice-for-non_002dprivileged-users.html#Advice-for-non_002dprivileged-users
;; http://william.famille-blum.org/blog/static.php?page=static081010-000413
;; http://www.barik.net/archive/2012/07/18/154432/
;; put this as reverse search c:\emacs-24.2\bin\emacsclientw.exe +%l "%f"
(unless cic:emacs-minimal
  ;; auctex wrong fit for a minimal opening
  (requiring-catch ("auctex")
                   (require 'bib-cite)
                   (require 'latex-extra)
                   (require 'tex-site)
                   (require 'tex)
                   (require 'texmathp)
                   ;; TODO: needed on my laptop, not sure why
                   (require 'font-latex)
                   (setq TeX-PDF-mode t)
                   (add-to-list 'auto-mode-alist '("\\.tikz$" . LaTeX-mode))
                   (setq TeX-source-correlate-method 'synctex
                         TeX-source-correlate-mode t
                         TeX-source-correlate-start-server t
                         ;; https://emacs.stackexchange.com/questions/13426/auctex-doesnt-run-bibtex
                         ;; Enable parse on load.
                         TeX-parse-self t
                         TeX-auto-save t
                         TeX-clean-confirm nil
                         ;; the default reindents before newline
                         )
                   (setq LaTeX-paragraph-commands
                         '("TODO"))
                   (setq preview-auto-cache-preamble t)
                   (setq preview-scale-function 1.5)
                   (setq reftex-plug-into-AUCTeX t
                         reftex-cite-view-format "%3a %y, %t, %B, %j %v:%P, %s %<"
                         reftex-toc-include-labels t
                         reftex-index-include-context t
                         reftex-label-alist (list '("section" 115 "%S" "~\\ref{%s}"
                                                    t
                                                    (regexp "parts?" "chapters?" "chap\\." "sections?" "sect?\\." "paragraphs?" "par\\." "\\\\S" "\247" "Teile?" "Kapitel" "Kap\\." "Abschnitte?" "appendi\\(x\\|ces\\)" "App\\." "Anh\"?ange?" "Anh\\."))))
                   (add-hook 'LaTeX-mode-hook 'cic:flyspell-init-text)
                   (add-hook 'TeX-mode-hook 'cic:flyspell-init-text)
                   ;; disable electric-indent-mode, I like paste things like tables verbatim and electric-indent screws it up
                   (defun cic:latex-disable-electic-indent ()
                     ;; TODO: electric-indent-just-newline would be nice
                     (electric-indent-local-mode 0))
                   (add-hook 'LaTeX-mode-hook 'cic:latex-disable-electic-indent)
                   (add-hook 'TeX-mode-hook 'cic:latex-disable-electic-indent)
                   (setq TeX-view-program-list
                         ;; TODO: how to maximize by default
                         '(("zathura" ("nohup zathura-local.sh %o" (mode-io-correlate " --synctex-forward %n:0:%b --synctex-editor-command=\"launch-emacsclient noframe +%{line} %{input}\"")) "zathura")
                           ;; ???
                           ("Evince" ("evince" (mode-io-correlate " -i %(outpage)") " %o"))))
                   (setq TeX-view-program-selection
                         '((output-dvi "DVI Viewer")
                           (output-pdf "zathura")
                           (output-html "HTML Viewer")))
                   (defun cic:view-alternate ()
                     (interactive)
                     (let ((TeX-view-program-selection '((output-dvi "DVI Viewer")
                                                         (output-pdf "Evince")
                                                         (output-html "HTML Viewer"))))
                       (TeX-view)))
                   (define-key TeX-mode-map (kbd "C-c M-v") 'cic:view-alternate)
                   (defun cic:reftex-reference ()
                     (interactive)
                     (let ((reftex-refstyle "\\ref"))
                       (reftex-reference " ")))
                   ;; TODO: would love to combine figure and table
                   (defun cic:reftex-reference-figure ()
                     (interactive)
                     (let ((reftex-refstyle "\\ref"))
                       (reftex-reference "f")))
                   (defun cic:reftex-reference-section ()
                     (interactive)
                     (let ((reftex-refstyle "\\ref"))
                       (reftex-reference "s")))
                   (defun cic:reftex-reference-table ()
                     (interactive)
                     (let ((reftex-refstyle "\\ref"))
                       (reftex-reference "t")))
                   (defun cic:reftex-reference-equation ()
                     (interactive)
                     (let ((reftex-refstyle "\\ref"))
                       (reftex-reference "e")))
                   (defun cic:auctex-latex-init ()
                     (add-to-list 'TeX-expand-list
                                  '("%(masterdir)" (lambda () (file-truename (TeX-master-directory)))))
                     ;; (font-lock-add-keywords nil
                     ;;                         '(("\\citemp" 1 font-latex-warning-face t)))))
                     ;; does not conflict with emacs-otlb
                     (local-set-key (kbd "H-i") 'cic:outline)
                     ;; think of "view"
                     ;; TODO: make this something for all modes, should this be hyper-v or ???
                     ;; TODO: used for other things right now...
                     ;; (local-set-key (kbd "H-x") 'reftex-view-crossref)
                     ;; set up references
                     ;; TODO: try these again, do I use them?
                     (local-set-key (kbd "H-r") 'cic:reftex-reference)
                     (local-set-key (kbd "H-f") 'cic:reftex-reference-figure)
                     (local-set-key (kbd "H-e") 'cic:reftex-reference-equation)
                     (local-set-key (kbd "H-s") 'cic:reftex-reference-section)
                     (local-set-key (kbd "H-t") 'cic:reftex-reference-table)
                     ;; jump to process buffer
                     (local-set-key (kbd "H-o") 'cic:switch-to-process-buffer)
                     ;; init crossref and such
                     (reftex-parse-all)
                     (dolist (file (reftex-get-bibfile-list))
                       (reftex-get-file-buffer-force file))
                     ;; XXXX: adds colon as symbol constituent too
                     (modify-syntax-entry ?: "w"))
                   ;; (setq TeX-pr)
                   (add-hook 'LaTeX-mode-hook 'cic:auctex-latex-init)
                   (defun cic:TeX-output-mode-init ()
                     ;; jump to latex buffer
                     (local-set-key (kbd "H-o") 'cic:switch-to-latex-buffer))
                   (add-hook 'TeX-output-mode-hook 'cic:TeX-output-mode-init)
                   (defun cic:reftex-toc-init ()
                     (local-set-key (kbd "H-i") 'cic:outline))
                   (add-hook 'reftex-toc-mode-hook     'cic:reftex-toc-init)
                   (defun cic:reftex-select-label-init ()
                     ;; sync with above?
                     (local-set-key (kbd "H-r") 'reftex-select-quit)
                     (local-set-key (kbd "H-f") 'reftex-select-quit)
                     (local-set-key (kbd "H-e") 'reftex-select-quit)
                     (local-set-key (kbd "H-s") 'reftex-select-quit)
                     (local-set-key (kbd "H-t") 'reftex-select-quit))
                   ;; TODO: eventually unify these functions
                   (defun cic:switch-to-process-buffer ()
                     (interactive)
                     (let ((latex-buffer (current-buffer))
                           (process-buffer (TeX-process-buffer-name (file-name-sans-extension (buffer-file-name))))
                           (tex-help-window nil))
                       ;; if a window called tex-help is open, just go to next
                       (walk-windows (lambda (w)
                                       (when (equal (buffer-name (window-buffer w)) "*TeX Help*")
                                         (setq tex-help-window w))))
                       (if tex-help-window
                           (next-error)
                         (if (get-buffer process-buffer)
                             (progn
                               (switch-to-buffer process-buffer)
                               (setq-local cic:latex-buffer latex-buffer)
                               ;; reparse and goto first error
                               (goto-char (point-min))
                               (if (cic:any-errors-in-process-buffer (current-buffer))
                                   (TeX-next-error '(4))
                                 (goto-char (point-max))))
                           (message "Process buffer doesn't exist!!!")))))
                   (defun cic:any-errors-in-process-buffer (process-buffer)
                     (with-current-buffer process-buffer
                       (save-excursion
                         (goto-char (point-min))
                         (> (length (delq nil (mapcar (lambda (e)
                                                        (when (eq (car e) 'error)
                                                          t))
                                                      TeX-error-list))) 0))))
                   (defun cic:switch-to-latex-buffer ()
                     (interactive)
                     (if cic:latex-buffer
                         (switch-to-buffer cic:latex-buffer)
                       (message "No default latex buffer!!!")))
                   (add-hook 'reftex-select-label-mode-hook 'cic:reftex-select-label-init)
                   (setq font-latex-match-reference-keywords
                         '(("citemp" "[{")
                           ("citem" "[{")))
                   ;; RefTeX
                   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
                   (setq reftex-plug-into-AUCTeX t)
                   ;; XXXX: specific to University of Saskatchewan thesis template
                   (eval-after-load "reftex"
                     '(add-to-list 'reftex-bibliography-commands "uofsbibliography"))
                   ;; TODO: do I remap these to other things
                   ;; (define-key TeX-mode-map (kbd "C-c C-b")  nil)
                   ;; (define-key TeX-mode-map (kbd "C-c C-c")  nil)
                   (defun TeX-BibTeX-sentinel-bibtex-always-successful (orig-fun &rest args)
                     (let ((ret (apply orig-fun args)))
                       (setq TeX-command-next TeX-command-default)
                       ;; not sure return value is needed, but OK
                       ret))
                   (advice-add 'TeX-BibTeX-sentinel :around #'TeX-BibTeX-sentinel-bibtex-always-successful)
                   (defun TeX-LaTeX-current-build-filename (orig-fun &rest args)
                     (setq cic:current-build-filename (buffer-file-name))
                     (apply orig-fun args))
                   (advice-add 'TeX-command-master :around #'TeX-LaTeX-current-build-filename)
                   (advice-add 'TeX-command        :around #'TeX-LaTeX-current-build-filename)
                   (defun TeX-LaTeX-no-view (orig-fun &rest args)
                     (when (not (TeX-process (TeX-master-file)))
                       (apply orig-fun args)))
                   (advice-add 'TeX-view :around #'TeX-LaTeX-no-view)
                   ;; TODO: make this act more like org mode
                   ;; TODO: make sure several runs of this file doesn't bugger variable
                   (cic:add-to-alist 'TeX-command-list "LaTeXdraft" "%`%l%(mode) -draftmode %' %t" 'TeX-run-TeX nil '(latex-mode doctex-mode) :help "Run LaTeX in draft mode.")
                   ;; TODO: maybe???
                   (define-key TeX-mode-map (kbd "C-c C-c") 'align-current)
                   (define-key TeX-mode-map (kbd "s-x s-x") 'TeX-command-master)
                   ;; (define-key TeX-mode-map (kbd "s-b")     'cic:current-compile)
                   ;; TODO: want function symbol instead of lambda for this
                   (define-key TeX-mode-map (kbd "s-B")     '(lambda ()
                                                               (interactive)
                                                               (TeX-command "BibTeX" 'TeX-master-file nil)))
                   (define-key TeX-mode-map (kbd "s-x b")   '(lambda ()
                                                               (interactive)
                                                               (TeX-command "BibTeX" 'TeX-master-file nil)))
                   (define-key TeX-mode-map (kbd "s-x c")    '(lambda ()
                                                                (interactive)
                                                                (TeX-command "LaTeXdraft" 'TeX-master-file nil)))
                   ;; TODO: do I ever want this back, should I replace something else?
                   ;; (define-key TeX-mode-map (kbd "C-c C-b")  )
                   ;; prevent tex mode from overwriting next/previous error
                   (define-key TeX-mode-map (kbd "s-N") (lambda () (interactive)
                                                          (call-interactively 'next-error)))
                   (define-key TeX-mode-map  (kbd "s-P") (lambda () (interactive)
                                                           (call-interactively 'previous-error)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotate
(requiring-package (annotate)
  ;; only enable annotate for read-only files
  (add-hook 'find-file-hook 'cic:annotate-enable)
  (defun cic:annotate-enable ()
    (when (string-match "\\.ro\\." buffer-file-name)
      (annotate-mode)))
  (defun annotate--remove-annotation-property--inhibit-readonly (orig-fun &rest args)
    (let ((old-buffer-read-only buffer-read-only))
      (read-only-mode -1)
      (apply orig-fun args)
      (when old-buffer-read-only
        (read-only-mode 1))))
  (advice-add 'annotate--remove-annotation-property :around #'annotate--remove-annotation-property--inhibit-readonly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bash-completion
(requiring-package (bash-completion)
  (bash-completion-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clippy
;; TODO: replace with something less silly
(requiring-package (clippy)
  ;; TODO: experimental
  (global-set-key (kbd "M-,") 'clippy-describe-function)
  ;; (global-set-key (kbd "M-h") 'clippy-describe-function)
  (global-set-key (kbd "M-h") 'cic:clippy-describe)
  (defvar cic:clippy-last-show nil
    "")
  (defun cic:clippy-describe ()
    (interactive)
    (if (eq last-command 'cic:clippy-describe)
        (progn
          (if (eq cic:clippy-last-show 'function)
              (progn
                (call-interactively 'clippy-describe-variable)
                (setq cic:clippy-last-show 'variable))
            (progn
              (call-interactively 'clippy-describe-function)
              (setq cic:clippy-last-show 'function))))
      (progn
        (call-interactively 'clippy-describe-function)
        (setq cic:clippy-last-show 'function))))
  ;; gives the table heading when at a table
  ;; TODO: find definition of words this way too
  (defun clippy-org-describe ()
    (interactive)
    (when (org-at-table-p)
      (let* ((the-string (cic:org-get-column-heading))
             (the-string-filled (with-temp-buffer
                                  (insert the-string)
                                  (goto-char (point-min))
                                  (fill-paragraph)
                                  (buffer-substring (point-min) (point-max)))))
        (clippy-say the-string-filled))))
  (define-key org-mode-map (kbd "M-h") 'clippy-org-describe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crontab-mode
;; TODO: replace, not in melpa?
;; (requiring-package (crontab-mode)
;;   (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
;;   (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
;;   ;; TODO: do I want anything else?
;;   (add-to-list 'auto-mode-alist '("crontab"              . crontab-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode
;; TODO: reenable
;; (requiring-package (company)
;;   (global-company-mode)
;;   (company-quickhelp-mode 1)
;;   ;; (add-hook 'after-init-hook 'global-company-mode)
;;   ;; TODO: for now
;;   ;; (define-key company-quickhelp-mode-map (kbd "s-q")  company-quickhelp-manual-begin)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conkeror
(autoload 'conkeror-minor-mode "conkeror-minor-mode")
; (add-hook 'js-mode-hook 'conkeror-minor-mode)
(add-hook 'js-mode-hook (lambda ()
                          (when (string-match "conkeror" (buffer-file-name))
                            (conkeror-minor-mode 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; dired-sort
;; (requiring-package (dired-sort)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emms
(unless cic:emacs-minimal
  ;; this sexp adds a LOT to load time and I don't use emms much right now
  ;; minimal does not need it
  (requiring-package (emms-setup)
    (require 'emms-info-libtag)
    (emms-all)
    (emms-default-players)
    (requiring-package (emms-mark)
      ;; TODO: always says no first track
      ;; (setq emms-playlist-default-major-mode 'emms-mark-mode)
      )
    (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
    (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
    (setq emms-info-function '(emms-info-libtag))
    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
    ;; XXXX: redefine for my computers running Debian in 2018+, take out -c option
    (defun emms-volume-amixer-change (amount)
      "Change amixer master volume by AMOUNT."
      (message "Playback channels: %s"
               (with-temp-buffer
                 (when (zerop
                        (call-process "amixer" nil (current-buffer) nil
                                      ;; "-c"
                                      ;; (format "%d" emms-volume-amixer-card)
                                      "sset" emms-volume-amixer-control
                                      (format "%d%%%s" (abs amount)
                                              (if (< amount 0) "-" "+"))))
                   (if (re-search-backward "\\[\\([0-9]+%\\)\\]" nil t)
                       (match-string 1))))))
    (define-emms-simple-player mpg321-custom '(file url)
      (emms-player-simple-regexp "mp3" "mp2")
      "mpg321" "--gain" "10")
    (delq 'emms-player-mpv emms-player-list)
    (delq 'emms-player-mpg321-custom emms-player-list)
    (delq 'emms-player-vlc emms-player-list)
    (delq 'emms-player-vlc-playlist emms-player-list)
    (pushnew 'emms-player-mpg321-custom emms-player-list))
    ;; (requiring-package (emms-player-mpv)
    ;;   (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
    ;;   (delq 'emms-player-mpv emms-player-list)
    ;;   (pushnew 'emms-player-mpv emms-player-list))
    )

(requiring-package (flyspell)
  ;; TODO: change this if I need it
  (setq flyspell-auto-correct-binding nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(unless cic:emacs-minimal
  ;; no flycheck for a minimal mode
  (requiring-package (flycheck)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically nil)
    (defun cic:flycheck-buffer-or-list (&optional arg)
      (interactive "P")
      (if arg
          (flycheck-clear)
        (if (eq last-command 'cic:flycheck-buffer-or-list)
            (if (get-buffer-window "*Flycheck errors*")
                (delete-window (get-buffer-window "*Flycheck errors*"))
              (flycheck-list-errors))
          (flycheck-buffer))))
    ;; TODO: reenalbe when it works for my workflow again
    ;; (global-set-key (kbd "C-c C-x") 'cic:flycheck-buffer-or-list)
    ;; I use these for other stuff
    ;; (global-set-key (kbd "s-,") 'flycheck-previous-error)
    ;; (global-set-key (kbd "s-.") 'flycheck-next-error)
    ;; hacks for my own stuff
    ;; (put 'python-flake8    (intern "flycheck-modes")     '(python-mode sage-mode))
    ;; (put 'python-pycompile (intern "flycheck-modes")     '(python-mode sage-mode))
    ;; (requiring-package (flycheck-pyflakes)
    ;;   (put 'python-pyflakes    (intern "flycheck-modes") '(python-mode sage-mode)))
    ;; I still install these, but most of my stuff is not good enough to
    ;; stick to rigid style
    ;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
    ;; TODO: need to specify good defaults
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; free keys
(requiring-package (free-keys)
  (setq free-keys-modifiers '("" "C" "M" "C-M" "s" "H")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot
(requiring-package (gnuplot)
  (setq auto-mode-alist
        (append
         (list
          '("\\.gnuplot$" . gnuplot-mode)
          '("\\.gp$" . gnuplot-mode))
         auto-mode-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add keys to the image-mode
(requiring-package (image-mode)
  (defun cic:image-goto-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  ;; find window size
  ;; (image-display-size (image-get-display-property))
  ;; (image-size)
  ;; get frame size

  (defun cic:image-top-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           move-width)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))))

  (defun cic:image-bottom-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))
      (unless (<= image-height window-height)
        (image-scroll-up (round image-height)))))

  (defun cic:image-left-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  (defun cic:image-right-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (image-forward-hscroll (round image-width)))
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  (defun cic:image-toggle-zoom ()
    (interactive)
    (unless (boundp 'cic:image-last-zoom)
      (setq-local cic:image-last-zoom 'default))
    ;; TODO: somehow save this default zoom
    ;; TODO: indicate zoom somewhere
    )

  ;; TODO: pop back to dired, or go to next on deletion?
  (defun cic:image-delete ()
    (interactive)
    )

  (defconst cic:image-move-lines-columns 8
    "Amount of move in images by lines")

  (defun cic:image-previous-lines ()
    (interactive)
    (image-previous-line cic:image-move-lines-columns))

  (defun cic:image-next-lines ()
    (interactive)
    (image-next-line cic:image-move-lines-columns))

  (defun cic:image-backward-hscroll ()
    (interactive)
    (image-backward-hscroll cic:image-move-lines-columns))

  (defun cic:image-forward-hscroll ()
    (interactive)
    (image-forward-hscroll cic:image-move-lines-columns))

  ;; TODO: need down to be middle lower of buffer
  (define-key image-mode-map (kbd "e")               'image-scroll-up)
  (define-key image-mode-map (kbd "s")               'image-backward-hscroll)
  (define-key image-mode-map (kbd "d")               'image-scroll-down)
  (define-key image-mode-map (kbd "f")               'image-forward-hscroll)
  (define-key image-mode-map (kbd "D")               'cic:dired-preview-image-delete)
  (define-key image-mode-map (kbd "H-i")             'cic:dired-preview-image-mode)
  (define-key image-mode-map (kbd "<up>")            'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<kp-up>")         'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<down>")          'cic:image-next-lines)
  (define-key image-mode-map (kbd "<kp-down>")       'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<left>")          'cic:image-backward-hscroll)
  (define-key image-mode-map (kbd "<prior>")         'cic:image-backward-hscroll)
  (define-key image-mode-map (kbd "<right>")         'cic:image-forward-hscroll)
  (define-key image-mode-map (kbd "<next>")          'cic:image-forward-hscroll)
  ;; move to extreme points in image
  (define-key image-mode-map (kbd "C-<kp-up>")       'cic:image-top-middle)
  (define-key image-mode-map (kbd "C-<up>")          'cic:image-top-middle)
  (define-key image-mode-map (kbd "C-<kp-down>")     'cic:image-bottom-middle)
  (define-key image-mode-map (kbd "C-<down>")        'cic:image-bottom-middle)
  (define-key image-mode-map (kbd "C-<kp-left>")     'cic:image-left-middle)
  (define-key image-mode-map (kbd "C-<prior>")       'cic:image-left-middle)
  (define-key image-mode-map (kbd "C-<kp-right>")    'cic:image-right-middle)
  (define-key image-mode-map (kbd "C-<next>")        'cic:image-right-middle)
  (define-key image-mode-map (kbd "C-<kp-home>")     'image-bob)
  (define-key image-mode-map (kbd "C-<f17>")         'image-bob)
  (define-key image-mode-map (kbd "C-<kp-prior>")    (lambda (&optional arg) (interactive) (image-bob) (image-eol arg)))
  (define-key image-mode-map (kbd "C-<f19>")         (lambda (&optional arg) (interactive) (image-bob) (image-eol arg)))
  (define-key image-mode-map (kbd "C-<kp-end>")      (lambda (&optional arg) (interactive) (image-eob) (image-bol arg)))
  (define-key image-mode-map (kbd "C-<f13>")         (lambda (&optional arg) (interactive) (image-eob) (image-bol arg)))
  (define-key image-mode-map (kbd "C-<kp-next>")     'image-eob)
  (define-key image-mode-map (kbd "C-<f15>")         'image-eob)
  ;; TODO: kp-begin is 5, toggle fit width/height/etc., maybe make this 0? would love 5 to go to middle
  ;; TODO: figure these out
  (define-key image-mode-map (kbd "C-<kp-begin>")    'cic:image-goto-middle)
  (define-key image-mode-map (kbd "<kp-insert>")     'cic:image-toggle-zoom)
  ;; the C-S- prevents accidental deletion
  (define-key image-mode-map (kbd "C-S-<kp-delete>") 'cic:image-delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;; TODO: do not use this right now
;; (requiring-package (magit)
;;   (global-set-key (kbd "C-x g") 'magit-status)
;;   ;; TODO: probably not needed
;;   (global-set-key (kbd "C-x l") 'magit-log-buffer-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nxml
(requiring-package (nxml-mode)
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site-specific lisp hooks
;; paredit and eldoc
(defun lisp-mode-site ()
  ;; (local-set-key (kbd "H-/") 'lisp-complete-symbol)
  ;; TODO set up autocomplete sources
  ;; (when (boundp 'ac-sources)
  ;;   (add-to-list 'ac-sources 'ac-source-etags)))
  )
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-site)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-site)
(requiring-package (paredit)
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  ;; TODO: what if I eval this??? then what
  (with-current-buffer "*scratch*"
    (enable-paredit-mode))
  (eval-after-load "paredit.el"
    (requiring-package (paredit-menu)))
  ;; change keys around for easy navigation of parenthesis
  ;; XXXX: change capitalization keys
  (global-set-key (kbd "M-C") 'capitalize-word)
  (global-set-key (kbd "M-L") 'downcase-word)
  (global-set-key (kbd "M-U") 'upcase-word))

(requiring-package (eldoc)
  ;; if not already loaded
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  ;; TODO: what if I eval this??? then what
  (with-current-buffer "*scratch*"
    (eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;; http://masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; TODO do I want to do something else for C-s C-r
;; TODO m-n/p is very good
;; TODO ffap
;; TODO good completion for org-mode
;; XXXX: keeping things fast for now
;; (setq ido-enable-flex-matching t)
(when cic:ido-enable
  ;; TODO: want option for flex matching all the time
  (setq ido-enable-flex-matching t)
  ;; XXXX: if t many things like describe-function is slow
  (setq ido-everywhere nil)
  (requiring-package (ido)
    (require 'cic-ido-hacks)
    (requiring-package (ido-completing-read+)
      ;; TODO: add any others
      ;; TODO: change to ido-completing-read+
      ;; (setq ido-ubiquitous-command-overrides '((disable prefix "org-capture")))
      (add-to-list 'ido-cr+-function-blacklist "org-capture.*"))
    (set-face-foreground 'ido-only-match "DarkGreen")
    (set-face-attribute  'ido-only-match nil :weight 'bold)
    ;; TODO: below here de-commented
    (ido-mode t)
    ;; TODO: change to ido-completing-read+
    (ido-ubiquitous-mode t)
    (ido-hacks-mode t)
    ;; much faster performance than ido-vertical than, especially for describe-function
    ;; no compatible with ido-hacks because it overrides ido-decorations
    ;; https://www.emacswiki.org/emacs/InteractivelyDoThings#toc24
    (setq ido-decorations (quote ("\n->  " "" "\n    " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n-> [" "]")))
    (defun ido-disable-line-truncation ()
      (set (make-local-variable 'truncate-lines) nil))
    (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
    ;; TODO: are there keys I should disable
    (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
    (add-hook 'ido-setup-hook 'ido-define-keys)

    (setq ido-max-prospects 50)))

(requiring-package (image+)
  (eval-after-load 'image '(require 'image+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
(requiring-package (json-mode)
  (require 'rx)
  (defconst cic:json-mode-comment-re
    (rx line-start
        (zero-or-more whitespace)
        (group
         (char ?/)
         (char ?/)
         (zero-or-more not-newline)
         eol)))
  ;; TODO: make sure this does not grow list indefinitely as I reload this file
  ;; TODO: using warning rather than comment because comments are not valid vanilla json syntax
  (add-to-list 'json-font-lock-keywords-1 (list cic:json-mode-comment-re 1 font-lock-warning-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jumplist
;; TODO: do not use this right now
;; (requiring-package (jumplist)
;;   (global-set-key (kbd "s-<") 'jumplist-previous)
;;   (global-set-key (kbd "s->") 'jumplist-next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-bullets
;; TODO: really slows stuff down
(requiring-package (org-bullets)
  (add-hook 'org-mode-hook (lambda ()
                             ;; for my emacs-otlb package, want to put this somewhere else maybe?
                             ;; cuts an n=1 test from 55s to 4-7s
                             ;; TODO: where else do I want to kill org-bullets?
                             (unless  (and (buffer-file-name) (string-match "pedestrian-log.*\\.org" (buffer-file-name)))
                               (org-bullets-mode 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pos-tip
(requiring-package (pos-tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python and sage
;; add to minor mode and flymake map?
;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
;; TODO: put into message box
;; TODO: navigate functions/defun
(requiring-package (python)
  ;; set up python keys
  ;; TODO: figure this
  ;; (define-key python-mode-map (kbd "M-[") 'python-indent-shift-left)
  ;; (define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)
  ;; (define-key python-mode-map (kbd "s-b") 'cic:current-compile)
  ;; (define-key python-mode-map (kbd "s-x") 'cic:current-compile)
  (define-key python-mode-map (kbd "s-x c") 'cic:check-python)
  (defun cic:check-python ()
    (interactive)
    (when (buffer-live-p (get-buffer "*cic-python-check*"))
      (with-current-buffer "*cic-python-check*"
        (erase-buffer)
        (goto-char (point-min))))
    (let ((return-code (call-process "python" nil "*cic-python-check*" nil "-m" "py_compile" (buffer-file-name))))
      (if (equal return-code 0)
          (message "Syntax check passed!!!")
        ;; TODO: flash if failed...
        ;; (message "***!!!Syntax check failed!!!***")
        (message (with-current-buffer "*cic-python-check*" (buffer-substring (point-min) (point-max)))))))
  (defconst python-fulldoc-setup-code
    "def __PYDOC_get_fulldoc(obj):
    try:
        import inspect
        if hasattr(obj, 'startswith'):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(
                    *inspect.getargspec(target)
                )
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
    except:
        doc = ''
    try:
        exec('print doc')
    except SyntaxError:
        print(doc)"
    "Python code to setup full documentation retrieval.")

  (defconst python-fulldoc-string-code
    "__PYDOC_get_fulldoc('''%s''')\n"
    "Python code used to get the full documentation string of an object.")

  (defun python-fulldoc-at-point (symbol)
    (interactive
     (let ((symbol (python-info-current-symbol t))
           (enable-recursive-minibuffers t))
       (list (read-string (if symbol
                              (format "Describe symbol (default %s): " symbol)
                            "Describe symbol: ")
                          nil nil symbol))))
    (python-shell-send-string python-fulldoc-setup-code (python-shell-get-process))
    (let ((fulldocs (python-shell-send-string-no-output (format python-fulldoc-string-code symbol) (python-shell-get-process))))
      (with-current-buffer-create "*Python docstring*"
        (let ((inhibit-read-only t))
          (special-mode)
          (erase-buffer)
          (insert fulldocs)
          (insert "\n")
          (goto-char (point-min))))
      (pop-to-buffer (get-buffer "*Python docstring*"))))

  ;; add hook to detect interpretor
  (defun python-eldoc-setup ()
    ;; remove any old advice
    (advice-add 'python-shell-get-or-create-process :around #'python-shell-get-or-create-process--non-interactive)
    (python-shell-send-buffer)
    (eldoc-mode)
    (advice-remove 'python-shell-get-or-create-process #'python-shell-get-or-create-process--non-interactive)
    ;; XXXX: change name
    (define-key python-mode-map (kbd "C-c h") 'python-fulldoc-at-point))

  (defun python-shell-get-or-create-process--non-interactive (orig-fun &rest args)
    (apply orig-fun (list (python-shell-parse-command) nil nil)))

  (defun python-detect-interpreter ()
    ;; read first line for shebang
    (condition-case nil
        (progn
          (save-excursion
            (goto-char (point-min))
            (let ((theline (cic:get-current-line)))
              (cond ((string-match "sage.*python"  theline)
                     (progn
                       (setq-local python-shell-interpreter "sage")
                       (setq-local python-shell-interpreter-args "-python -i")))
                    ;; XXXX: match sage without Python
                    ((string-match "sage"  theline)
                     (setq-local python-shell-interpreter "sage")
                     (setq-local python-shell-interpreter-args ""))
                    ;; default is just default built-in python
                    (t
                     (setq-local python-shell-interpreter "python")
                     (setq-local python-shell-interpreter-args "-i"))))))
      (error (message "Error setting up inferior process!!!"))))
  ;; add the hook to detect interpretor
  (add-hook 'python-mode-hook 'python-detect-interpreter)
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; After installation of the spkg, you must add something like the
  ;; following to your .emacs:
  ;; XXXX: obviously this is specific to a particular installation
  ;; TODO: maybe have a load log warning thing
  ;; find latest installed sage and add to load path
  (let ((sagedir (car (reverse (sort (delq nil (mapcar (lambda (s) (when (string-match "sage-.*" s) s)) (directory-files "/opt"))) 'string<)))))
    (add-to-list 'load-path (concat "/opt/" sagedir "/local/share/emacs/site-lisp/sage-mode"))
    ;; TODO: I will need to make my own sage version
    (if (featurep 'sage)
        (requiring-package (sage)
          (require 'sage "sage")
          ;; TODO: have an else for requiring-package
          (add-to-list 'auto-mode-alist '("\\.sage$" . python-mode))
          (add-to-list 'auto-mode-alist '("\\.spyx$" . python-mode)))
      (message "Sage not found so not loaded!"))
    (setq sage-command (concat "/opt/" sagedir "/sage"))
    ;; If you want sage-view to typeset all your output and display plot()
    ;; commands inline, uncomment the following line and configure sage-view:
    ;; (add-hook 'sage-startup-after-prompt-hook 'sage-view)
    ;; In particular customize the variable sage-view-default-commands.
    ;; Using sage-view to typeset output requires a working LaTeX
    ;; installation with the preview package.
    ;; Also consider running (customize-group 'sage) to see more options.
    )
  (define-key python-mode-map (kbd "C-c C-v") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow
(requiring-package (rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readline
(requiring-package (readline-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
(setq scheme-program-name "mit-scheme")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime
(requiring-package (slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql
(requiring-package (sql)
  (defun cic:sql-setup ()
    (sql-highlight-postgres-keywords)
    (setq-local tab-width 4))
  ;; TODO: change this to recognize something once I use more sql variations
  (add-hook 'sql-mode-hook 'cic:sql-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartscan
;; TODO: disabling global modes that may impact performance
;; TODO may replace with "evil" when appropriate
;; XXXX: must be done before
;; (setq smartscan-map
;;       (let ((map (make-sparse-keymap)))
;;         ;; TODO: change to something good soon
;;         ;;       replaced with mousewheel emulation
;;         ;; (define-key map (kbd "M-,") 'smartscan-symbol-go-backward)
;;         ;; (define-key map (kbd "M-.") 'smartscan-symbol-go-forward)
;;         ;; TODO: better replace
;;         (define-key map (kbd "H-%") 'smartscan-symbol-replace)
;;         map))
;; (requiring-package (smartscan)
;;   (setq smartscan-use-extended-syntax t)
;;   (global-smartscan-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spice-mode
(requiring-package (spice-mode)
  ;; TODO: add more as required
  ;; TODO: configure for ngspice
  (add-to-list 'auto-mode-alist '("\\.cir$" . spice-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(requiring-package (vc)
  (setq vc-follow-symlinks t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; w3m
(setq w3m-default-display-inline-images t)
(setq w3m-use-cookies t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vline
(add-hook 'calendar-mode-hook (lambda () (setq cursor-type 'box) (hl-line-mode) (vline-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some nice packages I like
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq package-check-signature nil)
(setq cic:package-list '(
                         ;; apropos-fn+var
                         ;; company-auctex
                         ;; flymake-cursor
                         ;; hexrgb
                         ;; ido-grid-mode
                         ;; ido-ubiquitous
                         ;; ido-vertical-mode
                         ;; ido-vertical-mode
                         ;; image-dired+
                         ;; letcheck
                         ;; py-import-check
                         ;; pylint
                         ;; TODO get rid of
                         ;; TODO: change to ido-completing-read+
                         ;; TODO: get rid of flymake
                         ;; TODO: unsure about popwin mode
                         ace-jump-mode
                         annotate
                         apache-mode
                         apt-sources-list
                         ;; arduino-mode
                         async
                         auctex
                         auto-overlays
                         bash-completion
                         bbdb
                         bbdb-ext
                         benchmark-init
                         biblio
                         capture
                         clippy
                         company
                         ;; company-arduino
                         company-c-headers
                         company-irony
                         company-math
                         company-quickhelp
                         conkeror-minor-mode
                         ;; crontab-mode
                         csv-mode
                         cython-mode
                         dash
                         dired-avfs
                         dired-hacks-utils
                         dired-icon
                         dired-rainbow
                         eimp
                         eldoc
                         emms
                         ;; emms-player-mpv
                         epl
                         ess
                         f
                         flycheck
                         flycheck-bashate
                         flycheck-checkbashisms
                         flycheck-cython
                         flycheck-package
                         flycheck-pyflakes
                         flymake-cursor
                         free-keys
                         ;; fuzzy-match
                         gh-md
                         ghub
                         ;; git-commit
                         gnuplot
                         gnuplot-mode
                         ido-completing-read+
                         ido-hacks
                         ;; ido-ubiquitous
                         idomenu
                         image+
                         json-mode
                         jumplist
                         ;; lacarte
                         latex-extra
                         ;; magit
                         markdown-mode
                         math-symbol-lists
                         matlab-mode
                         memoize
                         mew
                         nhexl-mode
                         org
                         org-bullets
                         package-lint
                         paredit
                         paredit-menu
                         peep-dired
                         pos-tip
                         prolog
                         ;; pythonic
                         rainbow-delimiters
                         readline-complete
                         s
                         sage-shell-mode
                         sicp
                         simple-call-tree
                         simple-httpd
                         slime
                         slime-company
                         smartscan
                         spice-mode
                         ssh-config-mode
                         ssh-tunnels
                         tracwiki-mode
                         twittering-mode
                         ;; vline
                         w3m
                         wanderlust
                         with-editor
                         xml-rpc

                         ))

(defun cic:install-packages ()
  (interactive)
  (let ((failed nil))
    (dolist (package cic:package-list)
      ;; package-built-in-p something else to check?
      (unless (cic:package-installed-manager-p package)
        ;; there are sometimes errors, catch them
        (condition-case error-string
            (package-install package)
          (error (progn
                   (setq filed t)
                   (message (concat "Failed to install package: " (symbol-name package) " ")))))))
    (when failed
      (message "Some packages failed to install!"))))
;; error-string

(defun cic:update-packages ()
  (package-list-packages)
  (with-current-buffer "*Packages*"
    (package-menu-mark-upgrades)
    ;; copied from package.el.gz
    (let (install-list)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq cmd (char-after))
          (unless (eq cmd ?\s)
            (setq pkg-desc (tabulated-list-get-id))
            (when (eq cmd ?I)
              (push pkg-desc install-list)))
          (forward-line)))
      (when install-list
        (dolist (package install-list)
          (condition-case error-string
              (progn
                (package-install package)
                (message (concat "Successfully upgraded package: " (symbol-name (elt package 1)))))
            (error (message (concat "Failed to upgrade package: " (symbol-name (elt package 1)) " " error-string)))))))
    (revert-buffer)))

(defun cic:package-installed-manager-p (package)
  ""
  ;; TODO: not completely tested, deals with stuff I fixed manually
  (cdr (assq package package-alist)))

(provide 'emacs-stdlib-site)
