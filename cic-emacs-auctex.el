;;; cic-emacs-auctex.el --- Configuring up AuxTeX the way I like it.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Sun, Apr 14, 2019
;; Version: 20190404
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
;; This provides some configuration that I use for common external
;; packages.  It is not loaded by default but can be loaded by using
;; (require 'cic-emacs-site).
;;
;; This package does not cover Auctex, that is done in a seperate
;; file.
;;
;;; Code:

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
                   (setq preview-auto-cache-preamble t
                         preview-scale-function 1.5)
                   (setq reftex-plug-into-AUCTeX t
                         reftex-cite-view-format "%3a %y, %t, %B, %j %v:%P, %s %<"
                         reftex-toc-include-labels t
                         reftex-index-include-context t
                         reftex-label-alist '(("section" 115 "%S" "~\\ref{%s}"
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
                         '(("zathura" ("nohup zathura-tex-local.sh %o" (mode-io-correlate " --synctex-forward %n:0:%b --synctex-editor-command=\"launch-emacsclient noframe +%{line} %{input}\"")) "zathura")
                           ;; TODO: disable evince for now
                           ;; ("Evince" ("evince" (mode-io-correlate " -i %(outpage)") " %o"))
                           ))
                   (setq TeX-view-program-selection
                         '((output-dvi "DVI Viewer")
                           (output-pdf "zathura")
                           (output-html "HTML Viewer")))
                   (defun cic:view-alternate ()
                     (interactive)
                     (let ((TeX-view-program-selection '((output-dvi "DVI Viewer")
                                                         ;; (output-pdf "Evince")
                                                         (output-html "HTML Viewer"))))
                       (TeX-view)))
                   ;; TODO: a decision must be made which is most convienient
                   (define-key TeX-mode-map (kbd "C-c M-v")   'cic:view-alternate)
                   (define-key TeX-mode-map (kbd "s-c v")     'TeX-view)
                   ;; ;; TODO: replace elsewhere too
                   ;; this conflicts with keys I like
                   ;; (define-key TeX-mode-map (kbd "s-c s-c")   'TeX-view)
                   ;; (define-key TeX-mode-map (kbd "s-c c")     'TeX-view)
                   (define-key TeX-mode-map (kbd "s-c s-v")   'TeX-view)
                   (define-key TeX-mode-map (kbd "s-c M-s-v") 'cic:view-alternate)
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
                     (local-set-key (kbd "s-c i") 'cic:outline)
                     ;; think of "view"
                     ;; TODO: make this something for all modes, should this be hyper-v or ???
                     ;; TODO: used for other things right now...
                     ;; (local-set-key (kbd "H-x") 'reftex-view-crossref)
                     ;; set up references
                     ;; TODO: try these again, do I use them?
                     ;; (local-set-key (kbd "H-r") 'cic:reftex-reference)
                     ;; (local-set-key (kbd "H-f") 'cic:reftex-reference-figure)
                     ;; (local-set-key (kbd "H-e") 'cic:reftex-reference-equation)
                     ;; (local-set-key (kbd "H-s") 'cic:reftex-reference-section)
                     ;; (local-set-key (kbd "H-t") 'cic:reftex-reference-table)
                     ;; jump to process buffer
                     ;; TODO: probably want this to be something else?
                     (local-set-key (kbd "s-c o") 'cic:switch-to-process-buffer)
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
                     (local-set-key (kbd "s-c o") 'cic:switch-to-latex-buffer))
                   (add-hook 'TeX-output-mode-hook 'cic:TeX-output-mode-init)
                   (defun cic:reftex-toc-init ()
                     (local-set-key (kbd "s-c i") 'cic:outline))
                   (add-hook 'reftex-toc-mode-hook     'cic:reftex-toc-init)
                   (defun cic:reftex-select-label-init ()
                     ;; sync with above?
                     ;; (local-set-key (kbd "H-r") 'reftex-select-quit)
                     ;; (local-set-key (kbd "H-f") 'reftex-select-quit)
                     ;; (local-set-key (kbd "H-e") 'reftex-select-quit)
                     ;; (local-set-key (kbd "H-s") 'reftex-select-quit)
                     ;; (local-set-key (kbd "H-t") 'reftex-select-quit)
                     )
                   ;; TODO: eventually unify these functions
                   (defun cic:switch-to-process-buffer ()
                     (interactive)
                     (let ((latex-buffer (current-buffer))
                           (process-buffer (TeX-process-buffer-name (file-name-sans-extension buffer-file-name)))
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
                     (with-current-buffer-min process-buffer
                                              (> (length (remove-if-not (lambda (e)
                                                                          (when (eq (car e) 'error)
                                                                            t))
                                                                        TeX-error-list)) 0)))
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
                   (defun TeX-BibTeX-sentinel-bibtex-always-successful (orig-fun &rest args)
                     (let ((ret (apply orig-fun args)))
                       (setq TeX-command-next TeX-command-default)
                       ;; not sure return value is needed, but OK
                       ret))
                   (advice-add 'TeX-BibTeX-sentinel :around #'TeX-BibTeX-sentinel-bibtex-always-successful)
                   (defun TeX-LaTeX-current-build-filename (orig-fun &rest args)
                     (setq cic:current-build-filename buffer-file-name)
                     (apply orig-fun args))
                   (advice-add 'TeX-command-master :around #'TeX-LaTeX-current-build-filename)
                   (advice-add 'TeX-command        :around #'TeX-LaTeX-current-build-filename)
                   (defun TeX-LaTeX-no-view (orig-fun &rest args)
                     (unless (TeX-process (TeX-master-file))
                       (apply orig-fun args)))
                   (advice-add 'TeX-view :around #'TeX-LaTeX-no-view)
                   ;; TODO: make this act more like org mode
                   ;; TODO: make sure several runs of this file doesn't bugger variable
                   (add-to-list 'TeX-command-list (list "LaTeXdraft" "%`%l%(mode) -draftmode %' %t" 'TeX-run-TeX nil '(latex-mode doctex-mode) :help "Run LaTeX in draft mode."))
                   ;; TODO: maybe???
                   (define-key TeX-mode-map (kbd "C-c C-c") 'align-current)
                   ;; TODO: better master command?
                   ;; (define-key TeX-mode-map (kbd "s-x s-x") 'TeX-command-master)
                   (define-key TeX-mode-map (kbd "s-c f")   'cic:current-compile-full)
                   (define-key TeX-mode-map (kbd "C-c C-b") 'cic:current-compile)
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



(defun first-latex-compile-process-sentinel (process event)
  (mpp (concat "First: " event))
  (when (equal event "finished\n")
    ;; call next
    (mpp "first sentinel")
    (let ((full-filename buffer-file-name))
      (TeX-command "BibTeX" 'TeX-master-file nil)
      (let ((active-process (with-current-file-transient full-filename
                              (TeX-active-process))))
        (if active-process
            (set-process-sentinel active-process 'second-bibtex-compile-process-sentinel)
          (mpp "No first active process"))))))

(defun second-bibtex-compile-process-sentinel (process event)
  (mpp (concat "Second: " event))
  (when (equal event "finished\n")
    ;; call next
    (mpp "second sentinel")
    (let ((full-filename buffer-file-name))
      (cic:current-compile nil)
      (let ((active-process (with-current-file-transient full-filename
                              (TeX-active-process))))
        (if active-process
            (set-process-sentinel active-process 'third-latex-compile-process-sentinel)
          (mpp "No second active process"))))))

(defun third-latex-compile-process-sentinel (process event)
  (mpp (concat "Third: " event))
  (when (equal event "finished\n")
    (mpp "third sentinel")
    (let ((full-filename buffer-file-name))
      (cic:current-compile nil))))

(defun first-latex-full-compile-process-sentinel (process event)
  (mpp (concat "First multi-: " event))
  (when (equal event "finished\n")
    ;; call next
    (mpp "first sentinel multi-")
    (let ((full-filename buffer-file-name))
      (TeX-command "BibTeX" 'TeX-master-file nil)
      (let ((active-process (with-current-file-transient full-filename
                              (TeX-active-process))))
        (if active-process
            (set-process-sentinel active-process 'second-bibtex-full-compile-process-sentinel)
          (mpp "No first multi- active process"))))))

(defun second-bibtex-full-compile-process-sentinel (process event)
  (mpp (concat "Second multi-: " event))
  (when (equal event "finished\n")
    ;; call next
    (mpp "second sentinel multi-")
    (let ((full-filename buffer-file-name))
      (cic:current-compile 'full)
      (let ((active-process (with-current-file-transient full-filename
                              (TeX-active-process))))
        (if active-process
            (set-process-sentinel active-process 'third-latex-full-compile-process-sentinel)
          (mpp "No second multi- active process"))))))

(defun third-latex-full-compile-process-sentinel (process event)
  (mpp (concat "Third multi-: " event))
  (when (equal event "finished\n")
    (mpp "third sentinel multi-")
    (let ((full-filename buffer-file-name))
      (cic:current-compile 'full)
      (let ((active-process (with-current-file-transient full-filename
                              (TeX-active-process))))
        (if active-process
            (set-process-sentinel active-process 'fourth-latex-full-compile-process-sentinel)
          (mpp "No third mutli- active process"))))))

(defun fourth-latex-full-compile-process-sentinel (process event)
  (mpp (concat "Fourth multi-: " event))
  (when (equal event "finished\n")
    (mpp "fourth sentinel multi-")
    (sit-for 1.0)
      (message "Done LaTeX multi-compile!")))

(provide 'cic-emacs-auctex)
