;;; emacs-stdlib-site.el --- Some configure that I use for common
;;; external packages.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu, Aug 27, 2015
;; Version: 20160121
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
;; This provides some configuration that I use for common external
;; packages.  It is not loaded by default but can be loaded by using
;; (require 'emacs-stdlib-site).
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site specific settings
(requiring-package (cl-lib))

(setq browse-url-browser-function 'browse-url-conkeror
      browse-url-generic-program "conkeror")
(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(defun command-line-diff (switch)
  "Allow diffs from the command line.
TODO broken, provided a diff cleanup function too! "
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
;; Usage: emacs -diff file1 file2

;; TODO: see if I can pattern bash* to automode alist
(add-to-list 'auto-mode-alist '("bash_profile_agents" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_functions" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_library" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_completion" . sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom elisp code and keys
;; easypg
(when (file-exists-p "~/.gpg-agent-info.env")
  (let ((gpg-agent
        ;; read the file
         (strip-full (with-temp-buffer
                  (insert-file-contents "~/.gpg-agent-info.env")
                  (buffer-string)))))
    ;; set the variable
    (setenv "GPG_AGENT_INFO" gpg-agent)))
(setq epa-file-select-keys 'silent
      epg-gpg-program "/usr/bin/gpg2")
(epa-file-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; acejump
;; TODO: update key
(requiring-package (ace-jump-mode)
  ;; TODO: not sure which one I want
  (global-set-key (kbd "H-SPC") 'ace-jump-mode)
  ;; (global-set-key (kbd "S-SPC") 'ace-jump-mode)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode-pop-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
;; https://www.gnu.org/software/auctex/manual/auctex/Advice-for-non_002dprivileged-users.html#Advice-for-non_002dprivileged-users
;; http://william.famille-blum.org/blog/static.php?page=static081010-000413
;; http://www.barik.net/archive/2012/07/18/154432/
;; put this as reverse search c:\emacs-24.2\bin\emacsclientw.exe +%l "%f"
(requiring-catch ("auctex")
                 ;; (unload-feature 'tex-site)
                 (require 'tex-site)
                 (require 'texmathp)
                 (setq TeX-PDF-mode t)
                 ;; TODO platform specific configurations?
                 (add-to-list 'auto-mode-alist '("\\.tikz$" . LaTeX-mode))
                 (setq TeX-source-correlate-method 'synctex)
                 (setq TeX-source-correlate-mode t)
                 (setq TeX-source-correlate-start-server t)
                 (setq LaTeX-paragraph-commands
                       '("TODO"))
                 (setq preview-auto-cache-preamble t)
                 (setq preview-scale-function 1.5)
                 (setq reftex-plug-into-AUCTeX t)
                 (add-hook 'LaTeX-mode-hook 'cic:flyspell-init-text)
                 (add-hook 'TeX-mode-hook 'cic:flyspell-init-text)
                 (setq TeX-view-program-selection
                       '((output-dvi "DVI Viewer")
                         (output-pdf "Evince")
                         (output-html "HTML Viewer")))
                 (setq TeX-pr)
                 (add-hook 'LaTeX-mode-hook (lambda () (add-to-list 'TeX-expand-list
                                                                    '("%(masterdir)" (lambda () (file-truename (TeX-master-directory)))))))
                 ;; general syntax stuff
                 (add-hook 'LaTeX-mode-hook (lambda ()
                                              (modify-syntax-entry ?: "w")
                                              (modify-syntax-entry ?: "_")
                                              ;; (font-lock-add-keywords nil
                                              ;;                         '(("\\citemp" 1 font-latex-warning-face t)))))
                                              ))
                 (setq font-latex-match-reference-keywords
                       '(("citemp" "[{")
                         ("citem" "[{")))
                 (setq TeX-view-program-list
                       '(("DVI Viewer" "")
                         ("HTML Viewer" "")))
                 ;; RefTeX
                 (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
                 (setq reftex-plug-into-AUCTeX t)
                 ;; XXXX: specific to University of Saskatchewan thesis template
                 (eval-after-load "reftex"
                   '(add-to-list 'reftex-bibliography-commands "uofsbibliography"))
                 (requiring-package (auctex-latexmk)
                   (unless (assoc "LatexMk" TeX-command-list)
                     (auctex-latexmk-setup)))
                 ;; https://tex.stackexchange.com/questions/170564/synctex-between-emacs-and-evince-which-would-move-focus-for-backward-search
                 (defun raise-client-frame ()
                   (let ((wmctrl (executable-find "wmctrl")))
                     (if wmctrl
                         (start-process "wmctrl" nil wmctrl "-R" (frame-parameter nil 'name)))))
                 ;; This raises the frame when using Evince.
                 (add-hook 'TeX-source-correlate-mode-hook
                           (lambda ()
                             (when (TeX-evince-dbus-p)
                               (dbus-register-signal
                                :session nil "/org/gnome/evince/Window/0"
                                "org.gnome.evince.Window" "SyncSource"
                                (lambda (file linecol &rest ignored)
                                  (TeX-source-correlate-sync-source file linecol ignored)
                                  (raise-client-frame))))))
                 ;; This raises the frame when using all other viewers.
                 (add-hook 'server-switch-hook 'raise-client-frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bash-completion
(requiring-package (bash-completion)
  (bash-completion-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode
(requiring-package (company)
  (global-company-mode)
  (company-quickhelp-mode 1)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conkeror
(autoload 'conkeror-minor-mode "conkeror-minor-mode")
; (add-hook 'js-mode-hook 'conkeror-minor-mode)
(add-hook 'js-mode-hook (lambda ()
                          (when (string-match "conkeror" (buffer-file-name))
                            (conkeror-minor-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emms
(requiring-package (emms-setup)
  (require 'emms-info-libtag)
  (emms-all)
  (emms-default-players)
  (setq emms-info-function '(emms-info-libtag)))

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
;; magit
(requiring-package (magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; TODO: probably not needed
  (global-set-key (kbd "C-x l") 'magit-log-buffer-file))

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
  (eval-after-load "paredit.el"
    (requiring-package (paredit-menu))))
;; TODO move seperately, maybe have an emacs-lisp.el
(requiring-package (eldoc)
  ;; if not already loaded
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ido-mode
; http://masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
; TODO do I want to do something else for C-s C-r
; TODO m-n/p is very good
; TODO ffap
; TODO good completion for org-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(requiring-package (ido)
         (require 'ido-hacks)
         (require 'ido-ubiquitous)
         (ido-mode t)
         (ido-ubiquitous-mode 1)
         (ido-hacks-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin
(requiring-package (popwin)
  (setq popwin:popup-window-height 30)
  (popwin-mode 1)
  (global-set-key (kbd "H-w") popwin:keymap)
  (assq-delete-all 'grep-mode popwin:special-display-config)
  (assq-delete-all 'occur-mode popwin:special-display-config)
  ;; (grep-mode :noselect t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;; add to minor mode and flymake map?
;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
;; TODO: put into message box
;; TODO: navigate functions/defun
(requiring-package (python)
  (add-to-list 'auto-mode-alist '("\\.sage$" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.spyx$" . python-mode))
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
    ;; TODO: possibly change to C-c h
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
                    ;; default is just normal python
                    (t
                     (setq-local python-shell-interpreter "python")
                     (setq-local python-shell-interpreter-args "-i"))))))
      (error (message "Error setting up inferior process!!!"))))
  ;; add the hook to detect interpretor
  (add-hook 'python-mode-hook 'python-detect-interpreter)
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
  )

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
;; smartscan
;; TODO may replace with "evil" when appropriate
;; XXXX: must be done before
(setq smartscan-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "H-s") 'smartscan-symbol-go-forward)
          (define-key map (kbd "H-r") 'smartscan-symbol-go-backward)
          (define-key map (kbd "H-%") 'smartscan-symbol-replace)
          map))
(requiring-package (smartscan)
  (setq smartscan-use-extended-syntax t)
  (global-smartscan-mode 1))

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
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ))
(setq package-check-signature nil)
(setq cic:package-list '(ace-jump-mode
                         apache-mode
                         arduino-mode
                         auctex
                         auctex-latexmk
                         conkeror-minor-mode
                         apropos-fn+var
                         bash-completion
                         benchmark-init
                         bbdb
                         bbdb-ext
                         capture
                         company
                         ;; company-auctex
                         company-quickhelp
                         cl-lib
                         crontab-mode
                         cython-mode
                         dired-avfs
                         dired-rainbow
                         eldoc
                         emms
                         ess
                         flymake-cursor
                         fuzzy-match
                         gh-md
                         gnuplot
                         gnuplot-mode
                         ;; hexrgb
                         ido-hacks
                         ido-ubiquitous
                         idomenu
                         ;; TODO get rid of
                         lacarte
                         ;; letcheck
                         json-mode
                         magit
                         markdown-mode
                         matlab-mode
                         mew
                         org
                         paredit
                         paredit-menu
                         popwin
                         prolog
                         ;; pylint
                         ;; py-import-check
                         rainbow-delimiters
                         readline-complete
                         sicp
                         slime
                         slime-company
                         smartscan
                         ssh-config-mode
                         ssh-tunnels
                         tracwiki-mode
                         twittering-mode
                         ;; w3m
                         xml-rpc
                         vline))

(defun cic:install-packages ()
  (interactive)
  (dolist (package cic:package-list)
    (unless (package-installed-p package)
      (package-install package))))

(provide 'emacs-stdlib-site)
