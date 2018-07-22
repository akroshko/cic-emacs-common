;;; emacs-stdlib-commands.el --- Standard emacs commands that should
;;; have many uses.
;;
;; Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20180213
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
;; These are relatively generic commands, often the extensions of ones
;; in common packages.
;;
;; Features that might be required by this library:
;;
;; Generally only requires a basic Emacs installation.
;; TODO: Want to list requirements eventually, but see
;; emacs-config.el for some potential requires.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands that can typically be run from key-strokes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode commands
;; taken from http://orgmode.org/worg/org-hacks.html#sec-1-3-1
(defun cic:org-end-of-next-heading (&optional arg)
  "Go to the end of the next heading.  ARG doesn't do anything
right now.
TODO: incomplete but still useful right now"
  (interactive "P")
  ; somehow deal with level 2 vs level 3
  (let (current-heading-level
        heading-empty)
    (save-excursion
      (unless (cic:org-headline-p (cic:get-current-line))
        (org-back-to-heading))
      (setq current-heading-level (org-outline-level)))
    (when (equal current-heading-level 3)
      (outline-up-heading 1))
    (when (equal current-heading-level 1)
      ;; go to level 2
      )
    (hide-subtree)
    (org-forward-heading-same-level 1)
    ;; this can be weird if no subheadings, same line...
    (save-excursion
      (let ((current-line (cic:get-current-line)))
        (hide-subtree)
        (org-cycle)
        (outline-end-of-subtree)
        (when (string= current-line (cic:get-current-line))
          (setq heading-empty t))))
    (unless heading-empty
      (hide-subtree)
      (org-cycle)
      (outline-end-of-subtree)
      (org-back-to-heading)
      (beginning-of-line))))

;; TODO: move somewhere else?
(define-key org-mode-map (kbd "H-t")   'cic:org-todo)
;; (define-key org-mode-map (kbd "s-:")   'cic:org-todo)
(define-key org-mode-map (kbd "C-H-t") 'cic:org-todo-set)
(define-key org-mode-map (kbd "H-T")   'cic:org-todo-clear)
;; (define-key org-mode-map (kbd "s-;")   'cic:org-todo-inprogress-done)
(define-key org-mode-map (kbd "s-:")   'cic:org-todo-cycle-note)
(define-key org-mode-map (kbd "s-'")   'cic:org-todo-cycle-done)
(define-key org-mode-map (kbd "s-;")   'cic:org-todo-cycle-not-done)

(defun cic:org-at-todo-p ()
  ;; TODO: there are much much better ways to do this!
  (let (matched
        ;; TODO: this should be changed to read org-todo-keywords
        (todo-keyword-strings '("NOTE" "TODO" "NEXT" "PRIORITY" "INPROGRESS" "DUPLICATE" "CANT" "WAITING" "DONE" "INVALID"))
        (the-current-line (cic:get-current-line)))
    (dolist (tks todo-keyword-strings)
      (when (string-match tks the-current-line)
        (setq matched t)))
    matched))

(defun cic:org-todo (arg)
  (interactive "P")
  (cond ((eq major-mode 'org-agenda-mode)
         (when (cic:org-at-todo-p)
           (org-agenda-todo arg)))
        (t
         (org-todo arg))))

;; clear with prefix
(defun cic:org-todo-inprogress-done (&optional arg)
  (interactive "P")
  (if arg
      (org-todo 'none)
    (let ((the-current-line (cic:get-current-line)))
      (cond ((string-match "INPROGRESS" the-current-line)
             (org-todo "CANT"))
            ((string-match "CANT" the-current-line)
             (org-todo "DONE"))
            (t
             (org-todo "INPROGRESS"))))))

(defun cic:org-todo-cycle-note (&optional arg)
  (interactive "P")
  (if arg
      (org-todo 'none)
    (let ((the-current-line (cic:get-current-line)))
      (cond ((string-match "^\\** NOTE" the-current-line)
             (org-todo "REFILE"))
            ((string-match "^\\** REFILE" the-current-line)
             (org-todo "NOTE"))
            (t
             (org-todo "NOTE"))))))

(defun cic:org-todo-cycle-not-done (&optional arg)
  (interactive "P")
  (if arg
      (org-todo 'none)
    (let ((the-current-line (cic:get-current-line)))
      (cond ;; ((string-match "NOTE" the-current-line)
            ;;  (org-todo "TODO"))
            ((string-match "^\\** TODO"       the-current-line)
             (org-todo "NEXT"))
            ((string-match "^\\** NEXT"       the-current-line)
             (org-todo "PRIORITY"))
            ((string-match "^\\** PRIORITY"   the-current-line)
             (org-todo "INPROGRESS"))
            ((string-match "^\\** INPROGRESS" the-current-line)
             (org-todo "WAITING"))
            ((string-match "^\\** WAITING"    the-current-line)
             (org-todo "TODO"))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; TODO: DONE goes to INPROGRESS, INVALID goes to TOD
            ((string-match "^\\** DONE"      the-current-line)
             (org-todo "INPROGRESS"))
            ((string-match "^\\** DUPLICATE" the-current-line)
             (org-todo "TODO"))
            ((string-match "^\\** CANT"      the-current-line)
             (org-todo "TODO"))
            ((string-match "^\\** INVALID"   the-current-line)
             (org-todo "TODO"))
            (t
             ;; goto NOTE by default? for now?
             (org-todo "TODO"))))))

(defun cic:org-todo-cycle-done (&optional arg)
  (interactive "P")
  (if arg
      (org-todo 'none)
    (let ((the-current-line (cic:get-current-line)))
      (cond ((string-match "^\\** DONE"      the-current-line)
             (org-todo "DUPLICATE"))
            ((string-match "^\\** DUPLICATE" the-current-line)
             (org-todo "CANT"))
            ((string-match "^\\** CANT"      the-current-line)
             (org-todo "INVALID"))
            ((string-match "^\\** INVALID"   the-current-line)
             (org-todo "DONE"))
            (t
             (org-todo "DONE"))))))

(defun cic:org-todo-set (arg)
  (interactive "P")
  (if arg
      (org-todo 'todo)
    (org-todo 'done)))

(defun cic:org-todo-clear (arg)
  (interactive "P")
  (org-todo 'none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse commands
;; hello

(defun cic:browse-url-at-point-conkeror ()
  "Find the URL at point and browse in the conkeror web browser."
  (interactive)
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic (cic:url-at-point-or-line 'url))))

(defun cic:browse-url-at-point-firefox ()
  "Find the URL at point and browse in the Firefox web browser."
  (interactive)
  ;; XXXX: may not be best way, but prevents firefox from closing when I restart emacs
  ;; TODO: find better way
  (let ((browse-url-generic-program "firefox")
        (browse-url-generic-args    '("-P" "default")))
    ;; (browse-url-firefox (cic:url-at-point-or-line 'url))
    (browse-url-generic (cic:url-at-point-or-line 'url))))

(defun cic:browse-url-at-point-chromium ()
  "Find the URL at point and browse in the Chromium web browser."
  (interactive)
  ;; XXXX: may not be best way, but prevents based on prevening firefox from closing when I restart emacs
  ;; TODO: find better way
  (let ((browse-url-generic-program "chromium")
        (browse-url-generic-args    '("--temp-profile")))
    (browse-url-generic (cic:url-at-point-or-line 'url))))

(defun cic:browse-url-at-point-gnome-web ()
  "Find the URL at point and browse in the Firefox web browser."
  (interactive)
  (browse-url-epiphany (cic:url-at-point-or-line 'url)))

(defun cic:browse-url-at-point-w3m ()
  "Find the URL at point and browse in the w3m web browser."
  (interactive)
  (w3m-browse-url (cic:url-at-point-or-line 'url)))

(defun cic:url-at-point-or-line (&optional current-line)
  "Find the URL at point and return.  Find the url in
CURRENT-LINE if specified."
  (let ((url (cic:trim-after-double-colon (thing-at-point 'url))))
    (unless url
      (unless current-line
        (setq current-line (cic:get-current-line)))
      (let ((url-start (string-match cic:emacs-stdlib-url-regexp current-line)))
        (when url-start
          (setq url (cic:trim-after-double-colon (substring current-line url-start (match-end 0)))))))
    url))

(defun cic:org-insert-two-level (&optional arg)
  "This is a really good function actually.  It inserts a top
level with a bottom level heading"
  ;; TODO almost there, works different for items or headlines, need
  ;; to detect
  (interactive)
  (move-end-of-line nil)
  (org-insert-heading)
  (move-end-of-line nil)
  (org-insert-heading)
  ;; (next-line)
  (org-metaright)
  (previous-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired....
;; TODO fix when getting to end of dired buffer, shows buffer and acts dumb
;;      proabably need to fix up save-current-buffer, etc.
(defun cic:next-file-dired (&optional motion)
  "Goto the next file from the current file as listed by dired.
Effect of motion is to go to previous."
  (interactive)
  (unless motion
    (setq motion 1))
  (let ((current-filename (buffer-file-name))
        (keep-going t)
        next-filename)
    (save-current-buffer
      (dired default-directory)
      (revert-buffer)
      (when current-filename
        (dired-goto-file current-filename)
        (while keep-going
          (condition-case nil
              (progn
                (dired-next-line motion)
                (when  (and (dired-get-filename) (file-regular-p (dired-get-filename)))
                  (setq next-filename (dired-get-filename))
                  (setq keep-going nil)))
            (error (setq keep-going nil))))))
    (when next-filename
      (find-file next-filename))))

(defun cic:next-file-dired-pagedown (&optional motion)
  "Page down in current file, then when at end goto the next file
from the current file as listed by dired.  Effect of motion is to
go to previous."
  (interactive)
  (if motion
      (unless (ignore-errors (or (scroll-down) t))
        (cic:next-file-dired motion))
    (unless (ignore-errors (or (scroll-up) t))
      (cic:next-file-dired motion))))

(defun cic:search-word-other-window ()
  "Search in the other window for the word at point.
TODO: not currently used but could be with a bit of tweaking."
  (interactive)
  (let (point-word)
    (setq point-word (thing-at-point 'word))
  (save-selected-window
    (other-window 1)
    (goto-char (point-min))
    (search-forward point-word))))

;; http://www.emacswiki.org/emacs/BufferLocalKeys
(defun cic:buffer-local-set-key (key func)
  "Set a buffer local key."
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixup commands
(defun cic:fix-whitespace (buffer)
  "Automate the command sequence M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'unix 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell commands

; http://emacswiki.org/emacs/ShellMode#toc11
; TODO create key for this
(defun cic:clear-shell ()
  "Clear the current shell."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun cic:compilation-shell ()
  "Make current shell a compilation shell.
TODO: toggle the compilation shell."
  (interactive)
  (shell)
  (cic:compilation-shell-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance commands
;; http://www.emacswiki.org/emacs/FullScreen
(defun cic:x-force-maximized (&optional f)
  "Force maximized.  Requires wmctrl to be installed.
TODO: Often called from .emacs so should handle errors well."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -badd,maximized_vert,maximized_horz"))

(defun cic:toggle-fullscreen ()
  "Toggle fullscreen.  Requires wmctrl to be installed.
TODO: Often called from .emacs so should handle errors well."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun cic:toggle-menubar ()
  "Toggle the menubar."
  (interactive)
  (if menu-bar-mode
      (menu-bar-mode -1)
    (menu-bar-mode t)))

(defun cic:whack-whitespace (arg)
  "Delete all white space from point to the next word.  With
    prefix ARG delete across newlines as well.  The only danger
    in this is that you don't have to actually be at the end of a
    word to make it work.  It skips over to the next whitespace
    and then whacks it all to the next word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

(defun cic:ansi-term-screen ()
  "Run the screen command in an ansi-term."
  (interactive)
  (ansi-term "/usr/bin/screen"))

;; TODO: not sure if this works at all.
;;       do I want to use list item or my shell command?
(defun cic:ansi-term-screen-select ()
  "Select screen sessions in an ansi term."
  (interactive)
  ;; get list of running screen sessions
  (let ((ansi-term-screen-sessions (remove "" (split-string (shell-command-to-string "ls /var/run/screen/S-$(whoami)") "\n")))
        (count 0)
        session
        temp-filename)
    (if ansi-term-screen-sessions
        (progn
          (setq cic:ansi-term-screen-sessions (mapcar (lambda (e)
                                                    (when (string-match "\\([0-9]+\\)\\..*" e)
                                                      (match-string 1 e))) cic:ansi-term-screen-sessions))
          ;; select the screen session
          (setq session (cic:select-list-item cic:ansi-term-screen-sessions))
          ;; TODO: there's probably a more secure way to do this than
          ;; random shell commands in temp files
          (setq temp-filename (make-temp-file "emacs-command-"))
          (with-current-file-min temp-filename
            (insert "#!/bin/bash\n")
            (insert (concat "/usr/bin/screen -R " session "\n"))
            (save-buffer))
          (shell-command (concat "chmod +x " temp-filename))
          (ansi-term temp-filename))
      (progn
        (message "No screen sessions found!!!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggle

(defmacro cic:toggle-variable (variable &optional message-t message-nil)
  "Toggle the value of VARIABLE from t to nil and back.
MESSAGE-T gives the message when switching to t. MESSAGE-NIL
gives the message when switching to nil."
  `(lambda ()
     (interactive)
     (if ,variable
         (progn
           (setq ,variable nil)
           (when ,message-nil
             (message ,message-nil)))
       (progn
         (setq ,variable t)
         (when ,message-t
           (message ,message-t))))))

(defun cic:capture-time-string ()
  "Read a time string.
TODO: eventually make more sophisticated"
  ;; remove sometime
  (interactive)
  ;; TODO create the appropriate prompt
  (read-string "Enter 24 hour time HHMM: "))

(defun cic:capture-time-date-string ()
  "Read a time and date string.
TODO: eventually make more sophisticated"
  (interactive)
  ;; get the date first
  (concat (org-read-date) " " (cic:capture-time-string)))

(defun cic:upward-tag-table ()
  "Look upwards in the directory structure for a TAGS file and
visit it."
  (interactive)
  (let ((my-tags-file (cic:find-file-upwards "TAGS")))
    (when my-tags-file
      (setq tags-file-name "")
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))

(defun cic:ansi-term-localhost (&optional command)
  "Start up an ansi-term on localhost."
  (interactive)
  (if command
      (ansi-term (concat "-i -c \"" command ";bash\""))
    (ansi-term "/bin/bash" "localhost")))

(defun cic:ansi-term-localhost-popup (&optional command)
  "Start up an ansi-term on localhost."
  (interactive)
  ;; open new terminal here if last command not popup or window does not exist
  (cond ((or
          (equal (current-buffer) (get-buffer "*localhost*"))
          (and (eq last-command 'cic:ansi-term-localhost-popup) (get-buffer-window "*localhost*")))
         (delete-window (get-buffer-window "*localhost*")))
        ((get-buffer-window "*localhost*")
         ;; just change directories
         (let ((thedir default-directory))
           (with-current-buffer (get-buffer "*localhost*")
             (unless (equal thedir default-directory)
               (term-send-string "*localhost*" (concat "pushd . >/dev/null;cd " thedir "\n")))))
         (select-window (get-buffer-window "*localhost*")))
        ((get-buffer "*localhost*")
         (split-window-below)
         (windmove-down)
         ;; just change directories
         (let ((thedir default-directory))
           (with-current-buffer (get-buffer "*localhost*")
             (unless (equal thedir default-directory)
               (term-send-string "*localhost*" (concat "pushd . >/dev/null;cd " thedir "\n")))))
         (switch-to-buffer (get-buffer "*localhost*")))
        ;; open window
        (t
         (split-window-below)
         (windmove-down)
         (if command
             (ansi-term (concat "-i -c \"" command ";bash\""))
           (ansi-term "/bin/bash" "localhost")))))

;; (convert-whole-numbers-to-decimal "27/89 ")
;; (convert-whole-numbers-to-decimal "27/89")
;; (convert-whole-numbers-to-decimal "7/89 ")
;; (convert-whole-numbers-to-decimal "a7/89 ")
;; (convert-whole-numbers-to-decimal "a7/89b ")
(defun convert-whole-numbers-to-decimal (thestring)
  ;; find all numbers no adjacent to letters that are not decimals
  (replace-regexp-in-string "\\(^\\|[^\.A-Za-z]\\)\\([0-9]\+\\)\\($\\|[^\.A-Za-z0-9]\\)" "\\2\." thestring nil nil 2))

(defun convert-newlines-to-spaces (thestring)
  (replace-regexp-in-string "\n" " " (s-trim-full thestring)))

;; TODO: combine these terminal/paste commands into common function
;; TODO: save output in case of emacs crash and/or run in screen
(defun cic:ansi-term-ipython (&optional use-clipboard-text)
  "Start up an ipython buffer on localhost."
  (interactive)
  (let (region-text)
    (cond (use-clipboard-text
           (setq region-text (x-get-selection)))
          ((region-active-p)
           (setq region-text (buffer-substring (region-beginning) (region-end)))))
    (cond ((get-buffer "*ipython*")
           ;; TODO: double get buffer call is inefficient
           (pop-to-buffer (get-buffer "*ipython*"))
           (when region-text
             ;; TODO: select better
             (term-line-mode)
             (insert (concat (convert-whole-numbers-to-decimal (convert-newlines-to-spaces region-text)) " "))
             (term-char-mode)))
          (t
           ;; create new buffer if does not exist
           (ansi-term "ipython" "ipython")
           ;; how to wait for startup
           ))))

;; TODO: save output in case of emacs crash and/or run in screen
(defun cic:ansi-term-sage (&optional command)
  "Start up an sage buffer on localhost."
  (interactive)
  (let (region-text)
    (when (region-active-p)
      (setq region-text (buffer-substring (region-beginning) (region-end))))
    (cond ((get-buffer "*sage*")
           ;; TODO: double get buffer call is inefficient
           (pop-to-buffer (get-buffer "*sage*"))
           (when region-text
             ;; TODO: select better
             (term-line-mode)
             (insert (convert-newlines-to-spaces region-text))
             (term-char-mode)))
          (t
           ;; create new buffer if does not exist
           (ansi-term "sage" "sage")
           ;; how to wait for startup
           ))))

;; TODO: sql too

;; http://oremacs.com/2015/01/01/three-ansi-term-tips/
;; TODO: maybe burrying an old term buffer might be better?
(defun cic:term-exec-hook ()
  "Kill term buffers when exiting."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'cic:term-exec-hook)

(defun cic:goto-previous-mark ()
  "Jump to the previous mark."
  (interactive)
  (set-mark-command '(4)))

;; http://hewner.com/2012/11/19/changing-the-emacs-modeline-color-in-a-buffer/
;; TODO: need to generalize this a little
(defun cic:term-toggle-modes ()
  "Change the color of the modeline of a terminal based on
whether it is active or inactive.  And whether it is in line or
character mode, indicating whether it can be a normal buffer or
not."
  (interactive)
  (when (eq major-mode 'term-mode)
    (if (term-in-char-mode)
        (progn
          (term-line-mode)
          ;; set the modeline background color and save a "cookie" so the change can be undone
          (face-remap-remove-relative old-term-color)
          (face-remap-remove-relative old-term-color-inactive))
      (progn
        (term-char-mode)
        ;; undo that change later
        ;; like this because green/red means program is in non-standard mode
        (setq-local old-term-color          (face-remap-add-relative 'mode-line          :background "green"))
        (setq-local old-term-color-inactive (face-remap-add-relative 'mode-line-inactive :background "red"))))))
(defun cic:term-mode-setup ()
  (setq-local old-term-color          (face-remap-add-relative 'mode-line          :background "green"))
  (setq-local old-term-color-inactive (face-remap-add-relative 'mode-line-inactive :background "red"))
  (setq-local term-buffer-maximum-size 0))
(add-hook 'term-mode-hook 'cic:term-mode-setup)

;; https://jcubic.wordpress.com/2012/01/26/switching-between-buffers-with-the-same-major-mode-in-emacs/
(defun cic:buffer-same-mode (change-buffer-fun)
  "Helper function to go to buffers of the same mode.
CHANGE-BUFFER-FUN gets repeatedly called until another buffer of
the same mode if found."
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (funcall change-buffer-fun)
      (setq next-mode major-mode))))

(defun cic:previous-buffer-same-mode ()
  "Go to the previous buffer of the same mode."
  (interactive)
  (cic:buffer-same-mode #'previous-buffer))

(defun cic:next-buffer-same-mode ()
  "Go to the next buffer of the same mode."
  (interactive)
  (cic:buffer-same-mode #'next-buffer))

(defun cic:py-align-matrix ()
  "Align a comma-delimitted matrix (within a region) in Python or
similar languages."
  (interactive)
  (replace-regexp ",\\s-*" "," nil (region-beginning) (region-end))
  (align-regexp (region-beginning) (region-end) "\\(,\\)" 1 1 t))

(defun cic:sort-symbols (reverse beg end)
  "Sort a set of symbols in a region."
  ;; https://emacs.stackexchange.com/questions/7548/sorting-words-with-hyphens-e-g-in-a-lisp-mode
  (interactive "*P\nr")
  (let ((temp-table (copy-syntax-table text-mode-syntax-table)))
    (with-syntax-table temp-table
      (modify-syntax-entry ?. "_" temp-table)
      (sort-regexp-fields reverse "\\_<.*?\\_>" "\\&" beg end))))

(defun cic:copy-file-name-to-clipboard (&optional arg)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (let (filename)
    (if arg
        (setq filename (buffer-file-name))
      (setq filename (if (equal major-mode 'dired-mode)
                         (expand-file-name default-directory)
                       (file-name-nondirectory (buffer-file-name))))
      )
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun cic:create-password-insert (&optional arg select)
  "When ARG is given, select a random password type and insert
into current buffer.  Without ARG, defaults to 24 character
alphanumeric."
  (interactive "P")
  (let ((select-list (list
                      (list '18anl      "18 character alphanumeric lowercase")
                      (list '18anlsuff  "18 character alphanumeric lowercase with 2 character suffix.")
                      (list '24l        "24 character alphabet lowercase")
                      (list '24anl      "24 character alphanumeric lowercase")
                      (list '18anp      "18 character alphanumeric with punctuation")
                      (list '12anp      "12 character alphanumeric with punctuation")
                      (list '8anp       "8 character alphanumeric with punctuation")))
        selected)
    (cond ((eq select t)
           (setq selected (cic:select-list-item select-list 'cadr))
           (setq selected (car (elt select-list selected))))
          (arg
           (setq selected '18anlsuff))
          (t
           (setq selected '18anl)))
    (cond ((eq selected '8anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 8)))
          ((eq selected '12anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 12)))
          ((eq selected '18anlsuff)
           (insert (concat (cic:create-password cic:password-characters-alphanum-lower 18) "Q.")))
          ((eq selected '18anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 18)))
          ((eq selected '24anl)
           (insert (cic:create-password cic:password-characters-alphanum-lower 24)))
          ((eq selected '24l)
           (insert (cic:create-password cic:password-characters-alpha-lower 24)))
          (t
           (insert (cic:create-password cic:password-characters-alphanum-lower 18))))))

(defun cic:create-password-insert-select ()
  "Select a random password type and insert into current buffer."
  (interactive)
  (cic:create-password-insert nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs development
(defun cic:recalculate ()
  (interactive)
  (org-table-recalculate '(16)))
(defun cic:elisp-eval-buffer ()
  (interactive)
  ;; TODO make sure we only evaluate elisp mode or ??
  (eval-buffer) (message "Evaluated buffer."))
(defun cic:elisp-eval-call-defun ()
  (interactive)
  (let ((d (eval-defun nil)))
    (funcall d)))
(defun cic:elisp-pp-capture-buffer ()
  (interactive)
  (switch-to-buffer "*PPCapture*"))
(defun cic:elisp-messages-buffer ()
  (switch-to-buffer "*Messages*"))
(defun cic:elisp-debug-on-error ()
  (interactive)
  (funcall (cic:toggle-variable debug-on-error
                                "Debug on error enabled."
                                "Debug on error disabled.")))

(defconst cic:emacs-scratch-buffer-string
  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

"
  "I'm attached to the appearance of the default scratch buffer.")

(defun cic:elisp-scratch-buffer ()
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (cic:elisp-recreate-scratch-buffer)))
;; https://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun cic:elisp-recreate-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  ;; TODO: not sure I need this because I auto-mode paredit....
  (lisp-interaction-mode)
  (insert cic:emacs-scratch-buffer-string)
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil))
(defun cic:help-org ()
  (interactive)
  (info "org"))
(defun cic:help-elisp ()
  (interactive)
  (info "elisp"))
(defun cic:external-collecton-buffer ()
  (interactive)
  (switch-to-buffer "*Collection*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cic:previous-file-dired ()
  (interactive)
  (cic:next-file-dired -1))
(defun cic:previous-file-dired-pageup ()
  (interactive)
  (cic:next-file-dired-pagedown -1))
(defun cic:org-end-of-prev-heading ()
  (interactive)
  (cic:org-end-of-next-heading -1))
(defun cic:next-window-frame ()
  (interactive)
  (condition-case error-string
      (progn
        (windmove-right))
    (error
     (other-frame 1))))
(defun cic:prev-window-frame ()
  (interactive)
  (condition-case error-string
      (progn
        (windmove-left))
    (error
     (other-frame -1))))
(defun cic:text-scale-neutral ()
  (interactive)
  (text-scale-adjust 0))

(defconst cic:prog-modes
  ;; TODO: expand to more likely modes
  (list 'c-mode 'emacs-lisp-mode 'java-mode 'js-mode 'python-mode 'scheme-mode 'sh-mode))

(defconst cic:user-wordlist
  "~/.words")

;; TODO: separate programming/code and text wordlists
(unless (boundp 'cic:user-wordlist)
  "The file to keep the word list in."
  (setq cic:user-wordlist ""))

(defun cic:prog-mode-p ()
  "Check if in one of my typical programming modes."
  (member major-mode cic:prog-modes))

(defun cic:text-mode-p ()
  "Check if this is a text mode."
  (not (cic:prog-mode-p)))

;; TODO add to flyspell buffer
;; TODO move out of keys and into somewhere else?
(defun cic:flyspell-word (&optional arg)
  "Spellcheck word and start flyspell-mode.  Prefix disables
flyspell-mode."
  (interactive "P")
  ;; reload word list by killing ispell
  ;; TODO: do not restart if nothing has changed
  ;; (ispell-kill-ispell t)
  ;; detect prog mode first
  ;; TODO: detect if flyspell started
  (cond (arg
         ;; TODO: check if flyspell is started
         (flyspell-mode -1)
         )
        (t
         (cond ;; ((not (eq last-command 'cic:flyspell-here))
          ;;  (ispell-word))
          ((and (not flyspell-mode) (cic:prog-mode-p))
           (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
           (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
           (cic:flyspell-init-prog))
          ((and (not flyspell-mode) (cic:text-mode-p))
           (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
           (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
           (cic:flyspell-init-text)))
         (ispell-word)
         ;; TODO: run async?
         (flyspell-buffer))))

;; TODO: move to somewhere more appropriate
(setq flyspell-issue-message-flag nil)

;; TODO: merge these two
(defun cic:wordlist-current-word ()
  "Add current word to user-defined wordlist."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (with-current-file cic:user-wordlist
        (goto-char (point-max))
        (insert (concat "\n" word "\n"))
        (flush-lines "^\\s-*$" (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer))
      (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
      (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
      (ispell-kill-ispell t)
      (cic:flyspell-word)
      ;; reset word list
      (message (concat "Successfully added " word " to list!")))))

(defun cic:wordlist-current-word-no-flyspell ()
  "Add current word to user-defined wordlist."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (with-current-file cic:user-wordlist
        (goto-char (point-max))
        (insert (concat "\n" word "\n"))
        (flush-lines "^\\s-*$" (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer))
      (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
      (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
      (ispell-kill-ispell t)
      ;; (cic:flyspell-word)
      ;; reset word list
      (message (concat "Successfully added " word " to list!")))))

;; TODO: also insert just time
(defun cic:insert-date-time-stamp (&optional arg)
  (interactive "P")
  (let* ((the-current-time (current-time))
         (the-time-string (cic:select-list-item (list (format-time-string "%a %b %d, %Y %H:%M:%S" the-current-time)
                                                      (format-time-string "%a %b %d, %Y" the-current-time)
                                                      (format-time-string "%Y%m%d%H%M%S" the-current-time)
                                                      (format-time-string "%Y%m%d" the-current-time)
                                                      (format-time-string "%Y%m%dT%H%M%S" the-current-time)
                                                      (concat (format-time-string "%a %b %d, %Y" the-current-time) " HH:MM:SS")
                                                      "DAY MON DD, YYYY XX:XX:XX"
                                                      "DAY MON DD, YYYY"
                                                      (concat (format-time-string "%Y%m%d" the-current-time)  "HHMMSS")
                                                      (concat (format-time-string "%Y%m%dT" the-current-time) "HHMMSS")
                                                      "YYYYMMDDHHMMSS"
                                                      "YYYYMMDDTHHMMSS"
                                                      "YYYYMMDD"))))
    ;; TODO: handle comments
    (when (eq major-mode 'org-mode)
      ;; TODO: insert appropriate heading
      (org-insert-heading))
    (insert the-time-string)))

(defvar cic:insert-current-time-last-bounds
  nil
  "The bounds of the last insert of current time.")

(defvar cic:insert-current-time-last-type
  nil)

;; TODO: decide whether to call timestamp and/or date???
(defun cic:insert-current-time (&optional arg)
  "Insert the current time and date written out.  ARG only
inserts date. Create an org-mode heading with the current time
and date.  Behaviour based on org-insert-heading."
  (interactive "P")
  (let (the-beg
        the-end)
    (cond ((eq last-command 'cic:insert-current-time)
           (delete-region (car cic:insert-current-time-last-bounds) (cadr cic:insert-current-time-last-bounds))
           (goto-char (car cic:insert-current-time-last-bounds))
           (setq the-beg (point))
           (cond ((equal cic:insert-current-time-last-type 0)
                  (insert (format-time-string "%a %b %d, %Y"))
                  (setq cic:insert-current-time-last-type 1))
                 ((equal cic:insert-current-time-last-type 1)
                  (insert (format-time-string "%Y%m%d%H%M%S"))
                  (setq cic:insert-current-time-last-type 2))
                 ((equal cic:insert-current-time-last-type 2)
                  (insert (format-time-string "%Y%m%dT%H%M%S"))
                  (setq cic:insert-current-time-last-type 3))
                 ((equal cic:insert-current-time-last-type 3)
                  (insert (format-time-string "%a %b %d, %Y %H:%M:%S"))
                  (setq cic:insert-current-time-last-type 0)))
           (setq the-end (point))
           (setq cic:insert-current-time-last-bounds (list the-beg the-end)))
          (t
           (when (eq major-mode 'org-mode)
             (org-insert-heading))
           (setq the-beg (point))
           (insert (format-time-string "%a %b %d, %Y %H:%M:%S"))
           (setq the-end (point))
           (setq cic:insert-current-time-last-bounds (list the-beg the-end))
           (setq cic:insert-current-time-last-type 0)))))

;; TODO: decide whether to call timestamp and/or date???
(defun cic:insert-current-timestamp (&optional arg)
  "Insert the current time and date written out.  ARG only
inserts date. Create an org-mode heading with the current time
and date.  Behaviour based on org-insert-heading."
  (interactive "P")
  ;; TODO: decide if I want this
  (when (eq major-mode 'org-mode)
    (org-insert-heading))
  (let ((time (current-time)))
    (if arg
        (insert (format-time-string "%Y%m%d" time))
      (insert (format-time-string "%Y%m%d%H%M%S" time)))))

;; https://stackoverflow.com/questions/5346107/emacs-case-sensitive-replace-string
(defun cic:query-replace-case-sensitive ()
  (interactive "")
  (let ((case-fold-search nil))
    (call-interactively 'query-replace)))

;; remember frame of last deleted window too
(defvar cic:delete-window-undo
  nil)

;; TODO: do I want to do this stuff for C-x 0 too?
(defun cic:delete-window (&optional arg)
  "Like delete-window but has special functionality for me."
  (interactive "P")
  ;; TODO: do I want to
  (if arg
      (and cic:delete-window-undo (set-window-configuration cic:delete-window-undo))
    (progn
      (when (equal "*Collection*" (buffer-name (current-buffer)))
        ;; put into x11 buffer
        ;; https://emacs.stackexchange.com/questions/14333/how-to-push-kill-ring-contents-onto-system-pasteboard-clipboard
        ;; TODO: expunge extra-whitespace before doing this
        (clipboard-kill-ring-save (point-min) (point-max))
        ;; TODO: this could be undone too
        (erase-buffer))
      ;; TODO: save frame too
      (setq cic:delete-window-undo (current-window-configuration))
      (delete-window))))

(defun cic:delete-window-below (&optional arg)
  "Like delete-window but has special functionality for me."
  (interactive "P")
  ;; is there a window below
  (if arg
      (and cic:delete-window-undo (set-window-configuration cic:delete-window-undo))
    (unless (= (length (window-list)) 1)
      (ignore-errors
        (setq cic:delete-window-undo (current-window-configuration))
        (progn
          (windmove-down)
          (delete-window))))))

;; TODO: undo delete frame
;;       see current-frame-configuration
(defun cic:delete-frame ()
  "Like delete-frame but has special functionality for me."
  (interactive)
  (when (equal "*Collection*" (buffer-name (current-buffer)))
    ;; put into x11 buffer
    ;; https://emacs.stackexchange.com/questions/14333/how-to-push-kill-ring-contents-onto-system-pasteboard-clipboard
    ;; TODO: expunge extra-whitespace before doing this
    (clipboard-kill-ring-save (point-min) (point-max))
    (erase-buffer))
  ;; XXXX: delete-frame interferes with clipboard
  ;;       probably a bug in emacs
  (let ((clipboard-contents (x-get-clipboard)))
    (delete-frame)
    (let ((x-select-enable-clipboard t))
      (x-select-text clipboard-contents))))

;; TODO: preserve outline, but also reset
;; TODO: do better to get 1,2,3
(defun cic:outline (&optional arg)
  "Show a mode-specific outline."
  (interactive "P")
  (cond ((eq major-mode 'latex-mode)
         (if (get-buffer-window "*toc*" t)
             (save-excursion
               (select-window (get-buffer-window "*toc*" t))
               (if arg
                   (reftex-toc-max-level 3)
                 (reftex-toc-max-level 2))
               (delete-window))
           (progn
             (reftex-toc)
             ;; make something brief and easy for navigation
             (setq reftex-toc-include-labels nil)
             (if arg
                 (reftex-toc-max-level 3)
               (reftex-toc-max-level 2))
             (reftex-toc-rescan))))
        ((eq major-mode 'reftex-toc-mode)
         (delete-window))
        (t
         (call-interactively 'imenu))))

(defvar cic:kill-transient-windows-undo
  nil)

(defun cic:kill-transient-windows (&optional arg)
  "Kill all transient windows."
  (interactive "P")
  (if arg
      (and cic:kill-transient-windows-undo (set-window-configuration cic:kill-transient-windows-undo))
    ;; loop over window
    (let ((undo-info (current-window-configuration))
          window-deleted)
      (dolist (window (window-list))
        ;; get window name
        (let ((buffer-name (buffer-name (window-buffer window))))
          (when (and (starts-with buffer-name "*")
                     (ends-with   buffer-name "*")
                     (not (string-match "scratch" buffer-name)))
            (delete-window window)
            (setq window-deleted t))
          (when (starts-with buffer-name "*Python check:")
            (kill-buffer buffer-name))))
      (when window-deleted
        (setq cic:kill-transient-windows-undo undo-info)))))

(defun cic:what-cursor-position ()
  "Give a lot of really cool info on cursor position."
  (what-cursor-position t))

;; TODO: maybe use for m-c
;;       or use c-w for this and change m-x
(defun kill-ring-save-whole-word-or-region ()
  "Save a whole word to kill ring, useful for things like searching."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    ;; check if at beginning of word
    (let ((previous-character-space (save-excursion
                                      (backward-char)
                                      ;; puncuation too
                                      (looking-at-p " "))))
      (unless previous-character-space
        (backward-word))
      (mark-word)
      (kill-ring-save (region-beginning) (region-end)))))

(defvar cic:org-meta-level
  "What level when org-meta"
  nil)

(defvar cic:org-meta-moving-up
  "Indicates direction that cycling moves things."
  t)

(defun cic:org-meta-content-cycle ()
  "Better org-meta to insert item, with cycling type."
  (interactive)
  (cond ((not (eq last-command 'cic:org-meta-content-cycle))
         ;; insert content normally
         (end-of-line)
         (org-meta-return)
         ;; record headline level meta-return inserted
         (setq cic:org-meta-level (org-current-level))
         ;; record parent headline level
         (setq cic:org-meta-moving-up t))
        ((cic:org-list-p (cic:get-current-line))
         (org-toggle-heading cic:org-meta-level)
         (setq cic:org-meta-moving-up t))
        ;; promote if equal
        ((and (org-on-heading-p) (= (org-outline-level) cic:org-meta-level))
         (if cic:org-meta-moving-up
             (org-promote)
           (progn
             (org-toggle-item nil))))
        ;; demote if unequal
        ((and (org-on-heading-p) (< (org-outline-level) cic:org-meta-level))
         (org-demote)
         (setq cic:org-meta-moving-up nil)))
  (end-of-line))

;; TODO: move keys somewhere better
(define-key org-mode-map (kbd "H-j") 'cic:org-meta-content-cycle)
;; TODO: make better once I decide? show all children
;; TODO: functions too
;; don't use this?
(define-key org-mode-map (kbd "H-s") 'cic:org-cycle-in-level-1-tree)

(defun cic:org-cycle-in-level-1-tree ()
  "Cycle open everything in current level 1 subtree."
  (interactive)
  (save-excursion
    (ignore-errors (outline-up-heading 5))
    (org-show-subtree)))
;; go to end of last heading

(define-key org-mode-map (kbd "H-o") 'cic:org-open-last-tree)

(defun cic:org-open-last-tree ()
  "Open at end of last tree, then cycle between beginning and end of it."
  (interactive)
  (if (and (eq last-command 'cic:org-open-last-tree) (not (org-at-heading-p)))
      ;; assume already opened for now
      (progn
        (ignore-errors (outline-up-heading 5)))
    (progn
      (goto-char (point-max))
      (org-back-to-heading)
      ;; make sure we get to proper
      ;; heading TODO: do not like
      ;; ignore-errors
      (ignore-errors (outline-up-heading 5))
      (org-show-subtree)
      (goto-char (point-max)))))

(defun cic:cycle-with-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun cic:window-kill-above ()
  (interactive)
  (let ((window (windmove-find-other-window 'up)))
    (when window
      (delete-window window))))

(defun cic:window-kill-below ()
  (interactive)
  (let ((window (windmove-find-other-window 'down)))
    (when window
      (delete-window window))))

(defun cic:split-window-above ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun cic:comment-header ()
  (interactive)
  (unless (save-excursion
          (beginning-of-line)
          (looking-at "[[:space:]]*$"))
    (end-of-line)
    (insert "\n"))
  (beginning-of-line)
  (insert (s-repeat 80 (s-trim-full comment-start))))

(provide 'emacs-stdlib-commands)
