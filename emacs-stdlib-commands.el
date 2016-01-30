;;; emacs-stdlib-commands.el --- Standard emacs commands that should
;;; have many uses.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20160130
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

(defvar cic:org-mark-toggle-headline-hook
  nil
  "Add functions to be run with cic:org-mark-toggle-headline.")

(defun cic:org-at-todo-p ()
  (let (matched
        ;; TODO: this should be changed to read org-todo-keywords
        (todo-keyword-strings '("TODO" "NEXT" "WAITING" "DONE" "INVALID")))
    (dolist (tks todo-keyword-strings)
      (when (string-match tks (cic:get-current-line))
        (setq matched t)))
    matched))

(defun cic:org-mark-toggle-headline (arg)
  "Change to DONE or TODO, + or -, or another custom state.

Changes a tree of bullet points to have + bullet marker or to -
or TODO with prefix ARG.  These can be highlighted a different
color to easily indicate doneness.  Also has a hook for custom
types of headings.  Does nothing if already in desired state."
  (interactive "P")
  (let (return-value
        return-true)
    ;; check the cic:org-mark-toggle-headline-hook first
    (dolist (heading-hook cic:org-mark-toggle-headline-hook)
      ;; make sure only one is run
      (unless return-true
        (setq return-value (funcall heading-hook))
        (when return-value
          (setq return-true t))))
    ;; TODO: do the org-mode thing if other values are not done?
    (unless return-true
      (cond ((eq major-mode 'org-agenda-mode)
             (when (cic:org-at-todo-p)
               (org-agenda-todo)))
            ((and (org-at-heading-p) (cic:org-at-todo-p))
             (if arg
                 (org-todo 'todo)
               (org-todo 'done)))
            ((org-at-item-p)
             (save-excursion
               (beginning-of-line)
               (let* ((current-line (cic:get-current-line))
                      (current-indentation (count-indentation current-line))
                      replace-regexp
                      regplace-item)
                 (when (and (not arg) (string-match "\\s-*- " current-line))
                   (setq replace-regexp "^\\s-*\\(-\\) .*$")
                   (setq replace-item "+"))
                 (when (and arg (string-match "\\s-*\\+ " current-line))
                   (setq replace-regexp "^\\s-*\\(\\+\\) .*$")
                   (setq replace-item "-"))
                 (when replace-regexp
                   ;; replace
                   (beginning-of-line)
                   (when (re-search-forward replace-regexp nil t)
                     (replace-match replace-item nil nil nil 1))
                   (forward-line)
                   (while (> (count-indentation (cic:get-current-line)) current-indentation)
                     ;; replace again
                     (beginning-of-line)
                     (when (re-search-forward replace-regexp nil t)
                       (replace-match replace-item nil nil nil 1))
                     (forward-line))))))
            (t
             (error "Not at an item or valid TODO!"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse commands

(defun cic:browse-url-at-point-conkeror ()
  "Find the URL at point and browse in the conkeror web browser."
  (interactive)
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic (cic:url-at-point-or-line 'url))))

(defun cic:browse-url-at-point-firefox ()
  "Find the URL at point and browse in the Firefox web browser."
  (interactive)
  (browse-url-firefox (cic:url-at-point-or-line 'url)))

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
  (let ((url (thing-at-point 'url)))
    (unless url
      (unless current-line
        (setq current-line (cic:get-current-line)))
      (let ((url-start (string-match cic:emacs-stdlib-url-regexp current-line)))
        (when url-start
          (setq url (substring current-line url-start (match-end 0))))))
    url))

(defun cic:org-heading-timestamp (&optional arg)
  "Create an org-mode heading with the current time and date.
ARG has not effect currently.  Behaviour based on
org-insert-heading."
  (interactive "P")
  (org-insert-heading)
  (cond
   ((eq arg '(4))
    (insert (format-time-string "%a %b %d, %Y")))
   (t
    (insert (format-time-string "%a %b %d, %Y")))))

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
Effect of motion is unknown."
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

(defun cic:ansi-term-screen-select ()
  "Select screen sessions in an ansi term.
TODO: not sure if this works at all."
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
        (setq-local old-term-color (face-remap-add-relative 'mode-line :background "green"))
        (setq-local old-term-color-inactive (face-remap-add-relative 'mode-line-inactive :background "red"))))))
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local old-term-color (face-remap-add-relative 'mode-line :background "green"))
            (setq-local old-term-color-inactive (face-remap-add-relative 'mode-line-inactive :background "red"))))

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
                         (file-name-nondirectory default-directory)
                       (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun cic:create-password-insert (&optional arg)
  "When ARG is given, select a random password type and insert
into current buffer.  Without ARG, defaults to 24 character
alphanumeric."
  (interactive "P")
  (let ((select-list (list
                      (list '30l "30 character alphabet with punctuation")
                      (list '24anp "24 character alphanumeric with punctuation")
                      (list '12anp "12 character alphanumeric with punctuation")
                      (list '24an  "24 character alphanumeric")
                      (list '12an "12 character alphanumeric")))
        selected)
    (if arg
        (progn
          (setq selected (cic:select-list-item select-list
                                           'cadr))
          (setq selected (car (elt select-list selected))))
      (setq selected '30l))
    (cond ((eq selected '12an)
           (insert (cic:create-password-12-Alphanum)))
          ((eq selected '12anp)
           (insert (cic:create-password-12-Alphanum-punct)))
          ((eq selected '24an)
           (insert (cic:create-password-24-Alphanum)))
          ((eq selected '24anp)
           (insert (cic:create-password-24-Alphanum-punct)))
          (t
           (insert (cic:create-password-30-alpha-lower))))))

(defun cic:create-password-insert-select ()
  "Select a random password type and insert into current buffer."
  (interactive)
  (cic:create-password-insert t))

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
  (switch-to-buffer "*PPCapture*"))
(defun cic:elisp-messages-buffer ()
  (switch-to-buffer "*Messages*"))
(defun cic:elisp-debug-on-error ()
  (interactive)
  (funcall (cic:toggle-variable debug-on-error
                                "Debug on error enabled."
                                "Debug on error disabled.")))
(defun cic:elisp-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))
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
(defun cic:org-end-of-prev-heading ()
  (interactive)
  (cic:org-end-of-next-heading -1))
(defun cic:prev-frame ()
  (interactive)
  (other-frame -1))
(defun cic:text-scale-neutral ()
  (interactive)
  (text-scale-adjust 0))

(defconst cic:prog-modes
  ;; TODO: expand to more likely modes
  (list 'c-mode 'emacs-lisp-mode 'java-mode 'js-mode 'python-mode 'scheme-mode 'sh-mode))

;; TODO: separate programming and text lists
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
(defun cic:flyspell-here ()
  ""
  (interactive)
  ;; reload word list by killing ispell
  (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
  (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
  (ispell-kill-ispell t)
  ;; detect prog mode first
  (cond ((cic:prog-mode-p)
         (cic:flyspell-init-prog))
        ((cic:text-mode-p)
         (cic:flyspell-init-text))))

;; TODO: move to somewhere more appropriate
(setq flyspell-issue-message-flag nil)

(defun cic:wordlist-current-word ()
  "Add current word to user-defined wordlist."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      ;; TODO unhard-code
      (with-current-file cic:user-wordlist
        (goto-char (point-max))
        (insert (concat "\n" word "\n"))
        (flush-lines "^\\s-*$" (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer))
      (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
      (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
      (ispell-kill-ispell t)
      (cic:flyspell-here)
      ;; reset word list
      (message (concat "Successfully added " word " to list!")))))

(provide 'emacs-stdlib-commands)
