;;; emacs-stdlib-commands.el --- Standard emacs commands that should
;;; have many uses.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20150327
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
(defun apk:org-end-of-next-heading (&optional arg)
  "Go to the end of the next heading.  ARG doesn't do anything
right now.
TODO: incomplete but still useful right now"
  (interactive "P")
  ; somehow deal with level 2 vs level 3
  (let (current-heading-level
        heading-empty)
    (save-excursion
      (unless (org-headline-p (get-current-line))
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
      (let ((current-line (get-current-line)))
        (hide-subtree)
        (org-cycle)
        (outline-end-of-subtree)
        (when (string= current-line (get-current-line))
          (setq heading-empty t))))
    (unless heading-empty
      (hide-subtree)
      (org-cycle)
      (outline-end-of-subtree)
      (org-back-to-heading)
      (beginning-of-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse commands

(defun apk:browse-url-at-point-conkeror ()
  "Find the URL at point and browse in the conkeror web browser."
  (interactive)
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic (apk:url-at-point-or-line 'url))))

(defun apk:browse-url-at-point-firefox ()
  "Find the URL at point and browse in the Firefox web browser."
  (interactive)
  (browse-url-firefox (apk:url-at-point-or-line 'url)))

(defun apk:browse-url-at-point-w3m ()
  "Find the URL at point and browse in the w3m web browser."
  (interactive)
  (browse-url-w3 (apk:url-at-point-or-line 'url)))

(defun apk:url-at-point-or-line (&optional current-line)
  "Find the URL at point and browse in the w3m web browser.  Find
the url in CURRENT-LINE if specified."
  (let ((url (thing-at-point 'url)))
    (unless url
      (unless current-line
        (setq current-line (get-current-line)))
      (let ((url-start (string-match apk:emacs-stdlib-url-regexp current-line)))
        (when url-start
          (setq url (substring current-line url-start (match-end 0))))))
    url))

(defun apk:org-heading-timestamp (&optional arg)
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

(defun apk:org-insert-two-level (&optional arg)
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
(defun apk:next-file-dired (&optional motion)
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

(defun apk:search-word-other-window ()
  "Search in the other window for the word at point.
TODO: not currently used but could be with a bit of tweaking."
  (interactive)
  (let (point-word)
    (setq point-word (thing-at-point 'word))
  (save-selected-window
    (other-window 1)
    (goto-char (point-min))
    (search-forward point-word))))

; http://www.emacswiki.org/emacs/BufferLocalKeys
(defun apk:buffer-local-set-key (key func)
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
(defun apk:fix-whitespace (buffer)
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
(defun apk:clear-shell ()
  "Clear the current shell."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun apk:compilation-shell ()
  "Make current shell a compilation shell.
TODO: toggle the compilation shell."
  (interactive)
  (shell)
  (apk:compilation-shell-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance commands
;; http://www.emacswiki.org/emacs/FullScreen
(defun apk:x-force-maximized (&optional f)
  "Force maximized.  Requires wmctrl to be installed.
TODO: Often called from .emacs so should handle errors well."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -badd,maximized_vert,maximized_horz"))

(defun apk:toggle-fullscreen ()
  "Toggle fullscreen.  Requires wmctrl to be installed.
TODO: Often called from .emacs so should handle errors well."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun apk:toggle-menubar ()
  "Toggle the menubar."
  (interactive)
  (if menu-bar-mode
      (menu-bar-mode -1)
    (menu-bar-mode t)))

(defun apk:whack-whitespace (arg)
  "Delete all white space from point to the next word.  With
    prefix ARG delete across newlines as well.  The only danger
    in this is that you don't have to actually be at the end of a
    word to make it work.  It skips over to the next whitespace
    and then whacks it all to the next word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

(defun apk:ansi-term-screen ()
  "Run the screen command in an ansi-term."
  (interactive)
  (ansi-term "/usr/bin/screen"))

(defun apk:ansi-term-screen-select ()
  "Select screen sessions in an ansi term.
TODO: not sure if this works well"
  (interactive)
  ;; get list of running screen sessions
  (let ((ansi-term-screen-sessions (remove "" (split-string (shell-command-to-string "ls /var/run/screen/S-$(whoami)") "\n")))
        (count 0)
        session
        temp-filename)
    (if ansi-term-screen-sessions
        (progn
          (setq apk:ansi-term-screen-sessions (mapcar (lambda (e)
                                                    (when (string-match "\\([0-9]+\\)\\..*" e)
                                                      (match-string 1 e))) apk:ansi-term-screen-sessions))
          ;; select the screen session
          (setq session (select-list-item apk:ansi-term-screen-sessions))
          ;; TODO: there's probably a more secure way to do this than
          ;; random shell commands in temp files
          (setq temp-filename (make-temp-file "emacs-command-"))
          (with-current-file temp-filename
            (insert "#!/bin/bash\n")
            (insert (concat "/usr/bin/screen -R " session "\n"))
            (save-buffer))
          (shell-command (concat "chmod +x " temp-filename))
          (ansi-term temp-filename))
      (progn
        (message "No screen sessions found!!!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggle

(defmacro apk:toggle-variable (variable &optional message-t message-nil)
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

(defun apk:capture-time-string ()
  "Read a time string.
TODO: eventually make more sophisticated"
  ;; remove sometime
  (interactive)
  ;; TODO create the appropriate prompt
  (read-string "Enter 24 hour time HHMM: "))

(defun apk:capture-time-date-string ()
  "Read a time and date string.
TODO: eventually make more sophisticated"
  (interactive)
  ;; get the date first
  (concat (org-read-date) " " (apk:capture-time-string)))

(defun apk:upward-tag-table ()
  "Look upwards in the directory structure for a TAGS file and
visit it."
  (interactive)
  (let ((my-tags-file (find-file-upwards "TAGS")))
    (when my-tags-file
      ;; TODO makunbound doesn't work here, symbolp error?
      (setq tags-file-name "")
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))

(provide 'emacs-stdlib-commands)
