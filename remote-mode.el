;;; remote-mode.el --- Open urls or files remotely and reuse same
;;; instance of remote program.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20150522
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
;; Remotely and automatically open urls at point in a Conkeror web
;; browser session or remotely open documents at point in dired in an
;; Okular document viewer sessions.  Could easily be modified to work
;; with other software.
;;
;; Features that might be required by this library:
;;
;; Requires pymacs for now, but want to remove that.
;; XXXX: requires okular, conkeror installed, and the provided okular_remote.py command
;; XXXX: An okular-preview command is required on the path
;;       ln -s /usr/bin/okular <<Users bin directory>>/okular-preview
;; XXXX: A conkeror-preview command will soon be required on the path
;;       ln -s /usr/bin/conkeror <<Users bin directory>>/conkeror-preview
;;
;;; Code:

;; TODO: set a global remote-url-mode?  rather than just for a buffer?
;; TODO: needs better error handling of okular mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url remote org minor mode
(define-minor-mode remote-url-mode
  "A minor mode that automatically calls a web browser for the
highlighted link on the current line."
  ;; the initial value
  nil
  ;; keybindings
  ;; TODO also want remote-url-mode-refresh
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-p") 'remote-url-mode-previous-line)
            (define-key map (kbd "C-n") 'remote-url-mode-next-line)
            map)
  :group 'remote)

(define-minor-mode remote-dired-mode
  "A minor mode that automatically opens the file at the current dired line"
  ;; keybindings
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-p") 'remote-dired-mode-previous-line)
            (define-key map (kbd "C-n") 'remote-dired-mode-next-line)
            map)
  :group 'remote)

(defun remote-url-mode-previous-line (&optional arg)
  "Goto the previous line and open up any urls.  ARG is passed to
previous-line."
  (interactive "P")
  (previous-line arg)
  (remote-url-mode-open))

(defun remote-url-mode-next-line (&optional arg)
  "Goto the next line and open up any urls.  ARG is passed to
next-line."
  (interactive "P")
  (next-line arg)
  (remote-url-mode-open))

(defun remote-url-mode-open (&optional line)
  "Remotely open the url on LINE."
  (unless line
    (setq line (get-current-line)))
  (let ((url (remote-match-url-line line)))
    (remote-url-open url)))

(defun remote-dired-mode-previous-line (&optional arg)
  "Goto the previous line and open up any valid filenames.  ARG
is passed to previous-line."
  (interactive "P")
  (previous-line arg)
  (remote-dired-mode-open))

(defun remote-dired-mode-next-line (&optional arg)
  "Goto the next line and open up any valid filenames.  ARG is
passed to previous-line."
  (interactive "P")
  (next-line arg)
  (remote-dired-mode-open))

(defun remote-dired-mode-open ()
  "Remotely open the filename on LINE if valid."
  (remote-dired-open (dired-file-name-at-point)))

(defun remote-url-open (url)
  "Handler for opening URL remotely."
  (start-process "conkeror remote" nil "/usr/bin/conkeror" "-e" (concat "load_url_in_current_buffer('" url "');")))

(defun remote-dired-open (filename)
  "Handler for opening FILENAME remotely"
  ;; TODO: want to avoid pymacs if at all possible
  (when (stringp filename)
    (when (filter-excise 'identity (mapcar (lambda (x) (ends-with filename x)) (list ".chm" ".djvu" ".dvi" ".epub" ".gif" ".jpg" ".jpeg" ".mobi" ".pdf" ".png" ".tiff")))
      (pymacs-exec (concat "okular_remote.main(['',\'" (replace-regexp-in-string "'" "\\\\'" (expand-file-name filename)) "\'])"))))
  ;; (start-process "dired remote" nil "~/bin/okular_remote.py" (replace-regexp-in-string " " "\\\\ " (concat "file://" filename)))
  )

(defun remote-match-url-line (&optional line)
  "Match the specific URL on the LINE."
  (let (matched)
    (unless line
      (setq line (get-current-line)))
    (when (string-match "[^: []*://\\([^?\r\n]+\\).*$" line)
      (setq matched (match-string 0 line)))
    (car (split-string matched "]"))))

(provide 'remote-mode)
