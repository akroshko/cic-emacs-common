;;; cic-emacs-org-mode.el --- Emacs functions written specifically
;;; for org-mode.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Arp 17, 2019
;; Version: 20191209
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
;; These are functions written specifically for org-mode.
;;
;;; Code:

(defun cic:org-find-headline (headline &optional buffer)
  "Find a particular HEADLINE in BUFFER."
  (goto-char (org-find-exact-headline-in-buffer headline)))

(defun cic:org-find-table (&optional count)
  "Find either the first or COUNT table in BUFFER.  Go to the
last row of the able."
  (unless count
    (setq count 1))
  (dotimes (i count)
    (while (not (or (org-at-table-p) (eobp)))
      (forward-line 1))
    (when (< i (- count 1))
      (cic:org-table-last-row)
      (forward-line 2))))

(defun cic:org-table-to-lisp-no-separators ()
  "Convert the org-table at point to Emacs Lisp representation
and eliminate seperators."
  (remove-if-not (lambda (x) (if (eq x 'hline) nil x)) (org-table-to-lisp)))

(defun cic:org-table-last-row ()
  "Goto the last non-seperator row of the next table in the
buffer."
  (let (table-next
        seperator-next
        table-next-next
        (keep-going t))
    (while keep-going
      (save-excursion
        (forward-line 1)
        (setq table-next     (org-at-table-p)
              ;; TODO: make sure seperator next goes into a function of its own
              seperator-next (string-match "|[-+]+\\+" (cic:get-current-line)))
        (forward-line 1)
        (setq table-next-next (and (org-at-table-p) (not (string-match "|[-+]+\\+" (cic:get-current-line))))))
      (if (or
           (and table-next (not seperator-next))
           (and table-next seperator-next table-next-next))
          (forward-line)
        (setq keep-going nil)))))

(defun cic:org-table-lookup-location (filename-list table-name key-value &optional column)
  "Lookup the location (in buffer) of KEY-VALUE in the either the
first column or COLUMN column from TABLE-NAME in the filenames
FILENAME-LIST."
  (unless column
    (setq column 1))
  (setq filename-list (cic:ensure-list filename-list))
  (let (found-list)
    (setq found-list (nreverse (delq nil
                                     (loop for the-filename in filename-list
                                           ;; cons and/or collect at end rather than append
                                           collect (do-org-table-rows the-filename table-name row
                                                                      (org-table-goto-column column)
                                                                      (when (string= key-value (s-trim-full (org-table-get nil column)))
                                                                        (push (list the-filename (point)) found-list)))))))
    (when (> (length found-list) 1)
      (error "Found too many locations!!!"))
    (car found-list)))

(defun cic:org-table-lookup-row (filename table-name key-value &optional column)
  "Lookup and return (in Emacs Lisp format) the row corresponding
to KEY-VALUE in either the first column or COLUMN column from
TABLE-NAME in FILENAME."
  (unless column
    (setq column 1))
  (let (lisp-table
        found-row)
    (with-current-file-transient-org-table filename table-name
                                 (setq lisp-table (cic:org-table-to-lisp-no-separators)))
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (when (string= key-value (s-trim-full (org-table-get nil column)))
                         (setq found-row (cic:org-table-assoc lisp-table key-value column))))
    found-row))

(defun cic:org-table-get-keys (filename table-name &optional column)
  "Get the list of keys in either the first column or COLUMN
column from TABLE-NAME in FILENAME ."
  (unless column
    (setq column 1))
  (let (key-values)
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (setq key-values (cons (s-trim-full-no-properties (org-table-get nil column)) key-values)))
    (remove-if 'cic:full-string-p key-values)
    key-values))

(defun cic:org-table-assoc (lisp-table key-value &optional column equal-test)
  "Get row associated with KEY-VALUE in either the first column
or COLUMN column from LISP-TABLE (in Emacs Lisp with no
seperators format)."
  (unless column
    (setq column 1))
  (cic:assoc-nth column key-value lisp-table equal-test))

;; TODO: sparsely tested
(defun cic:org-insert-indent-list-item ()
  "Insert a list item, open and indent properly."
  (if (cic:org-headline-p (cic:get-current-line))
      (progn
        (move-end-of-line 1)
        (insert "\n")
        (org-cycle)
        (insert "- "))
    (progn
      (move-end-of-line 1)
      (org-meta-return))))

(defun cic:org-insert-new-table-row ()
  "Insert a new row into a table, but only if at a table."
  (when (org-at-table-p)
    (forward-line)
    (beginning-of-line)
    (call-interactively 'org-table-insert-row)
    t))
(add-hook 'org-metareturn-hook 'cic:org-insert-new-table-row)

;; (remove-hook 'org-metareturn-hook 'cic:org-insert-new-list)
(defun cic:org-insert-new-list ()
  "Insert a list item, open and indent properly."
  (when (cic:org-headline-p (cic:get-current-line))
      (move-end-of-line 1)
      (insert "\n")
      (org-cycle)
      (insert "- ")
      t))
(add-hook 'org-metareturn-hook 'cic:org-insert-new-list)

(defun org-code-region ()
  "Add code markup to a selected region in org-mode."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (when (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (when (= (line-number-at-pos start) (line-number-at-pos end))
          (save-excursion
            (goto-char end)
            (insert "~")
            (goto-char start)
            (insert "~")))))))

;; TODO: fix up to remove quotes
(defun org-quote-region ()
  "Make a selected region a quote block in org-mode."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (when (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (end-of-line)
        (insert "\n#+END_QUOTE")
        (goto-char start)
        (beginning-of-line)
        (insert "#+BEGIN_QUOTE\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode C-enter
(defun cic:org-mode-control-return (&optional arg)
  "Alternative function for org-mode C-enter.  Inserts a new
table row if at a table, or an org-mode heading with a datestamp
otherwise.  Repeated usage cycles the format of the datestamp."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (cond ((org-at-table-p)
           (org-table-insert-row '(4)))
          (t
           (cond ((eq real-last-command 'cic:org-mode-control-return)
                  (cond ((string-match "\\* [0-9]\\{8\\}t[0-9]\\{6\\}" (cic:get-current-line))
                         (let (kill-ring
                               kill-ring-yank-pointer
                               (save-interprogram-paste-before-kill nil)
                               (interprogram-cut-function nil))
                           (backward-kill-word 1))
                         (insert (cic:datestamp-current-time-short)))
                        ((string-match "\\* [0-9]\\{8\\}" (cic:get-current-line))
                         (let (kill-ring
                               kill-ring-yank-pointer
                               (save-interprogram-paste-before-kill nil)
                               (interprogram-cut-function nil))
                           (backward-kill-word 1))
                         (insert (cic:datestamp-weekday-time)))
                        ((string-match "\\* [A-Za-z]\\{3\\} [A-Za-z]\\{3\\} [0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" (cic:get-current-line))
                         (let (kill-ring
                               kill-ring-yank-pointer
                               (save-interprogram-paste-before-kill nil)
                               (interprogram-cut-function nil))
                           (search-backward "\* ")
                           (forward-char 2)
                           (kill-line)))
                        (t
                         (insert (cic:datestamp-current-time)))))
                 (t
                  (org-insert-heading-respect-content)
                  (insert (cic:datestamp-current-time))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode commands

;; taken from http://orgmode.org/worg/org-hacks.html#sec-1-3-1
;; TODO: not well used, needs further testing and expansion
(defun cic:org-end-of-next-heading (&optional arg)
  "Go to the end of the next heading.  ARG doesn't do anything
right now."
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
    (outline-hide-subtree)
    (org-forward-heading-same-level 1)
    ;; this can be weird if no subheadings, same line...
    (save-excursion
      (let ((current-line (cic:get-current-line)))
        (outline-hide-subtree)
        (org-cycle)
        (outline-end-of-subtree)
        (when (string= current-line (cic:get-current-line))
          (setq heading-empty t))))
    (unless heading-empty
      (outline-hide-subtree)
      (org-cycle)
      (outline-end-of-subtree)
      (org-back-to-heading)
      (beginning-of-line))))

(defun cic:org-end-of-prev-heading ()
  "Command to jump ot the end of the previous org-mode heading."
  (interactive)
  (cic:org-end-of-next-heading -1))

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
  (cond ((derived-mode-p 'org-agenda-mode)
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
         (setq cic:org-meta-level     (org-current-level)
               ;; record parent headline level
               cic:org-meta-moving-up t))
        ((cic:org-list-p (cic:get-current-line))
         (org-toggle-heading cic:org-meta-level)
         (setq cic:org-meta-moving-up t))
        ;; promote if equal
        ((and (org-at-heading-p) (= (org-outline-level) cic:org-meta-level))
         (if cic:org-meta-moving-up
             (org-promote)
           (org-toggle-item nil)))
        ;; demote if unequal
        ((and (org-at-heading-p) (< (org-outline-level) cic:org-meta-level))
         (org-demote)
         (setq cic:org-meta-moving-up nil)))
  (end-of-line))

(defun cic:org-cycle-in-level-1-tree ()
  "Cycle open everything in current level 1 subtree."
  (interactive)
  (save-excursion
    (ignore-errors (outline-up-heading 5))
    (org-show-subtree)))
;; go to end of last heading

(defun cic:org-open-last-tree ()
  "Open at end of last tree, then cycle between beginning and end of it."
  (interactive)
  (if (and (eq last-command 'cic:org-open-last-tree) (not (org-at-heading-p)))
      ;; assume already opened for now
      (ignore-errors (outline-up-heading 5))
    (progn
      (goto-char (point-max))
      (org-back-to-heading)
      ;; make sure we get to proper
      ;; heading TODO: do not like
      ;; ignore-errors
      (ignore-errors (outline-up-heading 5))
      (org-show-subtree)
      (goto-char (point-max)))))

(provide 'cic-emacs-org-mode)
