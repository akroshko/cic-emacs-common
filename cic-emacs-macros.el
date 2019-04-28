;;; cic-emacs-macros.el --- Useful Emacs Lisp macros.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20190427
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
;; These are some useful Emacs Lisp macros.  They provide added
;; functionality over many of the with- macros built into Emacs.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to do things temporarily

;; XXXX: set filename filter if not already set, used for filenames in
;; special formats that may be declared late
(unless (fboundp 'with-filename-filter)
  (fset 'with-filename-filter 'identity))

;; do I want to do other headlines too, or within a headline
(defmacro do-org-headlines (filename headline-name headline-subtree &rest body)
  "Iterate over headlines in FILENAME. HEADLINE-NAME holds the
headline itself with HEADLINE-SUBTREE containing the subtree that
the HEADLINE represents."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     (let ((keep-going t))
       (while keep-going
         ;; next headline same level
         (if (re-search-forward (concat "^\* .*")  nil t)
             (progn
               (beginning-of-line)
               (let ((current-line (cic:get-current-line)))
                 (setq ,headline-name (when (string-match "^\* \\(.*\\)" current-line)
                                        (match-string 1 current-line)))
                 (setq ,headline-subtree (buffer-substring (point) (save-excursion
                                                                     (org-end-of-subtree)
                                                                     (point)))))
               ,@body
               (end-of-line))
           (setq keep-going nil))))))

;; TODO want to allow files with table-less headlines
(defmacro do-org-tables (filename table-name table &rest body)
  "Iterate over tables in FILENAME. TABLE-NAME holds the table
name itself with TABLE containing a lisp representation of the
table.  While iterating might position cursor in first column and
first row of table.  Use save-excursion while moving in the body
of this macro, just in case."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ;; TODO figure out best layout for file and best way to do this
     (let ((keep-going t))
       (while keep-going
         ;; next headline same level
         (when (string-match cic:headline-regexp (cic:get-current-line))
           (setq ,table-name (match-string 1 (cic:get-current-line))))
         (save-excursion
           (forward-line 1)
           (setq ,table nil)
           (while (not (or (and (cic:org-headline-p (cic:get-current-line)) (= (org-outline-level) 1)) (org-at-table-p) (eobp)))
             (forward-line 1))
           (when (org-at-table-p)
             (setq ,table (cic:org-table-to-lisp-no-separators))))
         (when (and ,table-name ,table)
           ,@body)
         ;; see if we can keep going
         (setq keep-going (cic:org-check-last-heading-level-1))
         ;; if not end
         (when keep-going
           (org-forward-heading-same-level 1 nil))))))

;; TODO: make sure we can deal with seperators
(defmacro do-org-table-rows (filename table-name row &rest body)
  "Iterate over the rows of the table TABLE-NAME in FILENAME.
ROW contains the current row converted into elisp."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ;; TODO replace with org-headline-goto???
     ;; XXXX: complicated regexp deals with tags
     (when (re-search-forward (concat "^\* " ,table-name "\\([[:space:]]:.*\\)?$") nil t)
       (cic:org-find-table)
       (let ((keep-going t)
             (lisp-table (cic:org-table-to-lisp-no-separators))
             (row-count 0))
         (while keep-going
           (unless (string-match "|-+\+.*|" (cic:get-current-line))
             (setq ,row (nth row-count lisp-table))
             ,@body
             (setq row-count (1+ row-count)))
           (save-excursion
             ;; TODO need to catch error or whatever from this
             (forward-line 1)
             (unless (org-at-table-p)
               (setq keep-going nil)))
           (when keep-going
             (forward-line 1)))))))

;; think, like table with table-row, should get item-lines
(defmacro do-org-list-items (filename list-name item-line &rest body)
  "Iterate over the items of list LIST-NAME in FILENAME.
ITEM-LINE contains the line that a particular item is on."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     (when (re-search-forward (concat "^\* " ,list-name "$") nil t)
       (forward-line 1)
       (let ((keep-going t))
         (while keep-going
           (setq ,item-line (cic:get-current-line))
           ,@body
           (save-excursion
             (forward-line 1)
             (when (or (and (cic:org-headline-p (cic:get-current-line)) (= (org-outline-level) 1)) (progn (end-of-line) (eobp)))
               (setq keep-going nil)))
           (when keep-going
             (forward-line 1)))))))

(defmacro with-current-buffer-create (buffer-or-name &rest body)
  "Create BUFFER-OR-NAME if nonexistant and run BODY like
with-current-buffer."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (get-buffer-create ,buffer-or-name))
     ,@body))

(defmacro with-current-buffer-min (buffer-or-name &rest body)
  "Like with-current-buffer but always go to point min."
  `(save-excursion
     (set-buffer ,buffer-or-name)
     (goto-char (point-min))
     ,@body))

(defmacro with-current-buffer-max (buffer-or-name &rest body)
  "Like with-current-buffer but always go to point max."
  `(save-excursion
     (set-buffer ,buffer-or-name)
     (goto-char (point-max))
     ,@body))

(defmacro with-current-buffer-erase (buffer-or-name &rest body)
  "Like with-current-buffer but always erase it."
  `(save-excursion
     (set-buffer ,buffer-or-name)
     (setq buffer-read-only nil)
     (erase-buffer)
     ,@body))

(defmacro with-current-file (filename &rest body)
  "Execute BODY with FILENAME as buffer.  Uses
with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     ,@body))

(defmacro with-current-file-transient (filename &rest body)
  "Execute BODY with FILENAME as buffer.  Close the file if it
does not already exist Uses with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
           (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))
       (set-buffer current-file-buffer)
       (let ((the-return (progn
                           ,@body)))
         (unless already-existing-buffer
           (kill-buffer current-file-buffer))
         the-return))))

(defmacro with-current-file-transient-headline (filename headline &rest body)
  "Execute BODY with FILENAME as buffer after finding HEADLINE.
Uses with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let (((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
            (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))))
     (set-buffer the-buffer)
     (cic:org-find-headline ,headline)
     (let ((the-return (progn
                         ,@body)))
       (unless already-existing-buffer
         (kill-buffer current-file-buffer))
       the-return)))

(defmacro with-current-file-min (filename &rest body)
  "Like with-current-file, but always go to point-min."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ,@body))

(defmacro with-current-file-transient-min (filename &rest body)
  "Like with-current-file, but always go to point-min."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
           (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))
       (set-buffer current-file-buffer)
       (goto-char (point-min))
       (let ((the-return (progn
                           ,@body)))
         (unless already-existing-buffer
           (kill-buffer current-file-buffer))
         the-return))))

(defmacro with-current-file-max (filename &rest body)
  "Like with-current-file, but always go to point max."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-max))
     ,@body))

(defmacro with-current-file-transient-max (filename &rest body)
  "Like with-current-file, but always go to point max."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
           (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))
       (set-buffer current-file-buffer)
       (goto-char (point-max))
       (let ((the-return (progn
                           ,@body)))
         (unless already-existing-buffer
           (kill-buffer current-file-buffer))
         the-return))))

(defmacro with-current-file-transient-org-table (filename table-name &rest body)
  "Like with-current-file, but find TABLE-NAME."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (let ((already-existing-buffer (get-file-buffer (with-filename-filter ,filename)))
           (current-file-buffer (find-file-noselect (with-filename-filter ,filename))))
       (set-buffer current-file-buffer)
       (goto-char (point-min))
       (when (re-search-forward (concat "^\* " ,table-name "$") nil t)
         (cic:org-find-table)
         (let ((the-return (progn
                             ,@body)))
           (unless already-existing-buffer
             (kill-buffer current-file-buffer))
           the-return)))))

(provide 'cic-emacs-macros)
