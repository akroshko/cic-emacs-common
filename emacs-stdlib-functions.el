;;; emacs-stdlib-functions.el --- Standard emacs function that should
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
;; These are relatively generic functions, often the extensions of
;; ones in common packages.
;;
;; Features that might be required by this library:
;;
;; Generally only requires a basic Emacs installation.
;; TODO: Want to list requirements eventually, but see
;; emacs-config.el for some potential requires.
;; XXXX: Have not added some extremely common functions to having apk:
;; prefix.  May put these in their own file.
;;
;;; Code:

(defun mpp (value &optional buffer)
  "Pretty print a message to a particular buffer and include a
time-stamp in the message.
XXXX: not adding apk prefix before this function is called so often in adhoc code
TODO: remove timestamp flag
TODO: have a decent default buffer that is not the normal message one"
  (let ((message-string (concat (apk:time-stamp) "\n" (with-output-to-string (princ value)))))
    (if buffer
        (save-excursion (with-current-buffer (get-buffer-create buffer)
                          (goto-char (point-max))
                          (insert (concat message-string "\n"))))
      (message message-string))))

(defun apk:time-stamp ()
  "Create a time-stamp."
  (format-time-string "%H:%M:%S" (current-time)))

(defun strip-full (str)
  "Strip-Full leading and trailing whitespace from STR.  Does
this for every line."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR.  Just beginning and end of lines
TODO: are one of this and strip-full redundant?"
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun strip-full-no-properties (str)
  "Like strip-full but remove text properties."
  (substring-no-properties (strip-full str)))

(defun strip-full-single-spaces (str)
  "Convert STR into one line with no leading or trailing whitespace."
  (while (string-match "[[:space:]]*\n+[[:space:]]*\\|[[:space:]]\\{2,\\}"
                       str)
    (setq str (replace-match " " t t str)))
  (strip-full str))

(defun strip-colons (str)
  "Strip leading and trailing colons off of STR."
  (when (stringp str)
    (while (string-match "^:" str)
      (setq str (replace-match "" t t str)))
    (while (string-match ":$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun strip-square-brackets (str)
  "Strip leading and trailing square brackets off of STR."
  (when (stringp str)
    (while (string-match "^\\[" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "\\]$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun strip-dashes (str)
  "Strip leading and trailing dashes off of STR."
  (when (stringp str)
    (while (string-match "^-" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "-$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun remove-trailing-whitespace (str)
  "Strip trailing whitespace off of STR."
  (when (string-match "[ \t\n]*$" str)
    (concat (replace-match "" nil nil str))))

(defun full-string-p (thing-or-string)
  "Determine if something is nil or an empty string."
  (if (or (not thing-or-string) (equal (strip-full thing-or-string) ""))
      nil
    t))

; taken from stackoverflow
(defun apk:count-occurences (regex string)
  "Count the occurences of REGEX in STRING."
  (apk:recursive-count regex string 0))

(defun apk:recursive-count (regex string start)
  "Recursively count REGEX in STRING from the START character.
TODO: Better docs."
  (if (string-match regex string start)
      (+ 1 (apk:recursive-count regex string (match-end 0)))
    0))

(defun string-to-float (str)
  "Convert STR to float."
  (let (new-var)
    (when (stringp str)
      (setq str (string-to-number str)))
    (unless (floatp str)
      (setq str (float str)))
    str))

(defun apk:flyspell-init-text ()
  "Inititalize flyspell for text modes."
  (flyspell-mode t)
  (flyspell-buffer))

(defun apk:flyspell-init-prog ()
  "Inititalize flyspell from programming modes."
  (flyspell-prog-mode)
  (flyspell-buffer))

(defun apk:make-some-files-read-only ()
  "When files are opened for certain modes, make them read only."
  (when (or (not (string-match (expand-file-name "~") (buffer-file-name)))
            (memq major-mode '(doc-view-mode)))
    (toggle-read-only 1)))

; http://www.emacswiki.org/emacs/ElispCookbook#toc59
(defun apk:walk-path (dir action)
  "Walk DIR executing ACTION with arugments (dir file)"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (apk:walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))

(defun apk:org-find-headline (headline &optional buffer)
  "Find a particular HEADLINE in BUFFER."
  (goto-char (org-find-exact-headline-in-buffer headline)))

(defun apk:org-find-checkbox (name)
  "Find a particular checkbox with text NAME in BUFFER.
TODO: Currently prefix search, do I want an exact search?"
  (search-forward-regexp (concat apk:emacs-stdlib-checkbox-regexp name))
  (move-beginning-of-line 1))

(defun apk:org-find-list-item (name)
  "Find a particular list item with text NAME in BUFFER.
TODO: Currently prefix search, do I want an exact search?"
  (search-forward-regexp (concat apk:emacs-stdlib-list-exact-regexp name))
  (move-beginning-of-line 1))

(defun apk:org-find-table (&optional count)
  "Find a particular table with text NAME (based on headline) in BUFFER.
TODO: Currently prefix search, do I want an exact search?"
  (unless count
    (setq count 1))
  (dotimes (i count)
    (while (not (org-table-p))
      (forward-line 1))
    (when (< i (- count 1))
      (apk:org-table-last-row)
      (forward-line 2))))

(defun apk:org-table-to-lisp-no-separators ()
  "Convert the org-table to lisp and eliminate seperators."
  (delq nil (mapcar (lambda (x) (if (eq x 'hline) nil x)) (org-table-to-lisp))))

(defun apk:org-table-last-row ()
  "Goto the last row of the next table in the buffer.."
  (let (table-next
        seperator-next
        table-next-next
        (keep-going t))
    (while keep-going
        (save-excursion
          (forward-line 1)
          (setq table-next (org-table-p))
          (setq seperator-next (string-match "|-+\+.*|" (get-current-line)))
          (forward-line 1)
          (setq table-next-next (org-table-p)))
        (if (or
               (and table-next (not seperator-next))
               (and table-next seperator-next table-next-next))
            (forward-line)
          (setq keep-going nil)))))

(defun apk:org-table-lookup-location (filename table-name key-value &optional column)
  "Lookup the location (in buffer) of KEY-VALUE from TABLE-NAME
in FILENAME given COLUMN number."
  (unless column
    (setq column 1))
  (let (found)
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (when (string= key-value (strip-full (org-table-get nil column)))
                         (setq found (list filename (point)))))
    found))

(defun apk:org-table-lookup-row (filename table-name key-value  &optional column)
  "Lookup the row (in elisp) of KEY-VALUE from TABLE-NAME in FILENAME given COLUMN.
number."
  (unless column
    (setq column 1))
  (let (lisp-table
        found-row)
    (with-current-file-org-table filename table-name
                                 (setq lisp-table (apk:org-table-to-lisp-no-separators)))
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (when (string= key-value (strip-full (org-table-get nil column)))
                         (setq found-row (apk:org-table-assoc lisp-table key-value column))))
    found-row))

(defun apk:org-table-get-keys (filename table-name &optional column)
  "Get the list of keys from TABLE-NAME in FILENAME given
COLUMN. number"
  (unless column
    (setq column 1))
  (let (key-values)
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (setq key-values (cons (strip-full-no-properties (org-table-get nil column)) key-values)))
    (remove-if 'full-string-p key-values)
    key-values))

(defun apk:org-table-select-key (filename table-name prompt &optional column history-variable)
  "Select key using completing-read with PROMPT from TABLE-NAME
in FILENAME given COLUMN number."
  (unless column
    (setq column 1))
  (completing-read prompt (apk:org-table-get-keys filename table-name column) nil t history-variable))

(defun apk:org-table-assoc (lisp-table key &optional column equal-test)
  "Get row associated with KEY from LISP-TABLE."
  (unless column
    (setq column 1))
  (apk:assoc-nth column key lisp-table equal-test))

(defun apk:assoc-nth (n key assoc-list &optional equal-test)
  "TODO: not sure what this does?"
  (unless equal-test
    (setq equal-test 'equal))
  (let (selected
        (zeron (- n 1)))
    (dolist (item assoc-list)
      (when (funcall equal-test key (nth zeron item))
        (setq selected item)))
    selected))

(defun apk:get-headline-text (headline-line)
  "Get just the clean headline text from HEADLINE-LINE
representing the text of a line the headline is on."
  (let (headline-text)
    (when (string-match apk:emacs-stdlib-headline-regexp headline-line)
      (setq headline-text (match-string 1 headline-line)))))

(defun org-headline-p (line-substring)
  "Is LINE-SUBSTRING an org-mode headline?"
  (string-match apk:emacs-stdlib-headline-regexp line-substring))

(defun org-list-p (line-substring)
  "Is LINE-SUBSTRING an org-mode list item?"
  (string-match apk:emacs-stdlib-list-regexp line-substring))

(defun org-plain-list-p (line-substring)
  "Is LINE-SUBSTRING a plain (no checkboxes) org-mode list item?"
  (and
   (string-match apk:emacs-stdlib-list-regexp line-substring)
   (not (string-match apk:emacs-stdlib-checkbox-regexp line-substring))))

(defun org-checkbox-p (line-substring)
  "Is LINE-SUBSTRING an org-mode checkbox item?"
  (string-match apk:emacs-stdlib-checkbox-regexp line-substring))

(defun apk:org-check-last-heading-level-1 ()
  "Check if we are at the last heading level 1 in the file.  To
see if a loop can keep going."
  (let ((line-no (line-number-at-pos)))
    (save-excursion
      (org-forward-heading-same-level 1 nil)
      (if (equal line-no (line-number-at-pos))
          nil
        t))))

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
               (let ((current-line (get-current-line)))
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
         (when (string-match apk:emacs-stdlib-headline-regexp (get-current-line))
           (setq ,table-name (match-string 1 (get-current-line))))
         (save-excursion
           (forward-line 1)
           (setq ,table nil)
           (while (not (or (and (org-headline-p (get-current-line)) (= (org-outline-level) 1)) (org-table-p)))
             (forward-line 1))
           (when (org-table-p)
             (setq ,table (apk:org-table-to-lisp-no-separators))))
         (when (and ,table-name ,table)
           ,@body)
         ;; see if we can keep going
         (setq keep-going (apk:org-check-last-heading-level-1))
         ;; if not end
         (when keep-going
           (org-forward-heading-same-level 1 nil))))))

;; TODO make sure we can deal with seperators
(defmacro do-org-table-rows (filename table-name row &rest body)
  "Iterate over the rows of the table TABLE-NAME in FILENAME.
ROW contains the current row converted into elisp."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ;; TODO replace with org-headline-goto???
     (when (re-search-forward (concat "^\* " ,table-name) nil t)
       (apk:org-find-table)
       (let ((keep-going t)
             (lisp-table (apk:org-table-to-lisp-no-separators))
             (row-count 0))
         (while keep-going
           (unless (string-match "|-+\+.*|" (get-current-line))
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
;; (do-org-list-items "~/Dropbox/projects/org-agenda/main-agenda.org" "Supplies" item-line (list (mpp item-line)))
(defmacro do-org-list-items (filename list-name item-line &rest body)
  "Iterate over the items of list LIST-NAME in FILENAME.
ITEM-LINE contains the line that a particular item is on."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     (when (re-search-forward (concat "^\* " ,list-name) nil t)
       (forward-line 1)
       (let ((keep-going t))
         (while keep-going
           (setq ,item-line (get-current-line))
           ,@body
           (save-excursion
             (forward-line 1)
             (let ((current-line (get-current-line)))
               (when (and (org-headline-p current-line) (= (org-outline-level) 1))
                 ; (org-at-drawer-p)
                 (setq keep-going nil))))
           (when keep-going
             (forward-line 1)))))))

(defmacro set-unless (var &rest body)
  "Set a variable with body if it is nil."
  (error))

(defmacro when-string-match (pattern string group matched &rest body)
  "When GROUP of PATTERN matches string set MATCHED to the result
and execute BODY.."
  (declare (indent 1) ;; (debug t)
           )
  `(unless ,group
     (setq ,group 0))
  `(when (string-match ,pattern ,string)
     (setq ,matched (match-string ,group ,string))
     ,@body))

(defmacro with-current-buffer-create (buffer-or-name &rest body)
  "Create BUFFER-OR-NAME if nonexistant and run BODY like
with-current-buffer."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (get-buffer-create ,buffer-or-name))
     ,@body))

(defmacro with-current-buffer-min (buffer-or-name &rest body)
  "Like with-current-buffer but always go to point-min."
  `(save-excursion
     (set-buffer ,buffer-or-name)
     (goto-char (point-min))
     ,@body))

(defmacro with-current-file (filename &rest body)
  "Execute BODY with FILENAME as buffer.  Uses
with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     ,@body))

(defmacro with-current-file-headline (filename headline &rest body)
  "Execute BODY with FILENAME as buffer after finding HEADLINE.
Uses with-filename-filter."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (apk:org-find-headline ,headline)
     ,@body))

(defmacro with-current-file-min (filename &rest body)
  "Like with-current-file, but always go to point-min."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     ,@body))

(defmacro with-current-file-org-table (filename table-name &rest body)
  "Like with-current-file, but find TABLE-NAME."
  (declare (indent 1) ;; (debug t)
           )
  `(save-excursion
     (set-buffer (find-file-noselect (with-filename-filter ,filename)))
     (goto-char (point-min))
     (when (re-search-forward (concat "^\* " ,table-name) nil t)
       (apk:org-find-table)
       ,@body)))

(defun car-fallthrough (object)
  "If car-safe does not work, just return the object.  Otherwise
return car of OBJECT."
  (if (car-safe object)
      (car object)
    object))

(defun cdr-fallthrough (object)
  "If cdr-safe does not work, just return OBJECT. Otherwise
return cdr of OBJECT."
  (if (cdr-safe object)
      (cdr object)
    object))

; http://www.emacswiki.org/emacs/ElispCookbook#toc20
(defun string-integer-p (string)
  (if (string-match "\\`[-+]?[0-9]+\\'" string)
      t
    nil))

(defun string-float-p (string)
  (if (string-match "\\`[-+]?[0-9]+\\.[0-9]*\\'" string)
      t
    nil))

;; XXXX: why does this work?
(defun zip (&rest streams)
  "Zip function like in many programming languages."
  (apply #'mapcar* #'list streams))

(defun ensure-list (object)
  "Turn an OBJECT that is not a list into a list.  If it is
already a list then leave it alone."
  (unless (listp object)
          (setq object (list object)))
  object)

(defun car-only (lst)
  "Ensure a list is only length 1 and get the car.  Raise an
error is list is longer than length 1."
  (when (> (length lst) 1)
    (error))
  (car lst))

(defun nts-nan (num)
  "Like number-to-string but returns a nil for nan.
TODO: Make formatting an option so more universal."
  (if (or (not num) (isnan (float num)) (= num 1.0e+INF) (= num -1.0e+INF))
      ""
    (format "%4.3f" num)))

(defun apk:org-last-headline ()
  "Goto last headline in the current buffer."
  (goto-char (point-min))
  (when (not (org-headline-p))
    (org-forward-element))
  (let (headline-position-list)
    (do-org-headlines (buffer-file-name) headline subtree
                      (setq headline-position-list (append headline-position-list (list (point)))))
    (goto-char (car (last headline-position-list)))))

(defun delete-substring (substring-regexp string)
  "Delete the substring matched by SUBSTRING-REGEXP from STRING."
  (if (string-match substring-regexp string)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (concat (substring string 0 beg) (substring string end)))
    string))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
(defun ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                         string)
       t))

(defun starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

;; I really do want this here if the following function will be useful at all
(random t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file helpers

;; do we open a new window?
;; do we switch?
;; TODO this can be fixed a lot!!!
;; TODO ensure no line argument goes to line 1
;; make sure existing buffer is opened!!!
(defun find-file-other-window-goto-line (filename &optional line alternate)
  "Find and select FILENAME in other window and goto line number
LINE."
  (let ((old-buffer (current-buffer))
        (old-window (get-buffer-window))
        new-buffer)
    (save-excursion
      ;; pop open new file in other buffer
      (setq new-buffer (find-file-other-window filename))
      (switch-to-buffer new-buffer)
      ;; jump to the correct line, in case buffer is already open
      (goto-char (point-min))
      (forward-line (1- line)))
  (select-window old-window)
  (set-buffer old-buffer)))

(defun list-files (path)
  "List files in PATH.
TODO: move this to somewhere generic, but have it also handle cic
paths."
  (filter-excise (lambda (e)
                   (not (file-directory-p (join-paths path e))))
                 (directory-files path)))

(defun find-file-goto-line (filename &optional line)
  (let ((old-buffer (current-buffer))
        (old-window (get-buffer-window))
        new-buffer)
    (save-excursion
      ;; pop open new file in other buffer
      (setq new-buffer (find-file filename))
      (switch-to-buffer new-buffer)
      ;; jump to the correct line, in case buffer is already open
      (goto-char (point-min))
      (when line
        (forward-line (1- line))))
  (select-window old-window)
  (set-buffer old-buffer)))

(defun apk:is-empty-string-whitespace (str)
  "Test if STR is empty or all whitespace."
  (string-equal (strip-full str) ""))

(defun apk:is-not-empty-string-nil (str)
  (and str (not (string= (strip-full str) ""))))

(defun delete-current-line ()
  "Delete the current line without touching the kill ring.
TODO: this function needs work."
  (let ((p1 (line-beginning-position))
        saved-text-properties
        p2)
    (save-excursion
      (goto-char (line-end-position))
      (forward-line 1)
      ;; save text properties
      ;; (setq saved-text-properties (text-properties-at (point)))
      (setq p2 (point)))
    (delete-region p1 p2)
    ;; restore text properties
    ;; TODO make actually work for all situations
    ;; (add-text-properties (point) (+ (point) 1) saved-text-properties)
    ))

;;from http://ergoemacs.org/emacs/elisp_all_about_lines.html
(defun get-current-line ()
  "Get the current line."
  (let ((p1 (line-beginning-position))
        (p2 (line-end-position)))
    (buffer-substring-no-properties p1 p2)))

(defun apk:get-current-location ()
  "Get the current location as <<filename>>::<<lineno>>"
  ;; TODO not sure if this should be here
  (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos))))

(defun apk:check-convert-number-to-string (number)
  "Try and convert NUMBER to a string, or just return untouched
if not possible."
  (if (string-integer-p number)
      (string-to-number number)
    number))

(defmacro with-time (time-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
  (list
   (if time-output-p 'apk:org-time-seconds-to-string 'identity)
   (cons 'progn
         (mapcar
          (lambda (expr)
            `,(cons (car expr)
                    (mapcar
                     (lambda (el)
                       (if (listp el)
                           (list 'with-time nil el)
                         (apk:org-time-string-to-seconds el)))
                     (cdr expr))))
          `,@exprs))))

(defun apk:org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
        ((>= secs 60) (format-seconds "%m:%.2s" secs))
        (t (format-seconds "%s" secs))))

(defun apk:org-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
          (min (string-to-number (match-string 2 s)))
          (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
          (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))

(defun apk:symbol-to-string-or-list (maybe-string-list)
  "Dereference a symbol to (presumably) a string or list of
strings. Leave alone if already a string or list of strings"
  (let (new-string-list)
    (if (symbolp maybe-string-list)
        (setq new-string-list (symbol-value maybe-string-list))
      (setq new-string-list maybe-string-list))
    (unless (listp new-string-list)
      (setq new-string-list (list new-string-list)))
    new-string-list))

(defun increment-char (ch)
  "Not perfect, need better way sometimes to select file..."
  (cond ((string= ch "z")
         "A")
        ((string= ch "Z")
         "1")
        (t
         (char-to-string (+ (string-to-char ch) 1)))))

(defun select-list-item (lst &optional string-key)
  "Select a string from a list of strings LST using alphabet then number keys.
TODO: use string-key to select a string"
  (let* ((count "a")
         (cancel 'cancel)
         (select-alist (mapcar (lambda (e)
                                 (let ((thelist (list count e e)))
                                   (setq count (increment-char count))
                                   thelist))
                               lst)))
    (setq select-alist (append select-alist (list (list "-" "cancel" 'cancel))))
    (select-nested-alist (lambda (e)
                           (setq selected e))
                         select-alist)))

(defun select-list-item-default-index (lst &optional string-key default-value)
  "Select a list item from LST.
TODO: document more"
  (let* ((count "a")
         (index-count 0)
         (cancel 'cancel)
         (default 'default)
         (select-alist (mapcar (lambda (e)
                                 (let (thelist)
                                   (if string-key
                                       (setq thelist (list count (funcall string-key e) index-count))
                                     (setq thelist (list count e index-count)))
                                   (setq count (increment-char count))
                                   (setq index-count (+ index-count 1))
                                   thelist))
                               lst)))

    (if default-value
        (setq select-alist (append select-alist (list (list "0" (concat "default " default-value) 'default) (list "-" "cancel" 'cancel))))
      (setq select-alist (append select-alist (list (list "0" "default" 'default) (list "-" "cancel" 'cancel)))))
    (select-nested-alist (lambda (e)
                           (setq selected e))
                         select-alist)))

(defun replace-nonprintable (str)
  "Replace non-printable characters with blanks in STR.
TODO: No str variable name and replace some bad characters with
dsimilar ones."
  (replace-regexp-in-string "[^[:print:]]" "" str))

(defun apk:get-list-duplicates (lst)
  "Get the duplicate items in list LST."
  (set-difference lst (delete-duplicates lst :test 'equal)))

(defun apk:test-strings-in-list (lst1 lst2)
  "Tests if any items in LST1 are also in LST2."
  (cl-intersection lst1 lst2 :test 'string=))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun apk:read-file-lines (filepath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" t)))

(defun apk:read-file-regexp (filepath)
  "Return a list of lines of a file at FILEPATH.  All lines are
properly escaped and combined with | to be an emacs regexp."
  (let ((file-lines (apk:read-file-lines filepath)))
    ;; rather than identity I need a generic escape function
    (mapconcat 'apk:escape-posix-regexp file-lines "\\|")))

(defun apk:escape-posix-regexp (posix-regexp)
  "Escapes a few select posix regexps to emacs regexps.
  Generally functionality is added here as needed."
  (replace-regexp-in-string (regexp-quote "\\\\") "\\\\" posix-regexp))

(provide 'emacs-stdlib-functions)
