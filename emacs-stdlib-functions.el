;;; emacs-stdlib-functions.el --- Standard emacs function that should
;;; have many uses.
;;
;; Copyright (C) 2015-2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20160511
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
;; XXXX: Have not added some extremely common functions to having cic:
;; prefix.  May put these in their own file.
;;
;;; Code:

(defun cic:mpp (value &optional buffer)
  "Pretty print a message to a particular buffer and include a
time-stamp in the message.
XXXX: not adding cic: prefix before this function is called so often in adhoc code
TODO: flag to not use timestamp"
  (let ((message-string (concat (cic:time-stamp) "\n" (with-output-to-string (princ value)))))
    (unless buffer
      (setq buffer (get-buffer-create "*PPCapture*")))
    (save-excursion (with-current-buffer (get-buffer-create buffer)
                      (goto-char (point-max))
                      (insert (concat message-string "\n"))))))

(defun cic:mpp-echo (value &optional buffer)
  "Pretty print a message to a particular buffer and include a
time-stamp in the message.  Echo in minibuffer area as well.
XXXX: not adding cic: prefix before this function is called so often in adhoc code
TODO: flag to not use timestamp"
  (let* ((raw-message-string (with-output-to-string (princ value)))
         (message-string (concat (cic:time-stamp) "\n" raw-message-string)))
    (unless buffer
      (setq buffer (get-buffer-create "*PPCapture*")))
    (save-excursion (with-current-buffer (get-buffer-create buffer)
                      (goto-char (point-max))
                      (insert (concat message-string "\n"))))
    (message raw-message-string)))

;; XXXX: this could cause issues with debugging if function is made more complex
(when (not (fboundp 'cic:find-file-meta))
  (defun cic:find-file-meta (filename &optional wildcards)
    "A find file function that is often replaced with something else in my special setups"
    (find-file filename wildcards)))

(when (not (fboundp 'cic:get-filename--meta))
  (defun cic:get-filename--meta (filename)
    "A get file function that is often replaced with something else in my special setups"
    filename))

(defun cic:time-stamp ()
  "Create a time-stamp."
  (format-time-string "%H:%M:%S" (current-time)))

(defun cic:strip-full (str)
  "Strip full leading and trailing whitespace from STR.  Does
this for every line."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun cic:chomp (str)
  "Chomp leading and tailing whitespace from STR.  Just beginning and end of lines
TODO: are this one and strip-full redundant?
TODO: determine which is more efficient"
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun cic:strip-full-no-properties (str)
  "Like strip-full but remove text properties."
  (substring-no-properties (strip-full str)))

(defun cic:strip-full-single-spaces (str)
  "Convert STR into one line with no leading or trailing whitespace."
  (while (string-match "[[:space:]]*\n+[[:space:]]*\\|[[:space:]]\\{2,\\}"
                       str)
    (setq str (replace-match " " t t str)))
  (strip-full str))

(defun cic:strip-colons (str)
  "Strip leading and trailing colons off of STR."
  (when (stringp str)
    (while (string-match "^:" str)
      (setq str (replace-match "" t t str)))
    (while (string-match ":$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun cic:strip-after-double-colon (str)
  "Strip everything after and including a double colon in STR."
  (when (stringp str)
    (if (string-match "\\(.*\\)::.*" str)
        (setq str (match-string 1 str))))
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

(defun cic:remove-trailing-whitespace (str)
  "Strip trailing whitespace off of STR."
  (when (string-match "[ \t\n]*$" str)
    (concat (replace-match "" nil nil str))))

(defun cic:remove-leading-whitespace (str)
  "Strip leading whitespace off of STR."
  (when (string-match "^[ \t\n]*" str)
    (concat (replace-match "" nil nil str))))

(defun cic:full-string-p (thing-or-string)
  "Determine if something is nil or an empty string."
  (if (or (not thing-or-string) (equal (strip-full thing-or-string) ""))
      nil
    t))

;; taken from stackoverflow
(defun cic:count-occurences (regex string)
  "Count the occurences of REGEX in STRING."
  (cic:recursive-count regex string 0))

(defun cic:recursive-count (regex string start)
  "Helper function for cic:count-occurences to Recursively count
REGEX in STRING from the START character."
  (if (string-match regex string start)
      (1+ (cic:recursive-count regex string (match-end 0)))
    0))

(defun cic:string-to-float (str)
  "Convert STR to float."
  (let (new-var)
    (when (stringp str)
      (setq str (string-to-number str)))
    (unless (floatp str)
      (setq str (float str)))
    str))

(defun cic:string-to-float-empty-zero (str)
  "Convert STR to float or return zero for a nil or empty
string."
  (if (cic:is-not-empty-string-nil str)
      (cic:string-to-float str)
    0))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc59
(defun cic:walk-path (dir action)
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
                         (cic:walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))

(defun cic:org-find-headline (headline &optional buffer)
  "Find a particular HEADLINE in BUFFER."
  (goto-char (org-find-exact-headline-in-buffer headline)))

(defun cic:org-find-checkbox (name)
  "Find a particular checkbox with text NAME in BUFFER.
TODO: Currently prefix search, do I want an exact search?"
  (search-forward-regexp (concat cic:emacs-stdlib-checkbox-regexp name))
  (move-beginning-of-line 1))

(defun cic:org-find-list-item (name)
  "Find a particular list item with text NAME in BUFFER.
TODO: Currently actually prefix search, do I want an exact search?"
  (search-forward-regexp (concat cic:emacs-stdlib-list-exact-regexp name))
  (move-beginning-of-line 1))

(defun cic:org-find-table (&optional count)
  "Find a particular table with text NAME (based on headline) in BUFFER.
"
  (unless count
    (setq count 1))
  (dotimes (i count)
    (while (not (or (org-table-p) (eobp)))
      (forward-line 1))
    (when (< i (- count 1))
      (cic:org-table-last-row)
      (forward-line 2))))

(defun cic:org-table-to-lisp-no-separators ()
  "Convert the org-table to lisp and eliminate seperators."
  (delq nil (mapcar (lambda (x) (if (eq x 'hline) nil x)) (org-table-to-lisp))))

(defun cic:org-table-last-row ()
  "Goto the last row of the next table in the buffer.."
  (let (table-next
        seperator-next
        table-next-next
        (keep-going t))
    (while keep-going
        (save-excursion
          (forward-line 1)
          (setq table-next (org-table-p))
          (setq seperator-next (string-match "|-+\+.*|" (cic:get-current-line)))
          (forward-line 1)
          (setq table-next-next (org-table-p)))
        (if (or
               (and table-next (not seperator-next))
               (and table-next seperator-next table-next-next))
            (forward-line)
          (setq keep-going nil)))))

(defun cic:org-table-lookup-location (filename table-name key-value &optional column)
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

(defun cic:org-table-lookup-row (filename table-name key-value  &optional column)
  "Lookup the row (in elisp) of KEY-VALUE from TABLE-NAME in FILENAME given COLUMN.
number."
  (unless column
    (setq column 1))
  (let (lisp-table
        found-row)
    (with-current-file-org-table filename table-name
                                 (setq lisp-table (cic:org-table-to-lisp-no-separators)))
    (do-org-table-rows filename table-name row
                       (org-table-goto-column column)
                       (when (string= key-value (strip-full (org-table-get nil column)))
                         (setq found-row (cic:org-table-assoc lisp-table key-value column))))
    found-row))

(defun cic:org-table-get-keys (filename table-name &optional column)
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

(defun cic:org-table-select-key (filename table-name prompt &optional column history-variable)
  "Select key using completing-read with PROMPT from TABLE-NAME
in FILENAME given COLUMN number."
  (unless column
    (setq column 1))
  (completing-read prompt (cic:org-table-get-keys filename table-name column) nil t history-variable))

(defun cic:org-table-assoc (lisp-table key &optional column equal-test)
  "Get row associated with KEY from LISP-TABLE."
  (unless column
    (setq column 1))
  (cic:assoc-nth column key lisp-table equal-test))

(defun cic:assoc-nth (n key assoc-list &optional equal-test)
  "Get the Nth item from the KEY from ASSOC-LIST with an optional
EQUAL-TEST that defaults to equal"
  (unless equal-test
    (setq equal-test 'equal))
  (let (selected
        (zeron (- n 1)))
    (dolist (item assoc-list)
      (when (funcall equal-test key (nth zeron item))
        (setq selected item)))
    selected))

(defun cic:get-headline-text (headline-line)
  "Get just the clean headline text from HEADLINE-LINE
representing the text of a line the headline is on."
  (let (headline-text)
    (when (string-match cic:emacs-stdlib-headline-regexp headline-line)
      (setq headline-text (match-string 1 headline-line)))))

(defun cic:org-headline-p (line-substring)
  "Is LINE-SUBSTRING an org-mode headline?"
  (string-match cic:emacs-stdlib-headline-regexp line-substring))

(defun cic:org-list-p (line-substring)
  "Is LINE-SUBSTRING an org-mode list item?"
  (string-match cic:emacs-stdlib-list-regexp line-substring))

(defun cic:org-plain-list-p (line-substring)
  "Is LINE-SUBSTRING a plain (no checkboxes) org-mode list item?"
  (and
   (string-match cic:emacs-stdlib-list-regexp line-substring)
   (not (string-match cic:emacs-stdlib-checkbox-regexp line-substring))))

(defun cic:org-checkbox-p (line-substring)
  "Is LINE-SUBSTRING an org-mode checkbox item?"
  (string-match cic:emacs-stdlib-checkbox-regexp line-substring))

(defun cic:org-check-last-heading-level-1 ()
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
         (when (string-match cic:emacs-stdlib-headline-regexp (cic:get-current-line))
           (setq ,table-name (match-string 1 (cic:get-current-line))))
         (save-excursion
           (forward-line 1)
           (setq ,table nil)
           (while (not (or (and (cic:org-headline-p (cic:get-current-line)) (= (org-outline-level) 1)) (org-table-p) (eobp)))
             (forward-line 1))
           (when (org-table-p)
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
     (cic:org-find-headline ,headline)
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
     (when (re-search-forward (concat "^\* " ,table-name "$") nil t)
       (cic:org-find-table)
       ,@body)))

(defun cic:car-fallthrough (object)
  "If car-safe does not work, just return the object.  Otherwise
return car of OBJECT."
  (if (car-safe object)
      (car object)
    object))

(defun cic:cdr-fallthrough (object)
  "If cdr-safe does not work, just return OBJECT. Otherwise
return cdr of OBJECT."
  (if (cdr-safe object)
      (cdr object)
    object))

; http://www.emacswiki.org/emacs/ElispCookbook#toc20
(defun cic:string-integer-p (string)
  "Check if STRING is an integer."
  (if (string-match "\\`[-+]?[0-9]+\\'" string)
      t
    nil))

(defun cic:string-float-p (string)
  "Check if STRING is a floating point number."
  (if (string-match "\\`[-+]?[0-9]+\\.[0-9]*\\'" string)
      t
    nil))

;; XXXX: why does this work?
(defun cic:zip (&rest streams)
  "Zip function like in many programming languages."
  (apply #'mapcar* #'list streams))

(defun cic:ensure-list (object)
  "Turn an OBJECT that is not a list into a list.  If it is
already a list then leave it alone."
  (unless (listp object)
          (setq object (list object)))
  object)

(defun cic:car-only (lst)
  "Ensure a list is only length 1 and get the car.  Raise an
error is list is longer than length 1."
  (when (> (length lst) 1)
    (error))
  (car lst))

(defun cic:nts-nan (num &optional format-string)
  "Like number-to-string but returns a nil for nan.
TODO: Make formatting an option so more universal."
  (unless format-string
    (setq format-string "%4.3f"))
  (if (or (not num) (isnan (float num)) (= num 1.0e+INF) (= num -1.0e+INF))
      ""
    (format format-string num)))

(defun cic:org-last-headline ()
  "Goto last headline in the current buffer."
  (goto-char (point-min))
  (when (not (cic:org-headline-p))
    (org-forward-element))
  (let (headline-position-list)
    (do-org-headlines (buffer-file-name) headline subtree
                      (setq headline-position-list (append headline-position-list (list (point)))))
    (goto-char (car (last headline-position-list)))))

(defun cic:delete-substring (substring-regexp string)
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

;; XXXX: I really do want this here if the following function will be useful at all
(random t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file helpers

;; do we open a new window?
;; do we switch?
;; TODO this can be fixed a lot!!!
;; TODO ensure no line argument goes to line 1
;; make sure existing buffer is opened!!!
(defun cic:find-file-other-window-goto-line (filename &optional line alternate)
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

(defun cic:list-files (path)
  "List files in PATH."
  (cic:filter-excise (lambda (e)
                   (not (file-directory-p (cic:join-paths path e))))
                 (directory-files path)))

(defun cic:find-file-goto-line (filename &optional line)
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

(defun cic:is-empty-string-whitespace (str)
  "Test if STR is empty, all whitespace, or nil."
  (when str
    (string-equal (strip-full str) "")))

(defun cic:is-not-empty-string-nil (str)
  "Check if STR is an empty string (no characters or all
whitespace) or a nil."
  (and str (not (string= (strip-full str) ""))))

(defun cic:delete-current-line ()
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
    ;; (add-text-properties (point) (1+ (point)) saved-text-properties)
    ))

;;from http://ergoemacs.org/emacs/elisp_all_about_lines.html
(defun cic:get-current-line ()
  "Get the current line."
  (buffer-substring-no-properties(line-beginning-position) (line-end-position)))

;; TODO: this is odd, can't seem to work
(defun cic:check-convert-number-to-string (number)
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
   (if time-output-p 'cic:org-time-seconds-to-string 'identity)
   (cons 'progn
         (mapcar
          (lambda (expr)
            `,(cons (car expr)
                    (mapcar
                     (lambda (el)
                       (if (listp el)
                           (list 'with-time nil el)
                         (cic:org-time-string-to-seconds el)))
                     (cdr expr))))
          `,@exprs))))

(defun cic:org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
        ((>= secs 60) (format-seconds "%m:%.2s" secs))
        (t (format-seconds "%s" secs))))

(defun cic:org-time-string-to-seconds (s)
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

(defun cic:symbol-to-string-or-list (maybe-string-list)
  "Dereference a symbol to (presumably) a string or list of
strings. Leave alone if already a string or list of strings"
  (let (new-string-list)
    (if (symbolp maybe-string-list)
        (setq new-string-list (symbol-value maybe-string-list))
      (setq new-string-list maybe-string-list))
    (cic:ensure-list new-string-list)))

(defun increment-char (ch)
  "Not perfect, need better way sometimes to select file..."
  (cond ((string= ch "z")
         "A")
        ((string= ch "Z")
         "1")
        (t
         (char-to-string (1+ (string-to-char ch))))))
;; (cic:select-list-item (list "hello" "jaws"))
(defun cic:select-list-item (lst &optional string-key)
  "Select a string from a list of strings LST using alphabet then number keys.
TODO: use string-key to select a string"
  (let* ((count "a")
         (index-count 0)
         (cancel 'cancel)
         (select-alist (mapcar (lambda (e)
                                 (let (the-list)
                                   (if string-key
                                       (setq thelist (list count (funcall string-key e) index-count))
                                     (setq thelist (list count e e)))
                                   (setq count (increment-char count))
                                   (setq index-count (1+ index-count))
                                   thelist))
                               lst)))
    (setq select-alist (append select-alist (list (list "-" "cancel" 'cancel))))
    (cic:select-nested-alist (lambda (e)
                           (setq selected e))
                         select-alist)))

(defun cic:select-list-item-default-index (lst &optional string-key default-value)
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
                                   (setq index-count (1+ index-count))
                                   thelist))
                               lst)))

    (if default-value
        (setq select-alist (append select-alist (list (list "0" (concat "default " default-value) 'default) (list "-" "cancel" 'cancel))))
      (setq select-alist (append select-alist (list (list "0" "default" 'default) (list "-" "cancel" 'cancel)))))
    (cic:select-nested-alist (lambda (e)
                           (setq selected e))
                         select-alist)))

(defun replace-nonprintable (str)
  "Replace non-printable characters with blanks in STR.
TODO: No str variable name and replace some bad characters with
dsimilar ones."
  (replace-regexp-in-string "[^[:print:]]" "" str))

(defun cic:get-list-duplicates (lst)
  "Get the duplicate items in list LST."
  (set-difference lst (remove-duplicates lst :test 'equal)))

(defun cic:test-strings-in-list (lst1 lst2)
  "Tests if any items in LST1 are also in LST2."
  (cl-intersection lst1 lst2 :test 'string=))

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun cic:read-file-lines (filepath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" t)))

(defun cic:read-file-regexp (filepath)
  "Return a list of lines of a file at FILEPATH.  All lines are
properly escaped and combined with | to be an emacs regexp."
  (let ((file-lines (cic:read-file-lines filepath)))
    ;; rather than identity I need a generic escape function
    (mapconcat 'cic:escape-posix-regexp file-lines "\\|")))

(defun cic:escape-posix-regexp (posix-regexp)
  "Escapes a few select posix regexps to emacs regexps.
  Generally functionality is added here as needed."
  (replace-regexp-in-string (regexp-quote "\\\\") "\\\\" posix-regexp))

(defun cic:join-paths (&rest args)
  "Join paths in elisp.

   XXX: Only works with two arguments!"
  (concat (file-name-as-directory (car args)) (cadr args)))

(defun count-indentation (&optional current-line)
  "Count the indentation level in CURRENT-LINE, if nil use the
current line at point."
  (unless current-line
    (setq current-line (cic:get-current-line)))
  (- (length current-line) (length (remove-leading-whitespace current-line))))

(defun cic:list-from-file-filename (filename)
  "Get a list based on the lines in FILENAME."
  (remove-if-not 'cic:is-not-empty-string-nil
                 (with-temp-buffer
                   (insert-file-contents filename)
                   (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

;; http://emacswiki.org/emacs/ElispCookbook#toc39
(defun cic:filter-excise (condp lst)
  "Remove any elements that do not meet CONDP."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filesystem helper functions

(defun cic:find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ;; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ;; Not found
                       (t (find-file-r (directory-file-name parent))))))) ;; Continue
    (find-file-r default-directory)))

(defun cic:org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed.
TODO: figure out what to do with this and whether I use it?"
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new functions

(defun goto-location (location)
  "XXXX: A helper function that will soon be replaced.
Abstraction level is too high."
  (if (stringp (car location))
      (progn
        (find-file (car location))
        (when (eq major-mode 'org-mode)
          (org-cycle '(64)))
        (goto-char (cadr location)))
    (progn
      (switch-to-buffer (car location))
      (when (eq major-mode 'org-mode)
        (org-cycle '(64)))
      (goto-char (cadr location)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string, shell command, and shell helpers

(defun cic:make-shell-command (&rest args)
  "Create a shell command from ARGS.

XXXX: Not currently used or tested."
  (concat (car args) " " (combine-and-quote-strings (cdr args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thing at point bounds

(defun cic:region-or-thing-at-point-no-properties (thing)
  "Select either a region if active or the thing-at-point."
  (let ((current-thing (cic:region-or-thing-at-point thing)))
    (if current-thing
        (substring-no-properties current-thing)
      nil)))

(defun cic:region-or-thing-at-point (thing)
  "Select either a region if active or the thing-at-point."
  (let (selected-text)
    (if (region-active-p)
        (setq selected-text (buffer-substring (mark) (point)))
      (setq selected-text (thing-at-point thing)))
    selected-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current filename
(defun cic:get-current-filename ()
  "Get the current filename based on context."
  (cond ((eq major-mode 'dired-mode)
         (file-name-nondirectory (dired-file-name-at-point)))
        (t
         (file-name-nondirectory (buffer-file-name)))))

;; from https://github.com/jimm/elisp/blob/master/emacs.el
;; http://en.wikipedia.org/wiki/Password_strength
;; 24 alphanumpunc characters = 144 bits of entropy

(defconst cic:password-characters-Alphanum
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "This is 62 characters total")

(defconst cic:password-characters-Alphanum-punct
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!?@#$%^&*-_=+/.,"
  "This is 78 characters total.")

(defconst cic:password-characters-Alpha
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "This is 52 characters total.")

(defun cic:create-password-12-Alphanum ()
  "Create a 12 character alphanumeric password. Don't know of
  anywhere that can't do alphanum with capitals."
  (cic:create-password cic:password-characters-Alphanum 12))

(defun cic:create-password-12-Alphanum-punct ()
  "Create a 12 character alphanumeric password with punctuation
  and capitals."
  (cic:create-password cic:password-characters-Alphanum-punct 12))

(defun cic:create-password-24-Alphanum ()
  "Create a 24 character alphanumeric password with capitals.  Don't know of
  anywhere that can't do alphanum with capitals."
  (cic:create-password cic:password-characters-Alphanum 24))

(defun cic:create-password-24-Alphanum-punct ()
  "Create a 24 character alphanumeric password with punctuation
  and capitals."
  (cic:create-password cic:password-characters-Alphanum-punct 24))

(defun cic:create-password-30-alpha-lower ()
  "Create a 30 character alphanumeric password with punctuation
  and capitals."
  (downcase (cic:create-password cic:password-characters-Alpha 30)))

(defun cic:create-password (char-set char-num &optional random-source)
  "Create a random password from CHAR-SET that is CHAR-NUM
characters long.  The shell command 'openssl rand' is used, but
setting RANDOM-SOURCE non-nil uses built-in random function.

XXXX: This function should absolutely be checked before using for
anything too critical!!!

TODO: Even more options for random numbers might be better but I
think openssl is find for this purpose."
  (let ((length-passwordchars (length char-set)))
    (cond (random-source
           (mapconcat (lambda (dummy)
                        (let ((idx (random length-passwordchars)))
                          (substring char-set idx (1+ idx))))
                      (number-sequence 0 (- char-num 1))
                      ""))
          (t
           (let ((new-password "")
                 (new-password-char))
             (dotimes (n char-num)
               ;; get a random number
               (setq new-password-char nil)
               (while (not new-password-char)
                 (setq new-password-char (string-to-number (strip-full (shell-command-to-string "openssl rand -hex 1")) 16))
                 (if (>= new-password-char length-passwordchars)
                     (setq new-password-char nil)
                   (setq new-password (concat new-password (substring char-set new-password-char (1+ new-password-char)))))))
             new-password)))))

(defun cic:select-nested-alist (&optional command filter-alists) ;;  filter-alist-first)
  "Allows user to select from an alist.
TODO: I don't actually use the nested function and I think this
can be greatly simplified."
  ;; "Allows user to select a symbol from a nested alist"
  (interactive)
  ;; get keys until alist is exhausted
  (let ((minibuffer-prompt "")
        (key-press nil)
        (current-alist filter-alists)
        ;; (current-alist-index filter-alist-first)
        ;; (continue-filter t)
        (continue t)
        (escape-action nil)
        (inner-alist nil)
        (inner-selected)
        (selected)
        (selected-text))
    ;; keep reading input while there are inner alists
    (while continue
      ;; read input
      ;; initialise variables
      (setq minibuffer-prompt "")
      ;; TODO do I want this?
      (setq escape-action nil)
      (dolist (search-key current-alist)
        (setq minibuffer-prompt (concat minibuffer-prompt
                                        (concat "(" (car search-key) ") " (cadr search-key) "\n"))))
      ;; read a key
      (setq key-press (make-string 1 (read-key minibuffer-prompt)))
      ;; test for valid keypress, inner-selection is nil if not valid
      (setq inner-selection (assoc key-press current-alist))
      ;; use input, decide if alist is in fact an inner alist
      (cond ((not inner-selection)
             ;; continue at same level
             )
            ((not (cic:nested-alist-maybe inner-selection))
             ;; bottomed out, don't continue
             ;; select alist
             (setq selected (caddr inner-selection))
             (setq selected-text (cadr inner-selection))
             (setq continue nil))
            (t
             ;; a list was selected, go down a level
             (setq current-alist (elt (cdr inner-selection) 1)))))
    (apply command (cic:symbol-to-string-or-list selected))))

(defun cic:nested-alist-maybe (maybe-nested)
  "XXXX: A helper function that may not be required."
  (and
   (listp (cddr maybe-nested))
   (listp (caddr maybe-nested))))
   ;; (listp ())))
   ;; is the second element a nested list
   ;; (listp (car (cdr maybe-nested)))))
   ;; ;; is the first element of the second also a list?
   ;; (listp (cdr (car (cdr maybe-nested))))))

(defun cic:org-insert-indent-list-item ()
  "XXXX: A helper function that may not be required."
  (if (cic:org-headline-p (cic:get-current-line))
      (progn
        (move-end-of-line 1)
        (insert "\n")
        (org-cycle)
        (insert " - "))
    (progn
      (move-end-of-line 1)
      (org-meta-return))))

(defun cic:uid-64 ()
  "Create an 11 character (>64bit) unique ID."
  ;; TODO: change to uid-11 to relect it may not quite be 64 bit
  (cic:create-password "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_" 11))

(defun browse-url-conkeror (url &rest args)
  "Browse a url in the Conkeror web browser."
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic url)))

(defun cic:org-table-elisp-replace (elisp-table-original elisp-table-replacement)
  "Replace the original table at point ELISP-TABLE-ORIGINAL with
the ELISP-TABLE-REPLACEMENT.  Only uses expensive org-table-put
when values has changed.

Meant to be used programatically and behaviour is undefined if
there is not mutual correspondance between table at point,
ELISP-TABLE-ORIGINAL, and ELISP-TABLE-REPLACEMENT."
  ;; TODO: add some quick sanity checks here
  (when (org-at-table-p)
    (save-excursion
      (let ((nrows (length elisp-table-original))
            (ncols (length (car elisp-table-original))))
        ;; TODO: do better than straight imperative programming?
        (dotimes (i nrows)
          (dotimes (j ncols)
            (when (not (string= (cic:elisp-array-string elisp-table-original i j)
                                (cic:elisp-array-string elisp-table-replacement i j)))
              (org-table-put (1+ i) (1+ j) (cic:elisp-array-string elisp-table-replacement i j)))))))))

(defun cic:elisp-array-string (elisp-array i j)
  (let ((thestr (elt (elt elisp-array i) j)))
    (if (stringp thestr)
        (cic:strip-full-no-properties thestr)
      "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tbel functions
;; TODO: make an independent package

(defun cic:org-table-tblel-p ()
  "Find if we are at a location associated with an org-table
along with a #+TBLEL line."
  (or
   (cic:org-table-tblel-line-p)
   (save-excursion
     (and
      (org-table-p)
      (progn
        (goto-char (org-table-end))
        (beginning-of-line)
        (looking-at "\\s-+#\\+TBLEL:"))))))

(defun cic:org-table-tblel-line-p ()
  "Check if at a #+TBLEL line."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-+#\\+TBLEL:")))

(defun cic:org-table-eval-tblel-line (&rest args)
  "Evalute the function on a #+TBLEL line."
  (when (cic:org-table-tblel-line-p)
    (cic:org-table-eval-tblel)))

(defun cic:org-table-eval-tblel (&rest args)
  "Get the lisp table and run the appropriate function on it (several functions?)."
  ;; TODO: unwind protect to avoid nuking table
  ;; TODO: avoid back-to-heading/find-table and use a better methodology for tables
  (interactive)
  (when (cic:org-table-tblel-p)
    (let (lisp-table
          lisp-function
          new-lisp-table)
      ;; get elisp function to run
      (save-excursion
        (unless (cic:org-table-tblel-line-p)
            (goto-char (org-table-end)))
        (beginning-of-line)
        ;; TODO: eventually get forms
        (setq lisp-function (substring-no-properties (elt (split-string (cic:get-current-line)) 1))))
      (save-excursion
        (when (cic:org-table-tblel-line-p)
          (forward-line -1)
          (back-to-indentation))
        ;; TODO: just evaluating a single lisp function, want more and
        ;; want to check error before nuking current table
        (setq lisp-table (cic:org-table-to-lisp-no-separators))
        ;; XXXX: found it essential to send copy-tree of lisp-table to
        ;; function, stops many subtle bugs
        (setq new-lisp-table (funcall (intern lisp-function) (copy-tree lisp-table)))
        ;; XXXX: make sure nil does not erase table
        (when new-lisp-table
          ;; finally put it back if all is well
          (cic:org-table-elisp-replace lisp-table new-lisp-table)
          ;; TODO: option to avoid this?
          (org-table-align))))
    t))

(defun cic:org-table-tblel-ctrl-c-recalc (orig-fun &rest args)
  "Function to advise ctrl-c and org-table evaluate functions."
  ;; kill ctrl-c-ctrol-c and just do our own thing until TBLEL==keyword is fixed
  (when (cic:org-table-tblel-line-p)
    (funcall orig-fun args)))

;; add some advice to intercept org-table functions and commands
(advice-add 'org-table-recalculate :before #'cic:org-table-eval-tblel)
(add-hook 'org-ctrl-c-ctrl-c-hook 'cic:org-table-eval-tblel-line)

(defun cic:org-table-tblel-setup ()
  "Set up some things so tblel is as integrated as possible in org-table."
  ;; set up fontification
  (font-lock-add-keywords 'org-mode
                          ;; TODO: change to org-meta-line, in keyword-face for convienience right now
                          '(("^\\s-+\\(#\\+TBLEL:.*\\)$" . font-lock-comment-face))))


(add-hook 'org-mode-hook 'cic:org-table-tblel-setup)

;; a nice generic sum function, sum all sumable solumns
;; TODO: get a table with seperators!!!!
(defun tblel-generic-sum (lisp-table)
  "Sum any column that is summable to the line at the end, and
excluding a header."
  (let ((sums (make-list (length (car lisp-table)) nil))
        (count 0)
        tmp-lisp-table)
    (dolist (row (cdr (butlast lisp-table)))
      (setq count 0)
      (dolist (e row)
        (when (or (cic:string-float-p (elt row count))
                  (cic:string-integer-p (elt row count)))
          (unless (elt sums count)
            (setcar (nthcdr count sums) 0))
          (setcar (nthcdr count sums) (+ (elt sums count) (string-to-number (elt row count)))))
        (setq count (+ count 1))))
    ;; now just insert it in last thing
    (setq tmp-lisp-table (butlast lisp-table))
    (append tmp-lisp-table (list (mapcar (lambda (e) (ignore-errors (number-to-string e))) sums)))))

(defun tblel-generic-sum-quantity (lisp-table)
  "Sums a quantity in second column with value in third column,
into the last row."
  (let ((sum nil)
        tmp-lisp-table
        last-row)
    (dolist (row (cdr (butlast lisp-table)))
      (when (and (or (cic:string-float-p (elt row 1))
                    (cic:string-integer-p (elt row 1)))
                 (or (cic:string-float-p (elt row 2))
                     (cic:string-integer-p (elt row 2))))
        (unless sum
          (setq sum 0))
        (setq sum (+ sum (* (string-to-number (elt row 1)) (string-to-number (elt row 2)))))))
    (setq tmp-lisp-table (butlast lisp-table))
    (setq last-row (car (last lisp-table)))
    (setcar (nthcdr 2 last-row) (ignore-errors (number-to-string sum)))
    (append tmp-lisp-table (list last-row))))

;; from https://stackoverflow.com/questions/6532898/is-there-a-apply-function-to-region-lines-in-emacs
(defun apply-function-to-region-lines (fn)
  "Apply a function FN to each line in the region."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (region-beginning)
      ;; loop over lines till region at end
      (goto-char (region-end))
      (let ((end-marker (copy-marker (point-marker)))
            next-line-marker)
        (goto-char (region-beginning))
        (if (not (bolp))
            (forward-line 1))
        (setq next-line-marker (point-marker))
        (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (funcall fn)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil)
      (setq deactivate-mark nil)))))

(defun cic:increase-indent ()
  "Increase the indent of a line or a region if that is active."
  (interactive)
  (if (region-active-p)
      (apply-function-to-region-lines (lambda ()
                                        (beginning-of-line)
                                        (insert " ")))
    (save-excursion
      (beginning-of-line)
      (insert " "))))

(defun cic:decrease-indent ()
  "Decrease the indent of a line or a region if that is active."
  (interactive)
  (if (region-active-p)
      (apply-function-to-region-lines (lambda ()
                                        (beginning-of-line)
                                        (when (looking-at " ")
                                          (delete-char 1))))
    (save-excursion
      (beginning-of-line)
      (when (looking-at " ")
        (delete-char 1)))))

(defun cic:make-file-finder (f)
  "Make a command to find a particular file."
  (let ((sym (gensym)))
    (set sym f)
    `(lambda ()
       (interactive)
       (find-file ,sym))))

(defun cic:org-refile-region-or-subtree (destination-file)
  "Refile and indicate has been refiled."
  ;; TODO: do I want this to be more general than org-file
  ;; TODO: make sure destination file is an org-file
  ;; TODO: maybe just check if destination file is symbol
  (unless (or (not (eq major-mode 'org-mode)) (not destination-file) (eq destination-file 'cancel))
    (let (region-to-move
          (beg-of-region (region-beginning))
          (end-of-region (region-end))
          (current-filename (cic:current-grouppath (buffer-file-name)))
          new-subtree
          (check-input-char 13))
      ;; ask about location
      ;; TODO: pop-up showing things to be moved
      (save-window-excursion
        (cic:find-file-meta destination-file)
        (goto-char (point-max))
        (setq check-input-char (read-char "Refile to here (<enter> accepts and any other key cancels): "))
        (message nil))
      (message nil)
      (when (equal check-input-char 13)
        (when (not (region-active-p))
          (org-mark-subtree)
          (setq beg-of-region (region-beginning))
          (setq end-of-region (region-end)))
        (setq region-to-move (buffer-substring beg-of-region end-of-region))
        (delete-region beg-of-region end-of-region)
        (with-current-file destination-file
          ;; is what I've cut a subtree
          (with-temp-buffer
            (org-mode)
            ;; trim whitespace
            (insert (chomp region-to-move))
            ;; add subtree to the end with tag
            (goto-char (point-min))
            (if (cic:org-headline-p (cic:get-current-line))
                (progn
                  ;; cut headline to level 1
                  (dotimes (count (- (org-outline-level) 1))
                    (org-promote-subtree))
                  ;; add tag to headline
                  (when (not (string-match ":refiled:" (cic:get-current-line)))
                    (end-of-line)
                    (insert (concat " (from " current-filename ") :refiled:"))))
              (progn
                ;; add headline
                (goto-char (point-min))
                (while (not (eobp))
                  (beginning-of-line)
                  (insert "  ")
                  (forward-line))
                (goto-char (point-min))
                (insert (concat "* (from " current-filename ") :refiled:\n"))))
            (setq new-subtree (buffer-substring (point-min) (point-max))))
          (with-current-file destination-file
            (goto-char (point-max))
            (when (not (= (current-column) 0))
              (insert "\n"))
            (insert new-subtree))))))
  (when (not (eq major-mode 'org-mode))
    (message "Refile only works in org-mode!")))

(defun cic:org-refile-subtree-preserve-structure (destination-file)
  "Refile and preserve structure."
  (unless (or (not (eq major-mode 'org-mode)) (not destination-file) (eq destination-file 'cancel))
    (cond ((region-active-p)
           (message "Cannot refile while region is active!!!"))
          ((/= (org-outline-level) 2)
           (message "Must be at level 2 heading to refile!!!"))
          (t
           (let (new-subtree
                 (current-filename (cic:current-grouppath (buffer-file-name)))
                 (current-toplevel-tree (save-excursion (cic:goto-previous-heading-level 1)
                                                        (org-back-to-heading)
                                                        (cic:strip-full-no-properties (org-get-heading))))
                 (check-input-char 13))
             ;; avoid if region is active
             (save-window-excursion
               (cic:find-file-meta destination-file)
               ;; find same heading
               (goto-char (point-min))
               (if (ignore-errors (cic:org-find-headline current-toplevel-tree))
                   (goto-char (org-end-of-subtree))
                 (goto-char (point-max)))
               ;; if headline does not exist, just go to end
               (setq check-input-char (read-char "Refile to here (<enter> accepts and any other key cancels): "))
               (message nil))
             (when (equal check-input-char 13)
               (org-mark-subtree)
               (setq beg-of-region (region-beginning))
               (setq end-of-region (region-end))
               (setq region-to-move (buffer-substring beg-of-region end-of-region))
               (delete-region beg-of-region end-of-region)
               (with-current-file (cic:get-filename-meta destination-file)
                 (goto-char (point-min))
                 (if (ignore-errors (cic:org-find-headline current-toplevel-tree))
                     (goto-char (org-end-of-subtree))
                   (progn
                     (goto-char (point-max))
                     (insert (concat "* " current-toplevel-tree))))
                 ;; go to the end of the tree
                 (insert "\n")
                 ;; now insert it
                 (insert region-to-move)
                 (cic:org-kill-trailing-blank-lines)))
             ;; TODO: save-buffers once this is a stable function
             )))))

;; TODO: add more cases at some point, start putting more places
(defun cic:org-kill-trailing-blank-lines ()
  "Kill trailing blanks at end of trees and after :END:"
  ;; XXXX: lightly tested, be sure to diff before committing for now
  (interactive)
  (let ((line-found nil))
   (when (eq major-mode 'org-mode)
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         (forward-line)
         (cond ((string-match "^\\s-*$" (cic:get-current-line))
                (setq line-found nil)
                (save-excursion
                  (forward-line)
                  (when (and (not (eobp)) (string-match "^\\*" (cic:get-current-line)))
                    (setq line-found t)))
                (when line-found
                  (beginning-of-line)
                  (kill-line)))
               ((string-match ":END:" (cic:get-current-line))
                (setq line-found nil)
                (save-excursion
                  (forward-line)
                  (when (and (not (eobp)) (string-match "^\\s-*$" (cic:get-current-line)))
                    (beginning-of-line)
                    (kill-line))))))))))

;; https://stackoverflow.com/questions/4870704/appending-characters-to-the-end-of-each-line-in-emacs
;; create key for this and clean up language
(defun add-string-to-end-of-lines-in-region (str b e)
  "prompt for string, add it to end of lines in the region"
  (interactive "sWhat shall we append? \nr")
  (goto-char e)
  (forward-line -1)
  (while (> (point) b)
    (end-of-line)
    (insert str)
    (forward-line -1)))

(provide 'emacs-stdlib-functions)
