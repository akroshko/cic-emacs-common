;;; cic-emacs-functions.el --- Common emacs functions that should have
;;; many uses.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20190508
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
;; These are relatively generic functions, often the extensions of
;; ones in common packages.
;;
;; Non-default features that are required by this library:
;;
;;; Code:

(defun cic:mpp (value &optional buffer)
  "Pretty print an object in a particular buffer (default
*PPCapture*). Include a seperator and time-stamp in the
destination buffer."
  ;; TODO: flag to not use timestamp
  (let ((message-string (concat "-- " (cic:time-stamp-verbose) "\n" (with-output-to-string (princ value)))))
    (unless buffer
      (setq buffer (get-buffer-create "*PPCapture*")))
    (with-current-buffer-max (get-buffer-create buffer)
                             (insert (concat message-string "\n")))))

(defun cic:mpp-list (value &optional buffer)
  "Pretty print a list to a particular buffer (default
*PPCapture*).  Include a seperator and time-stamp in the
destination buffer."
  ;; TODO: flag to not use timestamp
  (let ((message-string (concat "-- " (cic:time-stamp-verbose) "\n")))
    (unless buffer
      (setq buffer (get-buffer-create "*PPCapture*")))
    (with-current-buffer-max (get-buffer-create buffer)
                             (insert (concat message-string "\n"))
                             (dolist (e value)
                               (insert (with-output-to-string (princ e)))
                               ;; this is extreme for a seperator but it prevents confusion between list elements
                               (insert "\n----------------------------------------\n")))))

(defun cic:mpp-echo (value &optional buffer)
  "Pretty print an object to a particular buffer (default
*PPCapture*). Include a seperator and time-stamp in the
destination buffer.  Echo the represenation of this object in
minibuffer area as well."
  (let* ((raw-message-string (with-output-to-string (princ value)))
         (message-string (concat (cic:time-stamp-verbose) "\n" raw-message-string)))
    (unless buffer
      (setq buffer (get-buffer-create "*PPCapture*")))
    (with-current-buffer-max (get-buffer-create buffer)
                             (insert (concat message-string "\n")))
    (message raw-message-string)))

;; (mpp-table '(("1" "2" "3") ("1" "2" "3")))
(defun mpp-table (obj)
  "Pretty print a list of lists as an org-table in a particular
buffer (default *PPCapture*). Include a seperator and time-stamp
in the destination buffer."
  (with-temp-buffer
    (dolist (row obj)
      (dolist (column row)
        (insert (concat "| "
                        (pp-to-string column)
                        " ")))
      (insert "|\n"))
    (point-min)
    (org-table-align)
    (mpp (buffer-string))))

(unless (fboundp 'cic:find-file-meta)
  (defun cic:find-file-meta (arg filename &optional wildcards)
    "A find-file function for FILENAME that is often replaced
with something else in some of my Emacs configurations.  ARG as
'(4) indicates whether a new frame is created and ARG as '(16)
creates a new frame on a different monitor based on the
create-frame-other-window-maximized function."
    (cond ((null arg)
           (find-file filename wildcards))
          ((equal arg '(4))
           (create-frame-here)
           (find-file filename wildcards))
          ((equal arg '(16))
           (create-frame-other-window-maximized)
           (find-file filename wildcards)))))

(defun cic:time-stamp-verbose ()
  "Create a time stamp string that is easily human readable."
  (format-time-string "%H:%M:%S" (current-time)))

(defun subseq-short (seq start &optional end)
  "Take subsequence froma sequence that is possibly too short."
  (let ((length-seq (length seq)))
    (if (< length-seq end)
        (subseq seq start length-seq)
      (subseq seq start end))))

;; TODO: there is a directory-list-recursively (or similar...)
;; http://www.emacswiki.org/emacs/ElispCookbook#toc59
(defun cic:walk-path (dir action)
  "Walk DIR executing ACTION with arugments (dir file)"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst)
                   lst  (cdr lst))
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

(defun cic:assoc-nth (nth-value key-value assoc-list &optional equal-test)
  "Get the NTH-VALUE item from the KEY-VALUE item in ASSOC-LIST
with an optional EQUAL-TEST that defaults to the Emacs Lisp
function equal."
  (unless equal-test
    (setq equal-test 'equal))
  (let (selected (zeron (- nth-value 1)))
    (dolist (item assoc-list)
      (when (funcall equal-test key-value (nth zeron item))
        (setq selected item)))
    selected))

(defun cic:get-headline-text (headline-line)
  "Get only the clean headline text from HEADLINE-LINE
representing the text of a line the headline is on."
  (let (headline-text)
    (when (string-match cic:headline-regexp headline-line)
      (setq headline-text (match-string 1 headline-line)))))

(defun cic:org-headline-p (line-substring)
  "Test if LINE-SUBSTRING is an org-mode headline."
  (string-match cic:headline-regexp line-substring))

(defun cic:org-list-p (line-substring)
  "Test if LINE-SUBSTRING is an org-mode list item."
  (string-match cic:list-regexp line-substring))

(defun cic:org-plain-list-p (line-substring)
  "Test if LINE-SUBSTRING is a plain (no checkboxes) org-mode
list item."
  (and
   (string-match cic:list-regexp line-substring)
   (not (string-match cic:checkbox-regexp line-substring))))

(defun cic:org-checkbox-p (line-substring)
  "Test if LINE-SUBSTRING an org-mode checkbox item."
  (string-match cic:checkbox-regexp line-substring))

(defun cic:org-check-last-heading-level-1 ()
  "Check if we are at the last heading of level 1 in the curren buffer.
This is generally used to see if a loop over level 1 headings can
keep going."
  (let ((line-no (line-number-at-pos)))
    (save-excursion
      (org-forward-heading-same-level 1 nil)
      (if (equal line-no (line-number-at-pos))
          nil
        t))))

(defun cic:zip (&rest streams)
  "Zip function like that in many programming languages."
  (apply #'mapcar* #'list streams))

(defun cic:ensure-list (object)
  "Put OBJECT into a list if it is not already a list."
  (unless (listp object)
    (setq object (list object)))
  object)

(defun cic:car-only (lst)
  "Ensure a list is only length 1 and get the car.  Raise an
error is list is longer than length 1."
  (when (> (length lst) 1)
    (error "List must be length 1 only!"))
  (car lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file helpers

(defun cic:list-files (path)
  "List the files in PATH."
  (remove-if-not (lambda (e)
                   (not (file-directory-p (cic:join-paths path e))))
                     (directory-files path)))

;; TODO: option to take out dot directories?
(defun cic:list-directories (path)
  "List the directories in PATH.  Gets rid of . and .."
  (let ((the-dirs (remove-if-not (lambda (e)
                                       (file-directory-p (cic:join-paths path e)))
                                 (directory-files path))))
    ;; TODO: do one operation using pattern matching
    (cl-remove ".." (cl-remove "." the-dirs :test #'string=) :test #'string=)))

(defun cic:find-file-goto-line (filename &optional line)
  "Find FILENAME and goto the LINE."
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

(defun cic:get-current-line ()
  "Get the current line as a string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;; (cic:select-list-item (list "item1" "item2"))
(defun cic:select-list-item (lst &optional string-key header-message)
  "Select a string from a list LIST of objects (strings if
STRING-KEY is not specified) using alphabet and then single-digit
number.  STRING-KEY is a function that selects a string for
display from the list of times."
  (let* ((count "a")
         (index-count 0)
         (cancel 'cancel)
         (select-alist (mapcar (lambda (e)
                                 (let (the-list)
                                   (if string-key
                                       (setq thelist (list count (funcall string-key e) index-count))
                                     (setq thelist (list count e e)))
                                   (setq count       (increment-char count)
                                         index-count (1+ index-count))
                                   thelist))
                               lst)))
    (cic:select-alist select-alist
                      header-message)))

(defun cic:get-list-duplicates (lst)
  "Get the duplicate items in list LST."
  (set-difference lst (remove-duplicates lst :test 'equal)))

(defun count-indentation (&optional current-line)
  "Count the indentation level in CURRENT-LINE, if nil use the
current line at point."
  (unless current-line
    (setq current-line (cic:get-current-line)))
  (- (length current-line) (length (s-trim-left current-line))))

(defun cic:list-from-file (filename)
  "Get a list of strings based on the lines in FILENAME."
  (remove-if-not 'cic:is-not-empty-string-nil
                 (with-temp-buffer
                   (insert-file-contents filename)
                   (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filesystem helper functions

;; TODO: normalize paths...
(defun cic:find-file-upwards (file-to-find ;; &optional min-level-below-home
                                           )
  "Recursively searches each parent directory starting from the default-directory,
looking for a file with name FILE-TO-FIND.  Returns the path to
FILE-TO-FIND or nil if not found."
;; MIN-LEVEL-BELOW-HOME makes sure it does not go above this level below home.
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (cic:join-paths parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file)
                        ;; found
                        possible-file)
                       ((or (null parent) (equal parent (directory-file-name parent)))
                        ;; The parent of ~ is nil and the parent of / is itself.
                        ;; Thus the terminating condition for not finding the file
                        ;; accounts for both.
                        ;; i.e., not found
                        nil)
                       ;; ((and min-level-below-home (not (string-match (concat (expand-file-name "~") ) )))
                       ;;  nil)
                       (t
                        ;; continue upwards
                        (find-file-r (directory-file-name parent)))))))
    (find-file-r default-directory)))

;; TODO: normalize paths...
(defun cic:join-paths (&rest args)
  "Join paths in elisp. Only works with two arguments for now!"
  ;; TODO: enhance to work with arbitrary arguments
  (concat (cic:trim-trailing-slash (file-name-as-directory (car args))) "/" (cadr args)))

(defun cic:goto-location (location)
  "Goes to a location in the form '(FILENAME POSITION) or
'(BUFFER POSITION). Completely unfolds org-mode when going to an
org-mode file or buffer."
  ;; TODO be less severe about opening org-mode, maybe an org-show-context
  (if (stringp (car location))
      (progn
        (find-file (car location))
        (when (derived-mode-p 'org-mode)
          (org-show-all))
        (goto-char (cadr location)))
    (progn
      (switch-to-buffer (car location))
      (when (derived-mode-p 'org-mode)
        (org-show-all))
      (goto-char (cadr location)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thing at point bounds

(defun cic:region-or-thing-at-point-no-properties (thing)
  "Select either a region if active or get the thing-at-point
THING instead.  Make sure returned string has no properties."
  (let ((current-thing (cic:region-or-thing-at-point thing)))
    (if current-thing
        (substring-no-properties current-thing)
      nil)))

(defun cic:region-or-thing-at-point (thing)
  "Select either a region if active or the thing-at-point THING
instead."
  (let (selected-text)
    (if (region-active-p)
        (setq selected-text (buffer-substring (mark) (point)))
      (setq selected-text (thing-at-point thing)))
    selected-text))

(when nil
  (cic:select-alist '(("g" . ("galaxy" (list "file://galaxy" "test") ))
                      ("h" . ("help" "file://help")))
                    "Test: "))
(defun cic:select-alist (the-alist &optional header-message)
  "Allows user to select from an alist. Each entry in THE-ALIST
  must be in the format:

  (<<LENGTH ONE STRING>> (<<DESCRIPTION>> <<STRING OR LIST TO RETURN ON SELECTION>>))

  If non-nil HEADER-MESSAGE is a string that gives a message for the minibuffer to
  guide the selection.  If HEADER-MESSAGE is nil this message in the minibuffer is
  omitted."
  ;; TODO: I don't actually use the nested function and I think this can be greatly simplified.
  (interactive)
  ;; get keys until alist is exhausted
  (let ((minibuffer-prompt "")
        key-press
        canceled
        selected
        goback
        minibuffer-line
        minibuffer-select
        minibuffer-cancel
        (count 0))
    ;; initialise variables
    (if header-message
        (setq minibuffer-prompt (concat header-message "\n") )
      (setq minibuffer-prompt ""))
    ;; build minibuffer
    (dolist (search-key the-alist)
      (setq minibuffer-select (concat "(" (car search-key) ")"))
      ;; TODO: this needs a bit more work to really make it look good
      ;; (put-text-property 0 (length minibuffer-select) 'face 'bold minibuffer-select)
      (setq minibuffer-line (concat minibuffer-select " " (cadr search-key)))
      ;; TODO: I like these colors for now, although something a bit more subtle is fine
      (if (= (mod count 2) 1)
          (put-text-property 0 (length minibuffer-line) 'face '(:background "light gray") minibuffer-line)
        (put-text-property 0 (length minibuffer-line) 'face '(:background "light sky blue") minibuffer-line))
      (setq minibuffer-prompt (concat minibuffer-prompt
                                      (concat minibuffer-line "\n")))
      (setq count (1+ count)))
    (setq minibuffer-cancel "(-) cancel")
    ;; TODO: will need to define face
    ;; :bold t
    ;; '(:background "dark gray" :foreground "red")
    (put-text-property 0 (length minibuffer-cancel) 'face '(:background "khaki2") minibuffer-cancel)
    (setq minibuffer-prompt (concat minibuffer-prompt minibuffer-cancel))
    ;; read a key
    (setq key-press (read-key minibuffer-prompt))
    (cond ((equal key-press 67108914)
           (setq goback t)
           '(goback))
          (t
           (when (or (equal key-press 45) (equal key-press 3) (equal key-press 7))
             (setq canceled t))
           (setq key-press       (make-string 1 key-press)
                 ;; test for valid keypress, inner-selection is nil if not valid
                 inner-selection (assoc key-press the-alist))
           ;; use input, decide if alist is in fact an inner alist
           (setq selected (caddr inner-selection))
           (when (and selected (not canceled))
             (if (symbolp selected)
                 (symbol-value selected)
               selected))))))

(defun cic:browse-url-conkeror (url &rest args)
  "Browse a url in the Conkeror web browser."
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic url)))

(defun cic:make-file-finder (f)
  "Make a command to find a particular file."
  (lexical-let ((sym (gensym)))
    (setq sym f)
    `(lambda ()
       (interactive)
       (find-file ,sym))))

(defun cic:org-goto-previous-heading-level (level)
  "Go to the previous heading of LEVEL.  Fail nicely if this
makes no sense."
  ;; do nothing when level is one
  (unless (and (equal level 1) (equal (org-outline-level) 1))
    ;; goto the open heading level
    (org-back-to-heading)
    (beginning-of-line)
    (setq heading-level
          (org-outline-level))
    (setq move-heading-level
          (- heading-level
             level))
    (when (< move-heading-level 0)
      (setq move-heading-level 0))
    (org-up-heading-all move-heading-level)))

(defun cic:current-compile-full (&optional arg)
  "Full compile in one command (eventually split off helper
functions)."
  (interactive "P")
  (if arg
      (cic:current-compile '(64))
    (cic:current-compile '(16))))

;; TODO: make this work with other modes
(defun cic:current-compile (&optional arg)
  "Compile the current project nice and quick.  Meant to be run
fairly often while developing code or writing in LaTeX."
  (interactive "P")
  (cond ((derived-mode-p 'latex-mode)
         (cond ((equal arg '(4))
                (let ((full-filename buffer-file-name))
                           ;; recursive call to compile
                  (cic:current-compile nil)
                  (let ((active-process (with-current-file-transient full-filename
                                          (TeX-active-process))))
                    (when active-process
                      (set-process-sentinel active-process 'first-latex-compile-process-sentinel)))))
               ((equal arg '(16))
                (cic:current-compile 'full))
               ((equal arg '(64))
                (let ((full-filename buffer-file-name))
                  ;; recursive call to compile
                  (cic:current-compile 'full)
                  (let ((active-process (with-current-file-transient full-filename
                                          (TeX-active-process))))
                    (when active-process
                      (set-process-sentinel active-process 'first-latex-full-compile-process-sentinel)))))
               ((equal arg 'full)
                ;; TODO: this need to be fixed
                (with-current-file-transient "~/cic-vcs-academic/phdthesis/includeonly.tex"
                  (erase-buffer)
                  (basic-save-buffer))
                (TeX-command "LaTeX" 'TeX-master-file nil))
               (t
                (save-some-buffers t)
                ;; get includeonly working
                (when (or (string-match "thesis" (buffer-name)) (string-match "chapter" (buffer-name)))
                  ;; TODO: this need to be fixed
                  (with-current-file-transient "~/cic-vcs-academic/phdthesis/includeonly.tex"
                    (erase-buffer)
                    (insert (concat "\\includeonly{" (file-name-base buffer-file-name) "}\n"))
                    (basic-save-buffer)))
                (TeX-command "LaTeX" 'TeX-master-file nil))))
        ((derived-mode-p 'python-mode)
         ;; XXXX: not using pyflakes because can't easily ignore some common design decision I make in Python code
         ;; TODO: forget what this refers to
         ;; TODO: better customization of error codes
         ;; https://pycodestyle.readthedocs.io/en/latest/intro.html#error-codes
         ;; http://flake8.pycqa.org/en/latest/user/error-codes.html
         ;; TODO: have a production mode
         ;; XXXX: these reflect common stylistic changes I've made for research-focused code,
         ;;       e.g., from <<module>> import * is incredibly useful when experimenting with many functions reflecting many math problems,
         ;;             enforcing space after comma is not always best style for huge lists of numbers,
         ;;             long lines are good for long formulas,
         ;;             multiple spaces after commas are good for complex data structures,
         ;;             sometimes add paths is good before importing on experimental setups,
         ;;             requiring whitespace around operators does not allows intuitive grouping of expressions
         ;;             I like to comment blocks out so wrong and unexpected indentation will always be found
         (python-check (concat "flake8 --ignore=E114,E116,E122,E124,E127,E201,E221,E222,E225,E226,E231,E241,E251,E261,E266,E302,E305,E401,F401,E402,F402,E403,F403,E405,F405,E501 " buffer-file-name)))))

;; TODO: make this work with other modes
(defun cic:current-clean ()
  "Regulariliy clean the current project.  Meant to be run fairly
often while developing code or writing in LaTeX."
  (interactive)
  (cond ((derived-mode-p 'latex-mode)
         (TeX-command "Clean All" 'TeX-master-file nil)
         ;; this lets me see message from "Clean All" before my own success message
         (sit-for 0.5)
         (message "Cleaned!!!"))))

;; TODO: make this a bit more robust and universal
;; TODO: have a fallback for non-graphical displays
(defun create-frame-other-window-maximized (&optional frame-name)
  "Create a maximized frame called FRAME-NAME in the other
window (my secondary screen has the 0 0 coordinate)."
  (interactive)
  (select-frame (make-frame-command))
  (set-frame-position (selected-frame) 0 0)
  (cic:x-force-maximized))

;; TODO: make this a bit more robust and universal
;; TODO: have a fallback for non-graphical displays
(defun create-frame-other-window-maximized-focus (&optional frame-name)
  "Create a maximized frame called FRAME-NAME in the other
window (my secondary screen has the 0 0 coordinate)."
  (interactive)
  (select-frame-set-input-focus (make-frame-command))
  (set-frame-position (selected-frame) 0 0)
  (cic:x-force-maximized))

;; TODO: need to test this on non-graphical display
(defun create-frame-here (&optional frame-name)
  "Create a maximized frame called FRAME-NAME in the default
location.  Handles case where the display is non-graphical."
  (interactive)
  (let (new-frame)
    (cond (frame-name
           (if (display-graphic-p)
               (setq new-frame (make-frame (list (cons 'name frame-name))))
             (progn
               (setq new-frame (make-frame (list (cons name frame-name))))
               (select-frame new-frame))))
          (t
           (setq new-frame (make-frame-command))
           (select-frame new-frame)))
    (cic:x-force-maximized)
    new-frame))

(defun cic:toggle-hl-line-mode-and-visuals (&optional arg)
  "Toggle any visual aids, such as hl-line-mode.  Fix the cursor
to my preferred default."
  (interactive "P")
  (if arg
      (progn
        (set-cursor-color "#ffffff")
        (blink-cursor-mode -1))
    (call-interactively 'hl-line-mode)))

(defun cic:create-or-select-frame-displaying-buffer (my-buffer)
  "Create MY-BUFFER or select a frame displaying MY-BUFFER."
  (cond ((get-buffer-window my-buffer t)
         ;; raise frame, then focus
         (raise-frame (window-frame (get-buffer-window my-buffer t)))
         (select-window (get-buffer-window my-buffer t))
         (switch-to-buffer my-buffer))
        (t
         (create-frame-other-window-maximized)
         (switch-to-buffer my-buffer))))

(defun cic:select-frame-displaying-buffer (my-buffer)
  "select a frame displaying MY-BUFFER.  Fail silently if
MY-buffer does not exist."
  (when (get-buffer-window my-buffer t)
    ;; raise frame, then focus
    (raise-frame (window-frame (get-buffer-window my-buffer t)))
    (select-window (get-buffer-window my-buffer t))
    (switch-to-buffer my-buffer)))

(defun cic:other-window-next ()
  "Page down in the other window."
  (interactive)
  (let ((current-window (selected-window)))
    (other-window 1)
    ;; TODO: better way to detect end of buffer, eobp
    (ignore-errors (scroll-up))
    (select-window current-window)))

(defun cic:other-window-previous ()
  "Page up in the other window."
  (interactive)
  (let ((current-window (selected-window)))
    (other-window 1)
    ;; TODO: better way to detect end of buffer, eobp
    (ignore-errors (scroll-down))
    (select-window current-window)))

;; TODO: do not really use this right now but it might be useful for something
(defun cic:switch-buffer-new-window-below ()
  "Create new window below and allow selection of a new buffer."
  ;; TODO: unwind if not successful or if I cancel ido-switch-buffer
  (interactive)
  (split-window-vertically)
  (windmove-down)
  (ido-switch-buffer))

(defun cic:count-words-region-or-buffer ()
  "Count works in a region or just count in the whole buffer if
there is no active region."
  (interactive)
  (if (region-active-p)
      (call-interactively 'count-words-region)
    (save-excursion
      (mark-whole-buffer)
      (call-interactively 'count-words-region))))

(defun cic:kill-region-only-active (beg end &optional region)
  "Kill a region only if it is active."
  ;; TODO: do something else (like copy whole line) if no region?
  (interactive (list (mark) (point) 'region))
  (if (region-active-p)
      (kill-region beg end region)
    (message "Cannot delete unless there is an active region!")))

(defun cic:page-up ()
  "A nicer page-up command than the default.  Also avoids Emacs'
confusing scroll-up/down convention."
  ;; https://www.emacswiki.org/emacs/Scrolling#toc3
  ;; TODO get rid of flashing and instead smoothly go
  (interactive)
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun cic:page-down ()
    "A nicer page-down command than the default.  Also avoids
Emacs' confusing scroll-up/down convention."
  ;; TODO: in progress
  ;; https://www.emacswiki.org/emacs/Scrolling#toc3
  ;; restore cursor.... half pgup/down?
  (interactive)
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun cic:move-up ()
  "Moves viewport up by one line.  A nice command to bind to the
arrow keys for browsing code."
  (interactive)
  ;; TODO: this might still need work to be what I want
  ;; this combination is faster than letting (scroll-preserve-screen-position 'other)
  (let ((saved-position (point)))
    (ignore-errors (forward-line -1))
    ;; if forward-line moves point
    (when (/= (point) saved-position)
      ;; condition-case nil
      (ignore-errors (scroll-down 1))
      ;; (beginning-of-buffer (forward-line))
      )))

(defun cic:move-down ()
  "Moves viewport down by one line.  A nice command to bind to
the arrow keys for browsing code."
  (interactive)
    ;; TODO: this might still need work to be what I want
  ;; this combination is faster than letting (scroll-preserve-screen-position 'other)
  ;; TODO: not sure some of these bindings are necessary
  (let ((saved-position (point))
        (scroll-margin 0)
        (scroll-conservatively 10000)
        (next-screen-context-lines 0))
    (forward-line)
    ;; if forward-line moves point
    (when (/= (point) saved-position)
      (scroll-up 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture from an external source
;; these are often called from the command line
;; DO NOT DELETE BECAUSE THEY SEEM UNUSED!!!

(defun cic:capture-conkeror-buffer (title capture-text)
  "This is called from conkeror to copy the text in a buffer into
a temporary buffer in Emacs."
  ;; TODO: assist in formating a bit better because html is stupid
  (let ((decoded-title (base64-decode-string title))
        (decoded-temp-filename (base64-decode-string capture-text)))
    ;; TODO: need safe titles and add datestring
    ;; TODO: should I redo the buffer?
    (with-current-buffer-create (concat "capture-conkeror-" (format-time-string "%Y%m%d%H%M%S" (current-time)) "-" decoded-title)
      (insert-file-contents decoded-temp-filename)
      (goto-char (point-min)))))

(defun cic:capture-rxvt-scrollback (tmp-file)
  "This is called from rxvt to copy the scrollback text into a
temporary buffer in Emacs."
  (let ((captured-scrollback (with-current-file-transient tmp-file
                               (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer-create (concat "capture-urxvt-scrollback-" (format-time-string "%Y%m%d%H%M%S" (current-time)))
      (insert captured-scrollback)
      (goto-char (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apt
(defun cic:apt-show ()
  "Run apt-cache show command and show results in temporary
buffer."
  (interactive)
  ;; TODO: need to add . to this
  (let* ((the-symbol (thing-at-point 'symbol))
         ;; TODO: test for invalid packages
         (apt-show-output (shell-command-to-string (concat "apt-cache show " the-symbol)))
         (current-window (get-buffer-window))
         (the-buffer (pop-to-buffer "*apt show*")))
    (with-current-buffer-erase the-buffer
      (insert apt-show-output)
      (goto-char (point-min)))
    (select-window current-window)))

(defun cic:dired-filename-at-point ()
  (expand-file-name (dired-file-name-at-point)))

(defun cic:datestamp-current-time (&optional the-time)
  "Get a datestamp with the current time or optionally THE-TIME."
  (unless the-time
    (setq the-time (current-time)))
  (format-time-string "%Y%m%dt%H%M%S" the-time))

(defun cic:datestamp-current-time-short (&optional the-time)
  "Get a datestamp with just the current date or optionally the
date at THE-TIME."
  (unless the-time
    (setq the-time (current-time)))
  (format-time-string "%Y%m%d" the-time))

(defun cic:kill-line-elisp ()
  "Kill a line in elisp without modifying kill-ring or
interprogram-paste.  Ignore any custom kill-whole-line setting."
  (let (kill-ring
        kill-whole-line
        kill-ring-yank-pointer
        (save-interprogram-paste-before-kill nil)
        (interprogram-cut-function nil))
    (kill-line)))

(defun cic:kill-whole-line-elisp ()
  "Kill a whole line in elisp without modifying kill-ring or
interprogram-paste.  Ignore any custom kill-whole-line setting to
make sure whole line is killed."
  (let (kill-ring
        (kill-whole-line t)
        kill-ring-yank-pointer
        (save-interprogram-paste-before-kill nil)
        (interprogram-cut-function nil))
    (kill-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse commands

(defun cic:browse-url-at-point-conkeror ()
  "Find the URL at point and open in the conkeror web browser."
  (interactive)
  (let ((browse-url-generic-program "conkeror"))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-firefox ()
  "Find the URL at point and open in the Firefox web browser."
  (interactive)
  (let ((browse-url-generic-program "firefox"))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-chromium ()
  "Find the URL at point and open in the Chromium web browser."
  (interactive)
  (let ((browse-url-generic-program "chromium")
        (browse-url-generic-args    '("--temp-profile")))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-google-chrome ()
  "Find the URL at point and open in the Google Chrome web
browser."
  (interactive)
  (let ((browse-url-generic-program "google-chrome")
        (browse-url-generic-args    '("--temp-profile")))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-pale-moon ()
  "Find the URL at point and open in the Pale Moon web browser."
  (interactive)
  (let ((browse-url-generic-program "palemoon"))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-waterfox ()
  "Find the URL at point and open in the Waterfox web browser."
  (interactive)
  (let ((browse-url-generic-program "waterfox"))
    (browse-url-generic (cic:url-at-point-or-line (cic:get-current-line)))))

(defun cic:browse-url-at-point-w3m ()
  "Find the URL at point and open in the w3m web browser."
  (interactive)
  (w3m-browse-url (cic:url-at-point-or-line (cic:get-current-line))))

(defun cic:url-at-point-or-line (&optional current-line)
  "If CURRENT-LINE is nil, find the URL at point and return it.
Find the url in the string CURRENT-LINE if specified."
  (let ((url (cic:trim-uid-org-link (cic:trim-after-double-colon (thing-at-point 'url)))))
    (unless url
      (unless current-line
        (setq current-line (cic:get-current-line)))
      (let ((url-start (string-match cic:url-regexp current-line)))
        (when url-start
          ;; this cleans up links that might use :: in them
          ;; TODO: not sure I use this in URLs anymore
          (setq url (cic:trim-after-double-colon (substring current-line url-start (match-end 0)))))))
    url))

(defun cic:next-file-dired (&optional motion)
  "Go to the next file from the current file, based on the order listed in dired.
Effect of non-nil MOTION is to go to previous file instead."
  (interactive)
  ;; TODO: fix this because when it gets to end of the dired buffer,
  ;;       shows the dired buffer
  (unless motion
    (setq motion 1))
  (let ((current-filename buffer-file-name)
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
                  (setq next-filename (dired-get-filename)
                        keep-going nil)))
            (error (setq keep-going nil))))))
    (when next-filename
      (find-file next-filename))))

(defun cic:previous-file-dired ()
  "Go to the previous file from the current file, based on the
order listed in dired."
  (interactive)
  (cic:next-file-dired -1))

(defun cic:next-file-dired-pagedown ()
  "Page down in the current file, then when at end go to the next
file based on the order listed in dired."
  (interactive)
  ;; TODO: test this out when it gets to the end of a dired buffer
  (unless (ignore-errors (or (scroll-up) t))
    (cic:next-file-dired)))

(defun cic:previous-file-dired-pageup ()
  "Page up in the current file, then when at beginning go to the
previous file from the current file as listed by dired."
  (interactive)
  ;; TODO: test this out when it gets to the beginning of a dired buffer
  (unless (ignore-errors (or (scroll-down) t))
    (cic:previous-file-dired)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixup commands
(defun cic:fix-whitespace (buffer)
  "This cleans up whitespace in file.  It automate the key
  sequence sequence M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'unix 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance commands
;; http://www.emacswiki.org/emacs/FullScreen
(defun cic:x-force-maximized (&optional f)
  "Force current window to be maximized.  Requires wmctrl to be
installed."
  (interactive)
  ;; TODO: sometimes called ffrom configuration files, so should handle errors well
  (shell-command "wmctrl -r :ACTIVE: -badd,maximized_vert,maximized_horz"))

(defun cic:toggle-fullscreen ()
  "Toggle current window as fullscreen.  Requires wmctrl to be
installed."
  (interactive)
    ;; TODO: sometimes called ffrom configuration files, so should handle errors well
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; taken from https://www.emacswiki.org/emacs/DeletingWhitespace#toc18
(defun cic:whack-whitespace (arg)
  "Delete all white space from point to the next word.  With
prefix ARG delete across newlines as well.  The only danger in
this is that you don't have to actually be at the end of a word
to make it work.  It skips over to the next whitespace and then
whacks it all to the next word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

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

;; based on https://emacs.stackexchange.com/questions/7548/sorting-words-with-hyphens-e-g-in-a-lisp-mode
(defun cic:sort-symbols (reverse beg end)
  "Sort a set of symbols in a region defined by the buffer
locations BEG END. REVERSE non-nil sorts in reverse order."
  (interactive "*P\nr")
  (let ((temp-table (copy-syntax-table text-mode-syntax-table)))
    (with-syntax-table temp-table
      (modify-syntax-entry ?. "_" temp-table)
      (sort-regexp-fields reverse "\\_<.*?\\_>" "\\&" beg end))))

(defun cic:copy-file-name-to-kill-ring (&optional arg)
  "Copy the current buffer file name to the kill ring.  With
non-nil ARG copy the full path to the kill ring."
  (interactive "P")
  (let (filename)
    (cond ((derived-mode-p 'Info-mode)
           (Info-copy-current-node-name))
          (t
           (if arg
               (setq filename buffer-file-name)
             (setq filename (if (derived-mode-p 'dired-mode)
                                (expand-file-name default-directory)
                              (file-name-nondirectory buffer-file-name))))
           (when filename
             (kill-new filename)
             (message "Copied buffer file name '%s' to the clipboard." filename))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs development
(defun cic:recalculate ()
  "Generic function to aggressively recalculate tables."
  (interactive)
  (org-table-recalculate '(16)))
(defun cic:elisp-eval-buffer ()
  "Evaluate the current buffer and indicate success."
  (interactive)
  ;; TODO: make sure we only evaluate elisp mode
  (eval-buffer)
  (message "Evaluated buffer."))
(defun cic:elisp-pp-capture-buffer ()
  "Switch to the *PPCapture* that is default destination for
cic:mpp commands."
  (interactive)
  (switch-to-buffer "*PPCapture*"))
(defun cic:elisp-messages-buffer ()
  "Switch to the *Messages* buffer."
  (switch-to-buffer "*Messages*"))
(defun cic:elisp-debug-on-error ()
  "Toggle debug-on-error on with a helpful message."
  (interactive)
  (funcall (cic:toggle-variable debug-on-error
                                "Debug on error enabled."
                                "Debug on error disabled.")))

(defconst cic:emacs-scratch-buffer-string
  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

"
  "I'm attached to the appearance of the default scratch buffer.
  Therefore I like it restored with the default string.")

(defun cic:elisp-scratch-buffer ()
  "Switch to the *scratch* buffer, recreating if necessary."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (cic:elisp-recreate-scratch-buffer)))
(defun cic:elisp-recreate-scratch-buffer ()
  "Recrate the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  ;; TODO: not sure I need this because I auto-mode paredit....
  (lisp-interaction-mode)
  (insert cic:emacs-scratch-buffer-string)
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil))

(defconst cic:info-jump-alist
  '(("0" "dir"                         ("dir"))
    ("a" "auctex"                      ("auctex"))
    ("b" "(bash) Top"                  ("(bash) Top"))
    ("c" "cl"                          ("cl"))
    ("d" "(cl) Loop Facility"          ("(cl) Loop Facility"))
    ("e" "emacs"                       ("emacs"))
    ("f" "elisp"                       ("elisp"))
    ("g" "(elisp) Regular Expressions" ("(elisp) Regular Expressions"))
    ("h" "org"                         ("org"))
    ("i" "sicp"                        ("sicp")))
  "An alist of info nodes.")
(defun cic:info-jump-select ()
  "Select an info node to jump to."
  (interactive)
  (let ((help-list (cic:select-alist cic:info-jump-alist)))
    (when help-list
      (info (car help-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cic:next-window-frame ()
  "Go to the window to the right or the other frame if this is
not possible."
  (interactive)
  (condition-case error-string
      (windmove-right)
    (error
     (other-frame 1))))
(defun cic:prev-window-frame ()
  "Go to the window to the left or the previous frame if this is
not possible."
  (interactive)
  (condition-case error-string
      (windmove-left)
    (error
     (other-frame -1))))
(defun cic:text-scale-neutral ()
  "Put the text scale back to neutral/default."
  (interactive)
  (text-scale-adjust 0))

(defconst cic:user-wordlist
  "~/.words"
  "A personal list of words that will generally not be flagged as
  mispelled.")

;; TODO: merge these two functions
(defun cic:wordlist-current-word ()
  "Add current word to user-defined wordlist."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (with-current-file-transient-max cic:user-wordlist
        (insert (concat "\n" word "\n"))
        (flush-lines "^\\s-*$" (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (basic-save-buffer))
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
      (with-current-file-transient-max cic:user-wordlist
        (insert (concat "\n" word "\n"))
        (flush-lines "^\\s-*$" (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (basic-save-buffer))
      (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
      (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
      (ispell-kill-ispell t)
      ;; (cic:flyspell-word)
      ;; reset word list
      (message (concat "Successfully added " word " to list!")))))

(defun cic:datestamp-current-time-clean (&optional the-current-time)
  (unless the-current-time
    (setq the-current-time (current-time)))
  (format-time-string "%Y%m%d%H%M%S" the-current-time))

(defun cic:datestamp-weekday-time (&optional the-current-time)
  (unless the-current-time
    (setq the-current-time (current-time)))
  (format-time-string "%a %b %d %H:%M:%S" the-current-time))

;; TODO: also insert just time
(defun cic:insert-date-time-stamp ()
  "Function to select and insert a date and/or time stamp."
  (interactive)
  (let* ((the-current-time (current-time))
         (the-list (list (list "c" (cic:datestamp-current-time-clean) (list (cic:datestamp-current-time-clean)))
                         (list "d" (cic:datestamp-current-time-short) (list (cic:datestamp-current-time-short)))
                         (list "t" (cic:datestamp-current-time)       (list (cic:datestamp-current-time)))
                         (list "w" (cic:datestamp-weekday-time)       (list (cic:datestamp-weekday-time)))))
         (the-time-string (cic:select-alist the-list)))
    (when the-time-string
      (insert (car the-time-string)))))

(defvar cic:insert-current-time-last-bounds
  nil
  "The bounds of the last insert of current time.")

(defvar cic:insert-current-time-last-type
  nil)

(defun cic:query-replace-case-sensitive ()
  "Case insensitive query-replace."
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively 'query-replace)))

(defvar cic:kill-transient-windows-undo
  nil
  "Undo info for when transient windows are killed.")

;; TODO: this function needs work, configuring things to delete/not-delete as constants is important
(defun cic:kill-transient-windows (&optional arg)
  "Delete all transient windows.  These are generally ones
visiting buffers that that have * at the beginning and end *.
This function also deletes frames that are considered transient.
This function is very much a work in progress."
  (interactive "P")
  ;; kill transient frames first
  (dolist (frame (frame-list))
    (let ((the-frame-name (substring-no-properties (cdr (assoc 'name (frame-parameters frame))))))
      (when (or (equal the-frame-name "attachment-find") (equal the-frame-name  "capture") (equal the-frame-name  "emms"))
        (delete-frame frame))))
  (if arg
      (and cic:kill-transient-windows-undo (set-window-configuration cic:kill-transient-windows-undo))
    ;; loop over window
    (let ((undo-info (current-window-configuration))
          window-deleted)
      (dolist (window (window-list))
        ;; get window name
        (let ((buffer-name (buffer-name (window-buffer window))))
          (when (or (and (starts-with buffer-name "*")
                         (ends-with   buffer-name "*")
                         (not (string-match "scratch" buffer-name)))
                    (starts-with buffer-name " *http"))
            (delete-window window)
            (setq window-deleted t))
          (when (starts-with buffer-name "*Python check:")
            (kill-buffer buffer-name))))
      (when window-deleted
        (setq cic:kill-transient-windows-undo undo-info)))))

(defun kill-ring-save-whole-word-or-region ()
  "Save a whole word to kill ring if region not marked, useful
for things like searching."
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

(defun cic:start-process-message (&rest args)
  "The start-process function, but display a message in the
minibuffer giving the command run.  ARGS are the arguments to
the start-process function."
  (let ((quoted-command-args (mapcar (lambda (e)
                                       (if (string-match " "  e)
                                           (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" e) "\"")
                                         e))
                                     (subseq args 2))))
    (message (mapconcat 'identity (append '("Running command:") quoted-command-args) " "))
    (apply 'start-process args)))

(defun nil-command ()
  "A command that does nothing at all. Useful for minor modes
that want to override keys."
  (interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; post-command delete window hook

(defun cic:upward-tag-table ()
  "Look upwards in the directory structure for a TAGS file and
visit it."
  (interactive)
  (let ((home-directory (expand-file-name "~"))
        (ctags-path (expand-file-name "~/tmp/ctags/"))
        (my-tags-file (cic:find-file-upwards "TAGS")))
    (cond (my-tags-file
           (message "Loading tags file: %s" my-tags-file)
           (setq tags-file-name my-tags-file)
           (visit-tags-table my-tags-file))
          ((file-exists-p ctags-path)
           ;; try to find in a tmp file based on second-level secondary directory name
           (let* ((second-level-name (when (string-match (concat home-directory "/[^/]*/\\([^/]*\\)") default-directory)
                                       (match-string 1 default-directory)))
                  (second-level-name-tags (cic:join-paths ctags-path (concat second-level-name "-TAGS"))))
             (when (file-exists-p second-level-name-tags)
               (message "Loading tags file: %s" second-level-name-tags)
               (setq tags-file-name second-level-name-tags)
               (visit-tags-table second-level-name-tags)))))))

(defun cic:xref-find-definitions ()
  "Just find using word at point and default values for TAGS
file."
  (interactive)
  (cic:upward-tag-table)
  (xref-find-definitions (thing-at-point 'symbol)))

;; TODO: this is only elisp for now
(defun cic:find-documentation-transient-window ()
  "Show the documentation for symbol at point in the *Help*
buffer in a new window, but set up so the window closes as soon
as another command is used."
  (interactive)
  (let ((current-symbol (symbol-at-point)))
    (if (fboundp current-symbol)
        (describe-function current-symbol)
      (describe-variable current-symbol))
    (save-window-excursion
      (select-window (get-buffer-window "*Help*"))
      (set-window-parameter nil 'post-command-delete-property t))
    (add-hook 'post-command-hook 'cic:post-command-add-delete-transient))
  nil)

(defun cic:find-definition-transient-window ()
  "Show the definition at point in a new window, but set up so
the window closes as soon as another command is used."
  (interactive)
  (let* ((current-window (get-buffer-window))
         (current-symbol (symbol-at-point))
         (definition-location (when (fboundp current-symbol)
                                  (find-definition-noselect current-symbol nil))))
    (unless definition-location
      (setq definition-location (find-definition-noselect (variable-at-point) 'defvar)))
    (when definition-location
      (pop-to-buffer (car definition-location))
      (goto-char (cdr definition-location))
      (set-window-parameter nil 'post-command-delete-property t)
      (select-window current-window)
      (add-hook 'post-command-hook 'cic:post-command-add-delete-transient)))
  nil)

(defun cic:post-command-add-delete-transient ()
  "Used by post-command-hook to add the actual function that
deletes transient windows."
  (add-hook 'post-command-hook 'post-command-hook-delete-transient)
  (remove-hook 'post-command-hook 'cic:post-command-add-delete-transient))

(defun post-command-hook-delete-transient ()
    "Actually deletes transient windows that are marked with the
window paramter 'post-command-delete-property."
  ;; loop through all of window and check for post-command-delete property
  (dolist (the-window (window-list))
    (when (assoc 'post-command-delete-property (window-parameters the-window))
      (delete-window the-window)))
  (remove-hook 'post-command-hook 'cic:post-command-add-delete-transient)
  (remove-hook 'post-command-hook 'post-command-hook-delete-transient))

(provide 'cic-emacs-functions)
