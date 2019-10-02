;;; tblel.el --- Use elisp functions to calculate org-tables.
;;
;; Copyright (C) 2016-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu June 2, 2015
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
;;
;;; Code:
;; TODO: update to tblel prefix
;; TODO: indicate independent or supporting for other libraries
;; TODO: originally written due to org-table-calc being slow

(defun tblel-p ()
  "Find if we are at a location associated with an org-table
along with a #+TBLEL line."
  (or
   (tblel-line-p)
   (save-excursion
     (and
      (org-table-p)
      (progn
        (goto-char (org-table-end))
        (beginning-of-line)
        (looking-at " *#\\+TBLEL:"))))))

(defun tblel-line-p ()
  "Check if at a #+TBLEL line."
  (save-excursion
    (beginning-of-line)
    (looking-at " *#\\+TBLEL:")))

(defun tblel-eval-line (&rest args)
  "Evalute the function on a #+TBLEL line."
  (when (tblel-line-p)
    (tblel-eval)))

(defun tblel-eval (&rest args)
  "Get the lisp table and run the appropriate function on it (several functions?)."
  ;; TODO: unwind protect to avoid nuking table
  ;; TODO: avoid back-to-heading/find-table and use a better methodology for tables
  (interactive)
  (when (tblel-p)
    (let (lisp-table
          original-lisp-table
          lisp-function
          new-lisp-table)
      ;; get elisp function to run
      (save-excursion
        (unless (tblel-line-p)
            (goto-char (org-table-end)))
        (beginning-of-line)
        ;; TODO: eventually get forms
        (let ((split-tblel-line (split-string (cic:get-current-line))))
          (setq lisp-function (substring-no-properties (elt split-tblel-line 1)))
          (setq lisp-function-args (subseq split-tblel-line 2))))
      (save-excursion
        (when (tblel-line-p)
          (forward-line -1)
          (back-to-indentation))
        ;; TODO: just evaluating a single lisp function, want more and
        ;; want to check error before nuking current table
        (setq lisp-table          (copy-tree (org-table-to-lisp))
              original-lisp-table (copy-tree (cic:org-table-to-lisp-no-separators)))
        ;; XXXX: found it essential to send copy-tree of lisp-table to
        ;; function, stops many subtle bugs
        (if lisp-function-args
            (setq new-lisp-table (funcall (intern lisp-function) (copy-tree lisp-table) (copy-tree original-lisp-table) lisp-function-args))
          (setq new-lisp-table (funcall (intern lisp-function) (copy-tree lisp-table) (copy-tree original-lisp-table))))
        ;; XXXX: make sure nil does not erase table
        (when new-lisp-table
          ;; finally put it back if all is well
          (cic:org-table-elisp-replace original-lisp-table new-lisp-table)
          ;; TODO: option to avoid this?
          (org-table-align))))
    t))

;; TODO: appears unused
(defun tblel-ctrl-c-recalc (orig-fun &rest args)
  "Function to advise ctrl-c and org-table evaluate functions."
  ;; kill ctrl-c-ctrol-c and just do our own thing until TBLEL==keyword is fixed
  (when (tblel-line-p)
    (funcall orig-fun args)))

;; add some advice to intercept org-table functions and commands
(advice-add 'org-table-recalculate :before #'tblel-eval)
(add-hook 'org-ctrl-c-ctrl-c-hook 'tblel-eval-line)

(defface tblel-command-face
  '((t (:foreground "Firebrick")))
  "Show the tblel command.")

;; TODO: add better fontification for this so it works in other modes
(defun tblel-setup ()
  "Set up some things so tblel is as integrated as possible in org-table."
  ;; set up fontification
  (font-lock-add-keywords 'org-mode
                          ;; TODO: change to org-meta-line, in keyword-face for convienience right now
                          '(("^\\s-+\\(#\\+TBLEL:.*\\)$" . tblel-command-face))))

(add-hook 'org-mode-hook 'tblel-setup)

;; a nice generic sum function, sum all sumable solumns
;; TODO: get a table with seperators!!!!
(defun tblel-generic-sum (lisp-table  lisp-table-no-seperators &rest tblel-args)
  "Sum any column that is summable to the line at the end, and
excluding a header."
  (let ((sums (make-list (length (car lisp-table-no-seperators)) nil))
        (column-count 0)
        tmp-lisp-table)
    (dolist (row (cdr (butlast lisp-table-no-seperators)))
      (setq column-count 0)
      (dolist (current-column row)
        (when (or (not tblel-args) (member (number-to-string (1+ column-count)) (car tblel-args)))
          (when (or (cic:string-float-p current-column)
                    (cic:string-integer-p current-column))
            (unless (elt sums column-count)
              (setcar (nthcdr column-count sums) 0))
            (setcar (nthcdr column-count sums) (+ (elt sums column-count) (string-to-number current-column)))))
        (setq column-count (1+ column-count))))
    ;; now just insert it in last thing
    (setq tmp-lisp-table (butlast lisp-table-no-seperators))
    (append tmp-lisp-table (list (mapcar (lambda (e) (ignore-errors (number-to-string e))) sums)))))

(defun tblel-generic-sum-cumulative-two-three (lisp-table lisp-table-no-seperators &rest tblel-args)
  "Sum column one and record cumulative sums in column two and
three."
  (let ((sum1 0)
        (sums2 (make-list (length lisp-table-no-seperators) 0))
        (sums3 (make-list (length lisp-table-no-seperators) 0))
        (count 0)
        (tmp-lisp-table (butlast lisp-table-no-seperators))
        (sums (make-list (length (car lisp-table-no-seperators)) nil)))
    (dolist (row (butlast lisp-table-no-seperators))
      (when (and (not (equal count 0))
                 (or (cic:string-float-p (elt row 1))
                     (cic:string-integer-p (elt row 1)))
                 (setq sum1 (+ sum1 (string-to-number (elt row 1))))))
      (unless (equal count 0)
        (cond ((or (cic:string-float-p   (elt row 1))
                   (cic:string-integer-p (elt row 1)))
               (setcar (nthcdr count sums2) (+ (elt sums2 (- count 1)) (string-to-number (elt row 1))))
               (setcar (nthcdr count sums3) (+ (elt sums3 (- count 1)) (string-to-number (elt row 1)))))
              (t
               (setcar (nthcdr count sums2) (+ (elt sums2 (- count 1)) 0))
               (setcar (nthcdr count sums3) (+ (elt sums3 (- count 1)) 0))))
        (setcar (nthcdr 2 (elt tmp-lisp-table count)) (number-to-string (elt sums2 count)))
        (setcar (nthcdr 3 (elt tmp-lisp-table count)) (number-to-string (elt sums3 count))))
      (setq count (1+ count)))
    ;; now just insert it in last thing
    (setcar (nthcdr 1 sums) (number-to-string sum1))
    (setcar (nthcdr 2 sums) sums2)
    (setcar (nthcdr 3 sums) sums3)
    (append tmp-lisp-table (list sums))))

(defun tblel-generic-sum-quantity (lisp-table lisp-table-no-seperators &rest tblel-args)
  "Sums a quantity in second column with value in third column,
into the last row."
  (let ((sum nil)
        tmp-lisp-table
        last-row)
    (dolist (row (cdr (butlast lisp-table-no-seperators)))
      (when (and (or (cic:string-float-p (elt row 1))
                    (cic:string-integer-p (elt row 1)))
                 (or (cic:string-float-p (elt row 2))
                     (cic:string-integer-p (elt row 2))))
        (unless sum
          (setq sum 0))
        (setq sum (+ sum (* (string-to-number (elt row 1)) (string-to-number (elt row 2)))))))
    (setq tmp-lisp-table (butlast lisp-table-no-seperators)
          last-row (car (last lisp-table-no-seperators)))
    (setcar (nthcdr 2 last-row) (ignore-errors (number-to-string sum)))
    (append tmp-lisp-table (list last-row))))

(defun tblel-inches-to-millimeters (lisp-table lisp-table-no-seperators &rest tblel-args)
  (let ((tmp-lisp-table (copy-tree lisp-table-no-seperators)))
    (dolist (row lisp-table-no-seperators)
      ;; TODO: need a get inches function, take out of otdb
      (let ((inches (when (string-match "\\([0-9.]+\\)\"" (car row))
                      (match-string 1 (car row)))))
        (when inches
          (setcar (nthcdr 1 row) (concat (format "%.2f" (* (string-to-float inches) 25.4)) "mm")))))
    lisp-table-no-seperators))

;; (tblel-set-rest-reps (org-table-to-lisp) (cic:org-table-to-lisp-no-separators))
(defun tblel-set-rest-reps (lisp-table lisp-table-no-seperators &rest tblel-args)
  "Calculate a table with sets, set rest, reps, rep duration and
  figure out total amount."
  (let ((new-lisp-table (list (elt lisp-table 1)))
        total-column
        total-work-column
        total-work
        total-total
        (cummulative-work 0.0)
        (cummulative-total 0.0)
        cummulative-work-list
        cummulative-total-list
        current-cummulative-work
        current-cummulative-total)
    (dolist (lisp-row (cddr (butlast lisp-table 2)))
      (let ((sets         (unless (eq lisp-row 'hline) (string-to-number (elt lisp-row 1))))
            (reps         (unless (eq lisp-row 'hline) (string-to-number (elt lisp-row 3))))
            (set-rest     (unless (tblel-time-string-to-seconds (elt lisp-row 2))))
            (rep-duration (unless (eq lisp-row 'hline) (tblel-time-string-to-seconds (elt lisp-row 4)))))
        ;; TODO: document this better, other things?
        ;; TODO: match only "rest", guard against exercises that might have substring "rest"
        ;; TODO: want total reps too, really only useful for pullups...
        ;; TODO: really want next-lisp-row 'hline
        (cond ((eq lisp-row 'hline)
               ;; update last cummulative work and reset
               (setq cummulative-work-list (append (butlast cummulative-work-list) (list cummulative-work)))
               (setq cummulative-work 0.0)
               ;; update last cummulative total and reset
               (setq cummulative-total-list (append (butlast cummulative-total-list) (list cummulative-total)))
               (setq cummulative-total 0.0))
              ((string-match "rest" (downcase (elt lisp-row 0)))
               (let ((new-total (tblel-time-string-to-seconds (elt lisp-row 5))))
                 (setq total-column           (nconc total-column (list new-total))
                       cummulative-total      (+ new-total cummulative-total)
                       cummulative-work-list  (nconc cummulative-work-list (cons nil nil))
                       cummulative-total-list (nconc cummulative-total-list (cons nil nil)))))
              (t
               (let ((new-work (* sets (* reps rep-duration)))
                     (new-total (+ (* (- sets 1) set-rest) (* sets (* reps rep-duration)))))
                 (setq total-work-column      (nconc total-work-column (list new-work))
                       total-column           (nconc total-column (list new-total))
                       cummulative-work       (+ new-work cummulative-work)
                       cummulative-total      (+ new-total cummulative-total)
                       cummulative-work-list  (nconc cummulative-work-list (cons nil nil))
                       cummulative-total-list (nconc cummulative-total-list (cons nil nil))))))))
    (setq total-total            (apply '+ total-column)
          total-work             (apply '+ total-work-column)
          cummulative-work-list  (cdr cummulative-work-list)
          cummulative-total-list (cdr cummulative-total-list))
    (dolist (current-lisp-row (cddr (butlast lisp-table 2)))
      (cond ((eq current-lisp-row 'hline)
             t)
            ((string-match "rest" (downcase (elt current-lisp-row 0)))
             (setq current-cummulative-work  (pop cummulative-work-list)
                   current-cummulative-total (pop cummulative-total-list))
             (setq new-lisp-table
                   (nconc
                    new-lisp-table
                    (list (nconc
                           (subseq current-lisp-row 0 1)
                           (nconc (list "" "" "" ""))
                           (list (if current-cummulative-total
                                     (concat
                                      "("
                                      (format-seconds "%m:%.2s" current-cummulative-total)
                                      ") "
                                      (format-seconds "%m:%.2s" (pop total-column)))
                                   (format-seconds "%m:%.2s" (pop total-column)))))))))
            (t
             (setq current-cummulative-work  (pop cummulative-work-list)
                   current-cummulative-total (pop cummulative-total-list))
             (setq new-lisp-table
                   (nconc
                    new-lisp-table
                    (list (nconc
                           (subseq current-lisp-row 0 5)
                           (list (if current-cummulative-total
                                     (concat
                                      "("
                                      (format-seconds "%m:%.2s" current-cummulative-total)
                                      ") "
                                      (format-seconds "%m:%.2s" (pop total-column)))
                                   (format-seconds "%m:%.2s" (pop total-column)))))))))))
    (setq new-lisp-table (nconc
                          new-lisp-table
                          (list
                           (list (elt (car (last lisp-table 2)) 0)
                                 ""
                                 ""
                                 ""
                                 (format-seconds "%m:%.2s" total-work)
                                 (format-seconds "%m:%.2s" total-total)))))))

(defun tblel-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  ;;XXXX: get second time when multiple (allows good summaries)
  ;; TODO: generalize beyond 2
  (when (and (stringp s) (> (length (split-string s)) 1))
    (setq s (elt (split-string s) 1)))
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
            (unless (string= (cic:elisp-array-string elisp-table-original i j)
                             (cic:elisp-array-string elisp-table-replacement i j))
              (org-table-put (1+ i) (1+ j) (cic:elisp-array-string elisp-table-replacement i j)))))))))

(defun cic:elisp-array-string (elisp-array i j)
  (let ((thestr (elt (elt elisp-array i) j)))
    (if (stringp thestr)
        (s-trim-full-no-properties thestr)
      "")))

(provide 'tblel)
