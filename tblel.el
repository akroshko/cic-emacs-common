;;; tblel.el --- Use elisp functions to calculate org-tables.
;;
;; Copyright (C) 2016, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu June 2, 2015
;; Version: 20160810
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
        (looking-at "\\s-+#\\+TBLEL:"))))))

(defun tblel-line-p ()
  "Check if at a #+TBLEL line."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-+#\\+TBLEL:")))

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
          lisp-function
          new-lisp-table)
      ;; get elisp function to run
      (save-excursion
        (unless (tblel-line-p)
            (goto-char (org-table-end)))
        (beginning-of-line)
        ;; TODO: eventually get forms
        (setq lisp-function (substring-no-properties (elt (split-string (cic:get-current-line)) 1))))
      (save-excursion
        (when (tblel-line-p)
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

;; TODO: appears unused
(defun tblel-ctrl-c-recalc (orig-fun &rest args)
  "Function to advise ctrl-c and org-table evaluate functions."
  ;; kill ctrl-c-ctrol-c and just do our own thing until TBLEL==keyword is fixed
  (when (tblel-line-p)
    (funcall orig-fun args)))

;; add some advice to intercept org-table functions and commands
(advice-add 'org-table-recalculate :before #'tblel-eval)
(add-hook 'org-ctrl-c-ctrl-c-hook 'tblel-eval-line)

(defun tblel-setup ()
  "Set up some things so tblel is as integrated as possible in org-table."
  ;; set up fontification
  (font-lock-add-keywords 'org-mode
                          ;; TODO: change to org-meta-line, in keyword-face for convienience right now
                          '(("^\\s-+\\(#\\+TBLEL:.*\\)$" . font-lock-comment-face))))


(add-hook 'org-mode-hook 'tblel-setup)

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

(defun tblel-generic-sum-cumulative-two-three (lisp-table)
  "Sum column one and record cumulative sums in column two and
three."
  (let ((sum1 0)
        (sums2 (make-list (length lisp-table) 0))
        (sums3 (make-list (length lisp-table) 0))
        (count 0)
        (tmp-lisp-table (butlast lisp-table))
        (sums (make-list (length (car lisp-table)) nil)))
    (setq count 0)
    (dolist (row (butlast lisp-table))
      (when (and (not (equal count 0))
                 (or (cic:string-float-p (elt row 1))
                     (cic:string-integer-p (elt row 1)))
             (setq sum1 (+ sum1 (string-to-number (elt row 1))))))
      (when (not (equal count 0))
        (cond ((or (cic:string-float-p   (elt row 1))
                   (cic:string-integer-p (elt row 1)))
               (setcar (nthcdr count sums2) (+ (elt sums2 (- count 1)) (string-to-number (elt row 1))))
               (setcar (nthcdr count sums3) (+ (elt sums3 (- count 1)) (string-to-number (elt row 1)))))
              (t
               (setcar (nthcdr count sums2) (+ (elt sums2 (- count 1)) 0))
               (setcar (nthcdr count sums3) (+ (elt sums3 (- count 1)) 0))))
        (setcar (nthcdr 2 (elt tmp-lisp-table count)) (number-to-string (elt sums2 count)))
        (setcar (nthcdr 3 (elt tmp-lisp-table count)) (number-to-string (elt sums3 count))))
      (mpp tmp-lisp-table)
      (setq count (+ count 1)))
    ;; now just insert it in last thing
    (setcar (nthcdr 1 sums) (number-to-string sum1))
    (setcar (nthcdr 2 sums) sums2)
    (setcar (nthcdr 3 sums) sums3)
    (append tmp-lisp-table (list sums))))

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

;; (tblel-set-rest-reps (cic:org-table-to-lisp-no-separators))
(defun tblel-set-rest-reps (lisp-table)
  "Calculate a table with sets, set rest, reps, rep duration and
  figure out total amount."
  (let ((new-lisp-table (list (car lisp-table)))
        total-column
        total-work-column
        total-work
        total-total)
    (dolist (lisp-row (butlast (cdr lisp-table)))
      (let ((sets (string-to-number (elt lisp-row 1)))
            (reps (string-to-number (elt lisp-row 3)))
            (set-rest (tblel-time-string-to-seconds (elt lisp-row 2)))
            (rep-duration (tblel-time-string-to-seconds (elt lisp-row 4))))
        ;; TODO: document this better, other things?
        ;; TODO: match only "rest", guard against exercises that might have substring "rest"
        ;; TODO: want total reps too, really only useful for pullups...
        (if (string-match "rest" (downcase (elt lisp-row 0)))
            (progn
              (setq total-column (nconc total-column (list (tblel-time-string-to-seconds (elt lisp-row 5))))))
          (progn
            (setq total-work-column (nconc total-work-column (list (* sets (* reps rep-duration)))))
            (setq total-column (nconc total-column (list (+ (* (- sets 1) set-rest) (* sets (* reps rep-duration))))))))))
    (setq total-total (apply '+ total-column))
    (setq total-work  (apply '+ total-work-column))
    (dolist (current-lisp-row (cdr (butlast lisp-table)))
      (if (string-match "rest" (downcase (elt current-lisp-row 0)))
          (setq new-lisp-table
                (nconc
                 new-lisp-table
                 (list (nconc
                        (subseq current-lisp-row 0 1)
                        (nconc (list "" "" "" ""))
                        (list (format-seconds "%m:%.2s" (pop total-column)))))))
        (setq new-lisp-table
              (nconc
               new-lisp-table
               (list (nconc
                      (subseq current-lisp-row 0 5)
                      (list (format-seconds "%m:%.2s" (pop total-column)))))))))
    (setq new-lisp-table (nconc
                          new-lisp-table
                          (list
                           (list (elt (car (last lisp-table)) 0)
                                 ""
                                 ""
                                 ""
                                 (format-seconds "%m:%.2s" total-work)
                                 (format-seconds "%m:%.2s" total-total)))))))


;; XXXX: moved and renamed from github.com/akroshko/emacs-stdlib/emacs-stdlib-functions.el
(defun tblel-time-string-to-seconds (s)
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

(provide 'tblel)
