;;; cic-emacs-strings.el --- Emacs functions for processing strings
;;; and other text.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Apr 16, 2019
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
;; These are functions focused on string processing.  They should not require
;; extra features over a base Emacs installation.
;;
;;; Code:

(defun s-trim-full (str)
  "Trim all leading and trailing whitespace from STR.  Does this
for every line."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun s-trim-full-no-properties (str)
  "Like trim-full but remove text properties."
  (substring-no-properties (s-trim-full str)))

(defun cic:trim-into-one-line (str)
  "Convert STR into one line with no leading or trailing whitespace."
  (while (string-match "[[:space:]]*\n+[[:space:]]*\\|[[:space:]]\\{2,\\}"
                       str)
    (setq str (replace-match " " t t str)))
  (s-trim-full str))

(defun s-trim-colons (str)
  "Trim leading and trailing colons off of STR."
  (when (stringp str)
    (while (string-match "^:" str)
      (setq str (replace-match "" t t str)))
    (while (string-match ":$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun cic:trim-trailing-slash (str)
  "Trim trailing slash off of STR."
  (when (stringp str)
    (while (string-match "/$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun cic:trim-after-double-colon (str)
  "Trim everything after and including a double colon in STR."
  (when (stringp str)
    (if (string-match "\\(.*\\)::.*" str)
        (setq str (match-string 1 str))))
  str)

(defun s-trim-square-brackets (str)
  "Trim leading and trailing square brackets off of STR."
  (when (stringp str)
    (while (string-match "^\\[" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "\\]$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun s-trim-dashes (str)
  "Trim leading and trailing dashes off of STR."
  (when (stringp str)
    (while (string-match "^-" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "-$" str)
      (setq str (replace-match "" t t str))))
  str)

(defun s-trim-leading-dashes (str)
  "Trim leading dashes off of STR."
  (when (stringp str)
    (while (string-match "^-" str)
      (setq str (replace-match "" t t str))))
  str)

(defun cic:full-string-p (thing-or-string)
  "Determine if something is nil or an empty string."
  (if (or (not thing-or-string) (equal (s-trim-full thing-or-string) ""))
      nil
    t))

(defun cic:string-to-float (str)
  "Convert STR to floating point number.  If STR is a
non-floating point number convert it to a floating point number.
If STR is already a floating point number then just return STR."
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

; http://www.emacswiki.org/emacs/ElispCookbook#toc20
(defun cic:string-integer-p (string)
  "Check if STRING is an integer."
  (when (string-match "\\`[-+]?[0-9]+\\'" string)
      t))

(defun cic:string-float-p (string)
  "Check if STRING is a floating point number."
  (when (string-match "\\`[-+]?[0-9]+\\.[0-9]*\\'" string)
      t))

(defun cic:number-to-string-nan (num &optional format-string)
  "Like number-to-string but returns a nil for nan."
  (unless format-string
    (setq format-string "%4.3f"))
  (if (or (not num) (isnan (float num)) (= num 1.0e+INF) (= num -1.0e+INF))
      ""
    (format format-string num)))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
;; TODO: add cic- prefix
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

(defun cic:is-not-empty-string-nil (str)
  "Check if STR is an empty string (no characters or all
whitespace) or a nil."
  (and str (not (string= (s-trim-full str) ""))))

(defun increment-char (ch)
  "Not perfect, need better way sometimes to select file..."
  (cond ((string= ch "z")
         "A")
        ((string= ch "Z")
         "1")
        (t
         (char-to-string (1+ (string-to-char ch))))))

(defun replace-nonprintable (str)
  "Replace non-printable characters with blanks in STR.
TODO: No str variable name and replace some bad characters with
dsimilar ones."
  (replace-regexp-in-string "[^[:print:]]" "" str))

(defun cic:escape-posix-regexp (posix-regexp)
  "Escapes a few select posix regexps to emacs regexps.
  Generally functionality is added here as needed."
  (replace-regexp-in-string (regexp-quote "\\\\") "\\\\" posix-regexp))

;; (convert-whole-numbers-to-decimal "27/89 ")
;; (convert-whole-numbers-to-decimal "27/89")
;; (convert-whole-numbers-to-decimal "7/89 ")
;; (convert-whole-numbers-to-decimal "a7/89 ")
;; (convert-whole-numbers-to-decimal "a7/89b ")
(defun convert-whole-numbers-to-decimal (thestring)
  ;; find all numbers no adjacent to letters that are not decimals
  (replace-regexp-in-string "\\(^\\|[^\.A-Za-z]\\)\\([0-9]\+\\)\\($\\|[^\.A-Za-z0-9]\\)" "\\2\." thestring nil nil 2))

(defun convert-newlines-to-spaces (thestring)
  (replace-regexp-in-string "\n" " " (s-trim-full thestring)))

(provide 'cic-emacs-strings)
