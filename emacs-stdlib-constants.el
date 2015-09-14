;;; emacs-stdlib-constants.el --- Generic constants for emacs.
;;
;; Copyright (C) 2015, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
;; Version: 20150914
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
;; These are generic constants need by the rest of my packages.
;;
;;; Code:

(defconst cic:emacs-stdlib-url-regexp
  "https\?://[a-zA-Z0-9-]+\.[a-zA-Z0-9-]+\.?[a-zA-Z0-9.-]*/[a-zA-Z0-9.!#%&+./\\\\;,:=?@~_()-]*[a-zA-Z0-9.!#%&+./\\\\:=?@~_()-]\\|https\?://[a-zA-Z0-9-]+\.[a-zA-Z0-9-]+\.?[a-zA-Z0-9.-]*/"
  "A regexp that matches most urls.")

(defconst cic:emacs-stdlib-url-regexp-complete
  (concat "\\[\\[\\("  cic:emacs-stdlib-url-regexp "\\)\\(\\]\\[.*\\)?::[a-zA-Z0-9_-]\\{10,11\\}::\\]\\]")
  "A regexp that matches a complete url.")

(defconst cic:emacs-stdlib-url-regexp-org
  (concat "\\[\\[\\(" cic:emacs-stdlib-url-regexp "\\)\\]\\[.*\\]\\]")
  "A regexp that matches an org-mode url.")

(defconst cic:emacs-stdlib-headline-regexp
  "^\\*+ \\(.*\\)")

(defconst cic:emacs-stdlib-list-regexp
  "^ *\\([-+*]\\) \\(\\[.\\]\\)?\\(.*\\)")

(defconst cic:emacs-stdlib-checkbox-regexp
  "^ *\\([-+*]\\) \\(\\[.\\]\\)\\(.*\\)")

(defconst cic:emacs-stdlib-checkbox-checked-regexp
  "^ *\\([-+*]\\) \\(\\[[X-]\\]\\)\\(.*\\)")

(defconst cic:emacs-stdlib-checkbox-unchecked-regexp
  "^ *\\([-+*]\\) \\(\\[ \\]\\)\\(.*\\)")

(defconst cic:emacs-stdlib-list-exact-regexp
  "^ *\\([-+*]\\) \\(\\[.\\]\\)? *")

(provide 'emacs-stdlib-constants)
