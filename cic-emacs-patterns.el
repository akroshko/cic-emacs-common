;;; cic-emacs-patterns.el --- Generic constants for emacs.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Fri Mar 27, 2015
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
;; These are generic constants need by the rest of my packages.
;;
;;; Code:

(defconst cic:url-regexp
  ;; XXXX: taking off ending slash
  "\\(https\?\\|ftp\\)://[a-zA-Z0-9-]+\.[a-zA-Z0-9-]+\.?[a-zA-Z0-9.-]*/[a-zA-Z0-9.!#%&+./\\\\;,:=?@~_()-]*[a-zA-Z0-9.!#%&+./\\\\:=?@~_()-]\\|https\?://[a-zA-Z0-9-]+\.[a-zA-Z0-9-]+\.?[a-zA-Z0-9.-]*"
  "A regexp that matches most urls.")

(defconst cic:url-regexp-complete
  (concat "\\[\\[\\("  cic:url-regexp "\\)\\(\\]\\[.*\\)?::[a-zA-Z0-9_-]\\{11\\}::\\]\\]")
  "A regexp that matches a complete url.")

(defconst cic:url-regexp-org
  (concat "\\[\\[\\(" cic:url-regexp "\\)\\]\\[.*\\]\\]")
  "A regexp that matches an org-mode url.")

(defconst cic:headline-regexp
  "^\\*+ \\(.*\\)")

(defconst cic:list-regexp
  "^ +\\([-+*]\\) \\(\\[.\\]\\)?\\(.*\\)")

(defconst cic:checkbox-regexp
  "^ +\\([-+*]\\) \\(\\[.\\]\\)\\(.*\\)")

(defconst cic:checkbox-checked-regexp
  "^ +\\([-+*]\\) \\(\\[[X-]\\]\\)\\(.*\\)")

(defconst cic:checkbox-unchecked-regexp
  "^ +\\([-+*]\\) \\(\\[ \\]\\)\\(.*\\)")

(defconst cic:list-exact-regexp
  "^ +\\([-+*]\\) \\(\\[.\\]\\)? *")

(provide 'cic-emacs-patterns)
