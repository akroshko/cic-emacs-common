;;; cic-emacs--aliases.el --- Aliases for non-prefix versions of
;;; commonly-used function.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu, Aug 27, 2015
;; Version: 20190322
;; URL: https://github.com/akroshko/cic-emacs-common

;; This file is NOT part of GNU Emacs.

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
;; This provides non-prefix versions of basic elisp functions from
;; this package for convienience, debugging, and interactive use.
;; These aliases should not be used in more permanent elisp code. If
;; any of these are conflicting, they can be easily removed from the
;; namespace.
;;
;;; Code:

(defalias 'mpp 'cic:mpp)
(defalias 'mpp-list 'cic:mpp-list)
(defalias 'mpp-echo 'cic:mpp)
(defalias 'full-string-p 'cic:full-string-p)
(defalias 'string-to-float 'cic:string-to-float)
(defalias 'string-integer-p 'cic:string-integer-p)
(defalias 'string-float-p 'cic:string-float-p)
(defalias 'zip 'cic:zip)

(provide 'cic-emacs-aliases)
