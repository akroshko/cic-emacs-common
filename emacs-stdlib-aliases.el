;;; emacs-stdlib-aliases.el --- Aliases for non-prefix versions of
;;; commonly-used function.
;;
;; Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu, Aug 27, 2015
;; Version: 20180209
;; URL: https://github.com/akroshko/emacs-stdlib
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
;; This provides non-prefix versions of basic elisp functions for
;; convienience.  Therefore any conflicting names are easily removed
;; from the namespace.
;;
;;; Code:

(defalias 'mpp 'cic:mpp)
(defalias 'mpp-echo 'cic:mpp)
(defalias 'chomp 'cic:chomp)
(defalias 'strip-full 'cic:strip-full)
(defalias 'strip-full-no-properties 'cic:strip-full-no-properties)
(defalias 'strip-full-single-spaces 'cic:strip-full-single-spaces)
(defalias 'strip-colons 'cic:strip-colons)
(defalias 'strip-square-brackets 'cic:strip-square-brackets)
(defalias 'strip-dashes 'cic:strip-dashes)
(defalias 'remove-trailing-whitespace 'cic:remove-trailing-whitespace)
(defalias 'remove-leading-whitespace 'cic:remove-leading-whitespace)
(defalias 'full-string-p 'cic:full-string-p)
(defalias 'string-to-float 'cic:string-to-float)
(defalias 'car-fallthrough 'cic:car-fallthrough)
(defalias 'cdr-fallthrough 'cic:cdr-fallthrough)
(defalias 'string-integer-p 'cic:string-integer-p)
(defalias 'string-float-p 'cic:string-float-p)
(defalias 'zip 'cic:zip)
(defalias 'car-only 'cic:car-only)
(defalias 'nts-nan 'cic:nts-nan)

(provide 'emacs-stdlib-aliases)
