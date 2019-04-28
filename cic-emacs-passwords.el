;;; cic-emacs-passwords.el --- Create and insert passwords within
;;; Emacs.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Tue Apr 16, 2019
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
;; Creates and inserts passwords within Emacs, generally using the openssl
;; library.
;;
;;; Code:

;; from https://github.com/jimm/elisp/blob/master/emacs.el
;; http://en.wikipedia.org/wiki/Password_strength
;; 24 alphanumpunc characters = 144 bits of entropy

(defconst cic:password-characters-Alphanum
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "This is 62 characters total")

(defconst cic:password-characters-alphanum-lower
  "0123456789abcdefghijklmnopqrstuvwxyz"
  "This is 36 characters total")

(defconst cic:password-characters-Alphanum-punct
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!?@#$%^&*-_=+/.,"
  "This is 78 characters total.")

(defconst cic:password-characters-Alpha
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "This is 52 characters total.")

(defconst cic:password-characters-alpha-lower
  "abcdefghijklmnopqrstuvwxyz"
  "This is 26 characters total.")

(defun cic:create-password (char-set char-num &optional random-source)
  "Create a random password from CHAR-SET that is CHAR-NUM
characters long.  The shell command 'openssl rand' is used, but
setting RANDOM-SOURCE non-nil uses built-in random function.

WARNING: This function should absolutely be checked before using for
anything too critical!!!"
  ;; TODO: Even more options for random numbers might be better but I
  ;; think openssl is find for this purpose.
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
             ;; this lets me redo for checks of very rare circumstances
             (while (string= new-password "")
               ;; TODO: should probably just make one go at this and then truncate or take random substring
               (dotimes (n char-num)
                 ;; get a random number
                 (setq new-password-char nil)
                 (while (not new-password-char)
                   (setq new-password-char (string-to-number (s-trim-full (shell-command-to-string "openssl rand -hex 1")) 16))
                   (if (>= new-password-char length-passwordchars)
                       (setq new-password-char nil)
                     (setq new-password (concat new-password (substring char-set new-password-char (1+ new-password-char)))))))
               ;; just check for all one character now
               (when (string-match (concat "^[" (substring new-password 0 1) "]+$") new-password)
                 (setq new-password "")))
             new-password)))))

(defun cic:uid-11-alnum-lower ()
  "Create an 11 character unique ID (about 56 bits of randomness).

WARNING: This is meant to be easily searchable and human
enterable, it is not cryptographically secure!"
  (cic:create-password "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 11))

(defun cic:create-password-insert (&optional arg select)
  "When ARG is given, select a random password type and insert
into current buffer.  Without ARG, defaults to 24 character
alphanumeric."
  (interactive "P")
  (let ((select-list '((18anl     "18 character alphanumeric lowercase")
                       (18anlsuff "18 character alphanumeric lowercase with 2 character suffix.")
                       (10anl     "10 character alphanumeric lowercase")
                       (24l       "24 character alphabet lowercase")
                       (24anl     "24 character alphanumeric lowercase")
                       (18anp     "18 character alphanumeric with punctuation")
                       (12anp     "12 character alphanumeric with punctuation")
                       (15an      "15 character alphanumeric")
                       (8anp      "8 character alphanumeric with punctuation")))
        selected)
    (cond ((eq select t)
           (setq selected (car (elt select-list (cic:select-list-item select-list 'cadr)))))
          (arg
           (setq selected '18anlsuff))
          (t
           (setq selected '18anl)))
    (cond ((eq selected '8anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 8)))
          ((eq selected '12anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 12)))
          ((eq selected '18anlsuff)
           (insert (concat (cic:create-password cic:password-characters-alphanum-lower 18) "Q.")))
          ((eq selected '10anl)
           (insert (concat (cic:create-password cic:password-characters-alphanum-lower 10))))
          ((eq selected '18anp)
           (insert (cic:create-password cic:password-characters-Alphanum-punct 18)))
          ((eq selected '24anl)
           (insert (cic:create-password cic:password-characters-alphanum-lower 24)))
          ((eq selected '24l)
           (insert (cic:create-password cic:password-characters-alpha-lower 24)))
          ((eq selected '15an)
           (insert (cic:create-password cic:password-characters-Alphanum 15)))
          (t
           (insert (cic:create-password cic:password-characters-alphanum-lower 18))))))

(defun cic:create-password-insert-select ()
  "Select a random password type and insert into current buffer."
  (interactive)
  (cic:create-password-insert nil t))

(provide 'cic-emacs-passwords)
