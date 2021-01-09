;;; elcontext.el --- Create context specific actions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ht)

(defvar elcontext-directory--current ""
  "The current directory of the context.")

(defun elcontext-directory-valid-context (context)
  "Check if the CONTEXT did already run today."
  (when (or (equal (ht-get context :directory) "")
            (equal (expand-file-name "" (ht-get context :directory))
                   (expand-file-name "" default-directory)))
    t))

(provide 'elcontext-directory)

;;; elcontext-directory.el ends here
