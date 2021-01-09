;;; elcontext.el --- Create context specific actions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka
;; Version: 1.0.0
;; Package-Requires: ((ht "2.3") (emacs "24.3"))
;; Keywords: calendar, convenience
;; URL: https://github.com/rollacaster/elcontext

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Use M-x elcontext for on overview of all contexts. Within this overview
;; several hydras will guide through the API, press ? to open the help hydra.
;; Contexts can consist of name, location (only for macOS), timespan, directory,
;; and action. If a part of a context is omitted this part is always valid e.g.
;; no location means anywhere, no time means anytime. Each command is triggered
;; once per day.

;;; Code:

(require 'ht)

(defvar elcontext-contexts (ht))

(defvar elcontext--timer nil)

(defvar elcontext--context-id nil)

(defvar elcontext--context-current
      (ht (:name nil) (:time (ht)) (:action nil) (:location (ht)) (:directory "")))

(defface elcontext-success
  '((((class color)) :inherit 'success))
  "Green color indicating a context which did run today."
  :group 'elcontext)


(defun elcontext-add-context (id context)
  "Store a context with ID and CONTEXT."
  (ht-set! elcontext-contexts id context))

(defun elcontext-delete-context ()
  "Delete context at point."
  (interactive)
  (let ((context (ht-get elcontext-contexts (tabulated-list-get-id))))
    (condition-case nil
        (when (y-or-n-p (concat "Delete context " (ht-get context :name) "?"))
          (ht-remove! elcontext-contexts (tabulated-list-get-id))
          (tabulated-list-print))
      (wrong-type-argument (user-error "No context found at point")))))

(defun elcontext--save-contexts ()
  "Save contexts to disk."
  (f-write-text (prin1-to-string elcontext-contexts) 'utf-8
                (expand-file-name ".contexts" user-emacs-directory)))

(add-hook 'kill-emacs-hook 'elcontext--save-contexts)

(defun elcontext--load-contexts ()
  "Load contexts from disc."
  (when (f-exists? (expand-file-name ".contexts" user-emacs-directory))
    (let ((saved-contexts (read (f-read-text (expand-file-name ".contexts" user-emacs-directory)))))
      (if (ht? saved-contexts)
          (setq elcontext-contexts saved-contexts)
        (setq elcontext-contexts (ht))))))

(elcontext--load-contexts)

(provide 'elcontext-context)

;;; elcontext.el ends here
