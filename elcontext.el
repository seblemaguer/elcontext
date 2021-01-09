;;; elcontext.el --- Create context specific actions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Thomas Sojka

;; Author: Thomas Sojka
;; Version: 1.0.0
;; Package-Requires: ((ht "2.3") (hydra "0.14.0") (emacs "24.3") (f "0.20.0") (osx-location "0.4") (uuidgen "0.3"))
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
(require 'hydra)
(require 'f)
(require 'uuidgen)
(require 'elcontext-context)
(require 'elcontext-time)
(require 'elcontext-action)
(require 'elcontext-location)
(require 'elcontext-directory)

(if (string-equal system-type "darwin")
  (require 'osx-location)
  (eval-when-compile (defun osx-location-watch ())))



(defun elcontext--get-contexts-for-table ()
  "Return all context in table format."
  (ht-map (lambda (key context)
            (list key
                  (vconcat
                   (vector (if (elcontext-action-valid-context context)
                               (ht-get context :name)
                             (propertize  (if (ht-get context :name) (ht-get context :name) "") 'face 'elcontext-success)))
                   (when (string-equal system-type "darwin")
                     (vector (elcontext-location-to-string context)))
                   (vector (elcontext-time-to-string context)
                           (ht-get context :directory)
                           (prin1-to-string (ht-get context :action))))))
          elcontext-contexts))

(defun elcontext-check-contexts ()
  "Execute contexts if they are valid."
  (interactive)
  (ht-each (lambda (_ context)
             (if (and
                  (elcontext-action-valid-context context)
                  (elcontext-location-valid-context context)
                  (elcontext-time-valid-context context)
                  (elcontext-directory-valid-context context))
                 (elcontext-action-run context)))
           elcontext-contexts))

(defhydra elcontext-hydra-create-context (:hint nil :foreign-keys warn)
  (concat "_n_: Change name      | Name      %(ht-get elcontext--context-current :name)"
          (when (string-equal system-type "darwin") "
_l_: Change location  | Location  %(elcontext-location-to-string elcontext--context-current)")
 "
_t_: Change time      | Time      %(elcontext-time-to-string elcontext--context-current)
_d_: Change directory | Directory %(ht-get elcontext--context-current :directory)
_a_: Change action    | Action    %(ht-get elcontext--context-current :action)

_c_: Create context
_q_: Quit
")
      ("n" (ht-set! elcontext--context-current :name (read-from-minibuffer "Name: ")))
      ("l" (elcontext-location-create elcontext--context-current) :exit t)
      ("t" (elcontext-time-create elcontext--context-current) :exit t)
      ("d" (elcontext-directory-create elcontext--context-current) :exit t)
      ("a" (ht-set! elcontext--context-current :action (read-minibuffer "Action: ")))
      ("c" (progn
             (elcontext-add-context elcontext--context-id elcontext--context-current)
             (setq elcontext--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))
             (tabulated-list-print)) :color blue)
      ("q" (progn
             (setq elcontext--context-id nil)
             (setq elcontext--context-current (ht (:name nil) (:time (ht)) (:action nil) (:location (ht))))) :exit t))

(defhydra elcontext-hydra-help (:hint nil :exit t)
      "
**elcontext help**

_c_: Create context
_e_: Edit context
_d_: Delete context

_q_: Quit
"
      ("c" (elcontext-new-context))
      ("e" (elcontext-edit-context))
      ("d" (elcontext-delete-context))
      ("q" nil))

(defun elcontext-new-context ()
  "Create a new context."
  (interactive)
  (setq elcontext--context-id (uuidgen-4))
  (elcontext-hydra-create-context/body))

(defun elcontext-edit-context ()
  "Edit context at point."
  (interactive)
  (let ((context-id (tabulated-list-get-id)))
    (setq elcontext--context-id context-id)
    (setq elcontext--context-current (ht-get elcontext-contexts context-id))
    (condition-case nil
        (elcontext-hydra-create-context/body)
      (wrong-type-argument (user-error "No context found at point")))))



(defhydra elcontext-time-hydra (:hint nil :foreign-keys warn)
    "
_f_: Change from | From %(ht-get elcontext-time--current :from)
_t_: Change to   | To   %(ht-get elcontext-time--current :to)
_d_: Add days    | Days %(ht-get elcontext-time--current :days)
_r_: Remove days

_c_: Create timespan
_q_: Quit
"
    ("f" (let ((from-hour (elcontext-time--pad-time (elcontext-time--read-hour (elcontext-time--get-hour elcontext-time--current :from))))
               (from-minute (elcontext-time--pad-time (elcontext-time--read-minute (elcontext-time--get-minute elcontext-time--current :from)))))
           (ht-set! elcontext-time--current :from (concat from-hour ":" from-minute))))
    ("t" (let ((to-hour (elcontext-time--pad-time (elcontext-time--read-hour (elcontext-time--get-hour elcontext-time--current :to))))
               (to-minute (elcontext-time--pad-time (elcontext-time--read-minute (elcontext-time--get-minute elcontext-time--current :to)))))
           (ht-set! elcontext-time--current :to (concat to-hour ":" to-minute))))
    ("d" (ht-set! elcontext-time--current :days
                  (-snoc (ht-get elcontext-time--current :days)
                         (elcontext-time--read-week-days (ht-get elcontext-time--current :days)))))
    ("r" (ht-set! elcontext-time--current :days
                  (-remove-item (completing-read "Remove day:" (ht-get elcontext-time--current :days))
                                (ht-get elcontext-time--current :days))))
    ("c" (progn
           (if (or (and (s-present? (ht-get elcontext-time--current :from)) (s-blank? (ht-get elcontext-time--current :to)))
                   (and (s-present? (ht-get elcontext-time--current :to)) (s-blank? (ht-get elcontext-time--current :from))))
               (progn
                 (message "Please specify a from and to time.")
                 (elcontext-time-hydra/body))
             (progn
               (ht-set! elcontext--context-current :time elcontext-time--current)
               (setq elcontext-time--current (ht))
               (elcontext-hydra-create-context/body)))) :exit t)
    ("q" (progn
           (setq elcontext-time--current (ht))
           (ht-set! elcontext--context-current :time (ht))
           (elcontext-hydra-create-context/body)) :exit t))

(defun elcontext-time-create (context)
  "Create a new timespan or a edit a existing CONTEXT timespan from user input."
  (setq elcontext-time--current (ht-get context :time))
  (elcontext-time-hydra/body))



(defhydra elcontext-directory-hydra (:hint nil :foreign-keys warn)
  "
_s_: Set directory    | %`elcontext-directory--current
_e_: Edit location |

_c_: Create directory
_q_: Quit
"
  ("s" (setq elcontext-directory--current (read-directory-name "Directory: ")))
  ("e" (setq elcontext-directory--current (read-directory-name "Directory: " elcontext--context-current)))
  ("c" (progn
         (ht-set! elcontext--context-current :directory elcontext-directory--current)
         (setq elcontext-directory--current "")
         (elcontext-hydra-create-context/body)) :exit t)
  ("q" (elcontext-hydra-create-context/body) :exit t))

(defun elcontext-directory-create (context)
  "Choose a new directory or a edit a existing CONTEXT directory from user input."
  (setq elcontext-directory--current (ht-get context :directory))
  (elcontext-directory-hydra/body))



(defhydra elcontext-location-hydra (:hint nil :foreign-keys warn)
  "
_l_: Current location | %(elcontext-location-to-string (ht (:location elcontext-location--current)))
_e_: Edit location    |

_c_: Create location
_q_: Quit
"
  ("l" (setq elcontext-location--current (elcontext-location-get-gps)))
  ("e" (setq elcontext-location--current (elcontext-location-edit (ht-get elcontext-location--current :lat)
                                                      (ht-get elcontext-location--current :lon))))
  ("c" (progn
         (ht-set! elcontext--context-current :location elcontext-location--current)
         (setq elcontext-location--current (ht))
         (elcontext-hydra-create-context/body)) :exit t)
  ("q" (elcontext-hydra-create-context/body) :exit t))


(defun elcontext-location-create (context)
  "Create a new location or a edit a existing CONTEXT location from user input."
  (if (string-equal system-type "darwin")
      (progn
        (setq elcontext-location--current (ht-get context :location))
        (elcontext-location-hydra/body))
    (message "Location Feature works only with macOS")))





(define-minor-mode elcontext-global-mode
  "Toogle elcontext-mode. Checks every minute for valid contexts"
  :lighter " elc"
  :group 'elcontext
  :global t
  :require 'elcontext
  (if (symbol-value 'elcontext-global-mode)
      (progn
        (setq elcontext--timer (run-at-time nil 5 'elcontext-check-contexts))
        (when (string-equal system-type "darwin")
          (osx-location-watch)))
    (progn
      (cancel-timer elcontext--timer)
      (setq elcontext--timer nil))))

(defvar elcontext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'elcontext-new-context)
    (define-key map (kbd "e") 'elcontext-edit-context)
    (define-key map (kbd "d") 'elcontext-delete-context)
    (define-key map (kbd "?") 'elcontext-hydra-help/body)
    map)
  "Keymap for `elcontext-mode'.")

(define-derived-mode elcontext-mode tabulated-list-mode "Contexts"
  "Special mode for contexts."
  (setq mode-name "elcontext")
  (use-local-map elcontext-mode-map)
  (setq tabulated-list-format
        (vconcat (vector '("Name" 15 t))
                 (when (string-equal system-type "darwin") (vector '("Location" 25 t)))
                 (vector '("Time" 25 t) '("Directory" 25 t) '("Action" 25 t))))
  (setq tabulated-list-entries 'elcontext--get-contexts-for-table)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun elcontext ()
  "Manage contexts in Emacs."
  (interactive)
  (get-buffer-create "**Contexts**")
  (switch-to-buffer "**Contexts**")
  (elcontext-mode))


(provide 'elcontext)

;;; elcontext.el ends here
