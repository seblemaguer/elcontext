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

(require 'calendar)
(require 'ht)
(require 'dash)
(require 's)
(require 'elcontext-utils)

(defvar elcontext-time--current (ht)
  "The current time.")


(defun elcontext-time--date-to-calendardate (date)
    "Convert time to calendar DATE."
    (let ((month (nth 4  date))
          (day (nth 3  date))
          (year (nth 5  date)))
      (substring (calendar-day-name (cons month (cons day (cons year ())))) 0 3)))

(defun elcontext-time--get-hour (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (car (split-string (ht-get* timespan time) ":"))
    (wrong-type-argument nil)))

(defun elcontext-time--get-hour-number (timespan time)
  "Get the hour from a TIMESPAN for a certain TIME."
  (condition-case nil
      (string-to-number (elcontext-time--get-hour timespan time))
    (wrong-type-argument nil)))

(defun elcontext-time--get-minute (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (-last-item (split-string (ht-get* timespan time) ":"))
    (wrong-type-argument nil)))

(defun elcontext-time--get-minute-number (timespan time)
  "Get the minute from a TIMESPAN for a certain TIME."
  (condition-case nil
      (string-to-number (elcontext-time--get-minute timespan time))
    (wrong-type-argument nil)))

(defun elcontext-time--within-timespanp (date timespan)
  "Check if a DATE is within a TIMESPAN."
  (let ((hour (nth 2  (decode-time date)))
        (minute (nth 1  (decode-time date)))
        (day (elcontext-time--date-to-calendardate (decode-time date)))
        (fromHour (elcontext-time--get-hour-number timespan :from))
        (fromMinute (elcontext-time--get-minute-number timespan :from))
        (toHour (elcontext-time--get-hour-number timespan :to))
        (toMinute (elcontext-time--get-minute-number timespan :to))
        (days (ht-get timespan :days)))
    (cond
     ((and (numberp fromHour) (not (numberp toHour))) (user-error "From hour was specified without To hour"))
     ((and (numberp toHour) (not (numberp fromHour))) (user-error "From hour was specified without To hour"))
     ((and (member day days) (not (numberp fromHour)) (not (numberp toHour))) t)
     ((and (not (member day days)) (not (numberp fromHour)) (not (numberp toHour))) nil)
     ((and (null days) (> hour fromHour) (< hour toHour)) t)
     ((and (null days) (>= hour fromHour) (>= minute fromMinute) (< hour toHour)) t)
     ((and (null days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t)
     ((and (member day days) (> hour fromHour) (< hour toHour)) t)
     ((and (member day days) (>= hour fromHour) (>= minute fromMinute) (< hour toHour)) t)
     ((and (member day days) (>= hour fromHour) (>= minute fromMinute) (<= hour toHour) (<= minute toMinute)) t))))

(defun elcontext-time--pad-time (time)
  "Pad a TIME with leading 0s."
  (s-pad-left 2 "0" time))

(defun elcontext-time--format-from (timespan)
  "Format the from hourso of a TIMESPAN."
  (ht-get timespan :from))

(defun elcontext-time--read-week-days (selected-days)
  "Read week days from user input ignoring SELECTED-DAYS."
  (completing-read "Week day: "
            (-difference '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" ) selected-days)
            :require-match t))

(defun elcontext-time--read-hour (&optional hour)
  "Read an hour form user input. Can define default HOUR."
  (elcontext-utils-read-number-range 0 23 "Hour: " hour))

(defun elcontext-time--read-minute (&optional minute)
  "Read an minute form user input. Can define default MINUTE."
  (elcontext-utils-read-number-range 0 59 "Minute: " minute))

(defun elcontext-time-to-string (context)
  "Format a CONTEXT time to a string."
  (let ((timespan (ht-get context :time)))
    (if timespan
        (let ((from-hour (elcontext-time--get-hour timespan :from))
              (from-minute (elcontext-time--get-minute timespan :from))
              (to-hour (elcontext-time--get-hour timespan :to))
              (to-minute (elcontext-time--get-minute timespan :to))
              (days (ht-get timespan :days)))
          (concat
           (when (not (null from-hour))
             (concat
              (elcontext-time--pad-time from-hour) ":" (elcontext-time--pad-time from-minute)
              "-"
              (elcontext-time--pad-time to-hour) ":" (elcontext-time--pad-time to-minute)))
           (when (not (null days))
             (concat (when from-hour " ")
                     (s-replace " " "," (s-replace ")" "" (s-replace "(" "" (format "%s" days))))))))
      "")))

(defun elcontext-time-valid-context (context)
  "Check if the CONTEXT is valid for current time."
  (if (ht-get context :time)
      (elcontext-time--within-timespanp (current-time) (ht-get context :time)))
  t)

(provide 'elcontext-time)

;;; elcontext-time.el ends here
