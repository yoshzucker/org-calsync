;;; org-calsync-busy.el --- Export an abstracted busy ics  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-calsync

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The other direction, and the reason this package can be used at work.
;;
;; A calendar full of client names and meeting subjects cannot leave the
;; machine it lives on.  But the *shape* of it can: when you are busy, for how
;; long, and nothing else.  This writes an ics in which every event carries one
;; fixed label and its time -- no title, no location, no notes, no attendees --
;; which is safe to send to a phone or a private calendar.
;;
;; The abstraction is the whole point, so it is done by construction rather
;; than by filtering: `org-calsync-busy--vevent' is given only a UID, a time
;; and the label, and there is no code path by which anything else could reach
;; the file.

;;; Code:

(require 'org-calsync)
(require 'org-element)

(defcustom org-calsync-busy-file-name "busy.ics"
  "Basename, under `org-directory', of the abstracted busy ics."
  :type 'string
  :group 'org-calsync)

(defcustom org-calsync-busy-label "Busy"
  "The SUMMARY every exported event carries -- the only text in the file."
  :type 'string
  :group 'org-calsync)

(defcustom org-calsync-busy-days 60
  "How many days ahead `org-calsync-export-busy' covers."
  :type 'integer
  :group 'org-calsync)

(defcustom org-calsync-busy-exclude-categories nil
  "Categories left out of the busy ics.

For events the receiving device already has: exporting those would show every
one of them twice, once with its real name and once as a nameless block."
  :type '(repeat string)
  :group 'org-calsync)

(defcustom org-calsync-busy-compose-function nil
  "Function called with the written ics path, to hand it to a mailer.
Nil just reports the path."
  :type '(choice (const :tag "just report the path" nil) function)
  :group 'org-calsync)

(defun org-calsync-busy-file ()
  "Absolute path of the abstracted busy ics."
  (org-calsync--expand-file org-calsync-busy-file-name))

(defun org-calsync-busy--rrule (ts)
  "Return an ical RRULE line for Org timestamp TS's repeater, or nil."
  (let ((val (or (org-element-property :repeater-value ts) 1)))
    (pcase (org-element-property :repeater-unit ts)
      ('day   (format "RRULE:FREQ=DAILY;INTERVAL=%d" val))
      ('week  (format "RRULE:FREQ=WEEKLY;INTERVAL=%d" val))
      ('month (format "RRULE:FREQ=MONTHLY;INTERVAL=%d" val))
      ('year  (format "RRULE:FREQ=YEARLY;INTERVAL=%d" val)))))

(defun org-calsync-busy--ics-times (ts)
  "Return (:all-day BOOL :dtstart STR :dtend STR) for Org timestamp TS.

All-day DTEND is the exclusive next day, which is the ics convention and the
mirror of what the reader takes off on the way in.  A timed event with no end
gets a zero-length one rather than a guessed duration: the export says when
you are busy, and inventing a length would be saying something else."
  (let ((ys (org-element-property :year-start ts))
        (ms (org-element-property :month-start ts))
        (ds (org-element-property :day-start ts))
        (hs (org-element-property :hour-start ts))
        (mns (org-element-property :minute-start ts))
        (ye (org-element-property :year-end ts))
        (me (org-element-property :month-end ts))
        (de (org-element-property :day-end ts))
        (he (org-element-property :hour-end ts))
        (mne (org-element-property :minute-end ts)))
    (if (null hs)
        (let ((end+1 (time-add (encode-time 0 0 0 de me ye) 86400)))
          (list :all-day t
                :dtstart (format "%04d%02d%02d" ys ms ds)
                :dtend (format-time-string "%Y%m%d" end+1)))
      (list :all-day nil
            :dtstart (format "%04d%02d%02dT%02d%02d00" ys ms ds hs (or mns 0))
            :dtend (if he
                       (format "%04d%02d%02dT%02d%02d00" ye me de he (or mne 0))
                     (format "%04d%02d%02dT%02d%02d00" ys ms ds hs (or mns 0)))))))

(defun org-calsync-busy--entry-timestamp ()
  "Parse the first active timestamp in the entry's own body, or nil."
  (save-excursion
    (pcase-let ((`(,start . ,end) (org-calsync--entry-body-bounds)))
      (goto-char start)
      (when (re-search-forward org-tsr-regexp end t)
        (org-timestamp-from-string (match-string 0))))))

(defun org-calsync-busy--vevent (uid ts label stamp)
  "Return a VEVENT carrying only LABEL and TS -- never anything else.

Takes a UID, a time and a label, and has no access to the entry they came
from.  That is deliberate: confidentiality here is a property of the
signature, not of the caller remembering to strip fields."
  (let ((d (org-calsync-busy--ics-times ts))
        (rrule (org-calsync-busy--rrule ts)))
    (concat
     "BEGIN:VEVENT\r\n"
     (format "UID:%s\r\n" uid)
     (format "DTSTAMP:%s\r\n" stamp)
     (if (plist-get d :all-day)
         (format "DTSTART;VALUE=DATE:%s\r\nDTEND;VALUE=DATE:%s\r\n"
                 (plist-get d :dtstart) (plist-get d :dtend))
       (format "DTSTART:%s\r\nDTEND:%s\r\n"
               (plist-get d :dtstart) (plist-get d :dtend)))
     (if rrule (concat rrule "\r\n") "")
     (format "SUMMARY:%s\r\n" label)
     "END:VEVENT\r\n")))

;;;###autoload
(defun org-calsync-export-busy ()
  "Write an abstracted busy ics of the calendar, then offer it to a mailer.

Covers the next `org-calsync-busy-days' days, plus repeating events whatever
their first occurrence.  Each event keeps its `:UID:' so re-importing on the
receiving device updates rather than duplicates; entries without one get a UID
synthesized from their heading and time.

Returns the path written."
  (interactive)
  (let* ((src (org-calsync-file))
         (out (org-calsync-busy-file))
         (label org-calsync-busy-label)
         (now (current-time))
         (dnow (decode-time now))
         (today (encode-time 0 0 0 (nth 3 dnow) (nth 4 dnow) (nth 5 dnow)))
         (win-end (time-add now (* org-calsync-busy-days 24 60 60)))
         (stamp (format-time-string "%Y%m%dT%H%M%SZ" now t))
         (events '()))
    (unless (file-exists-p src)
      (user-error "No calendar file: %s" src))
    (with-current-buffer (find-file-noselect src)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (unless (member (org-entry-get nil "CATEGORY")
                          org-calsync-busy-exclude-categories)
            (let ((ts (org-calsync-busy--entry-timestamp)))
              (when ts
                (let ((start (encode-time
                              0 (or (org-element-property :minute-start ts) 0)
                              (or (org-element-property :hour-start ts) 0)
                              (org-element-property :day-start ts)
                              (org-element-property :month-start ts)
                              (org-element-property :year-start ts))))
                  (when (or (org-element-property :repeater-type ts)
                            (and (not (time-less-p start today))
                                 (time-less-p start win-end)))
                    (let ((uid (or (org-entry-get nil "UID")
                                   (concat "busy-"
                                           (md5 (concat
                                                 (org-get-heading t t t t)
                                                 (or (org-element-property
                                                      :raw-value ts)
                                                     "")))))))
                      (push (org-calsync-busy--vevent uid ts label stamp)
                            events))))))))
        nil)))
    (let ((ics (concat "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n"
                       "PRODID:-//org-calsync//busy//EN\r\n"
                       (apply #'concat (nreverse events))
                       "END:VCALENDAR\r\n"))
          (coding-system-for-write 'utf-8-unix))
      (write-region ics nil out))
    (if (functionp org-calsync-busy-compose-function)
        (funcall org-calsync-busy-compose-function out)
      (message "Busy ics written: %s" out))
    out))

(provide 'org-calsync-busy)

;;; org-calsync-busy.el ends here
