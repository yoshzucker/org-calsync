;;; org-calsync-ics.el --- Read ics into org-calsync events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-calsync

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One producer of `org-calsync' events: an ics file, read with the icalendar
;; parser Emacs already ships.  No network, no extra runtime.
;;
;; DESCRIPTION is deliberately dropped.  A meeting body is usually a wall of
;; dial-in boilerplate and somebody else's signature, and an agenda that
;; carries it is an agenda nobody reads twice.

;;; Code:

(require 'org-calsync)
(require 'icalendar)

(defun org-calsync-ics--clean (s)
  "Unescape RFC5545 TEXT S and flatten it to one heading-safe line."
  (let ((r (or s "")))
    (setq r (replace-regexp-in-string "\\\\[nN]" " " r t t))       ; \n \N -> space
    (setq r (replace-regexp-in-string "\\\\\\([,;]\\)" "\\1" r t)) ; \, \; -> , ;
    (string-trim (replace-regexp-in-string "[ \t]*[\r\n]+[ \t]*" " " r))))

(defun org-calsync-ics--rrule->repeater (rrule)
  "Return an Org repeater (with leading space) for ical RRULE, or nil.

Only plain FREQ/INTERVAL is mapped.  BYDAY lists, COUNT and UNTIL have no Org
equivalent, so they are approximated by the frequency alone rather than
guessed at -- a repeater that is roughly right is more useful in an agenda
than an event that silently occurs once."
  (when (and rrule (stringp rrule))
    (let* ((up (upcase rrule))
           (freq (when (string-match "FREQ=\\([A-Z]+\\)" up)
                   (match-string 1 up)))
           (interval (if (string-match "INTERVAL=\\([0-9]+\\)" up)
                         (string-to-number (match-string 1 up))
                       1))
           (unit (pcase freq
                   ("DAILY" "d") ("WEEKLY" "w")
                   ("MONTHLY" "m") ("YEARLY" "y") (_ nil))))
      (when unit (format " +%d%s" interval unit)))))

(defun org-calsync-ics--timestamp (dtstart dtend attrs rrule)
  "Build an Org active timestamp from ical DTSTART/DTEND.

ATTRS is DTSTART's attribute list, which is how an all-day event announces
itself.  An all-day DTEND is exclusive per RFC5545 while Org's range end is
inclusive, so a day is taken off it -- the single most common way an imported
calendar ends up one day long everywhere."
  (let* ((all-day (or (member "DATE" attrs)
                      (not (string-match-p "T" dtstart))))
         (start (encode-time (icalendar--decode-isodatetime dtstart)))
         (end (and dtend (encode-time (icalendar--decode-isodatetime dtend))))
         (rep (or (org-calsync-ics--rrule->repeater rrule) "")))
    (if all-day
        (let* ((incl-end (and end (time-subtract end 86400)))
               (start-day (format-time-string "%Y-%m-%d" start))
               (end-day (and incl-end (format-time-string "%Y-%m-%d" incl-end))))
          (if (and end-day (not (string= start-day end-day)))
              (format "<%s>--<%s>"
                      (format-time-string "%Y-%m-%d %a" start)
                      (format-time-string "%Y-%m-%d %a" incl-end))
            (format "<%s%s>" (format-time-string "%Y-%m-%d %a" start) rep)))
      (let ((start-day (format-time-string "%Y-%m-%d" start))
            (end-day (and end (format-time-string "%Y-%m-%d" end))))
        (cond
         ((null end)
          (format "<%s%s>" (format-time-string "%Y-%m-%d %a %H:%M" start) rep))
         ((string= start-day end-day)
          (format "<%s-%s%s>"
                  (format-time-string "%Y-%m-%d %a %H:%M" start)
                  (format-time-string "%H:%M" end)
                  rep))
         (t
          (format "<%s%s>--<%s>"
                  (format-time-string "%Y-%m-%d %a %H:%M" start)
                  rep
                  (format-time-string "%Y-%m-%d %a %H:%M" end))))))))

(defun org-calsync-ics--event (event category)
  "Return the `org-calsync' event plist for ical EVENT, or nil when undated.

An event with no usable DTSTART is dropped rather than placed at some default
hour: a calendar entry whose time is invented is worse than one that is
missing, because only the second is noticed."
  (let* ((dtstart (icalendar--get-event-property event 'DTSTART))
         (ts (and dtstart
                  (ignore-errors
                    (org-calsync-ics--timestamp
                     dtstart
                     (icalendar--get-event-property event 'DTEND)
                     (icalendar--get-event-property-attributes event 'DTSTART)
                     (icalendar--get-event-property event 'RRULE))))))
    (when ts
      (let ((summary (org-calsync-ics--clean
                      (or (icalendar--get-event-property event 'SUMMARY)
                          "(no title)"))))
        (list :uid (or (icalendar--get-event-property event 'UID)
                       ;; No UID means no identity across imports, so one is
                       ;; synthesized from what cannot change without the
                       ;; event becoming a different event.
                       (concat "nouid-" (md5 (concat dtstart summary))))
              :title summary
              :category category
              :location (let ((l (icalendar--get-event-property event 'LOCATION)))
                          (and l (not (string-empty-p l))
                               (org-calsync-ics--clean l)))
              :timestamp ts)))))

(defun org-calsync-ics-events (file &optional category)
  "Return the `org-calsync' events in ics FILE, tagged with CATEGORY."
  (let* ((ical (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (icalendar--read-element nil nil))))
    (mapcar (lambda (e) (org-calsync-ics--event e category))
            (icalendar--all-events ical))))

;;;###autoload
(defun org-calsync-import-file (file &optional category)
  "Import ics FILE into the calendar file, adding and updating only.

Nothing is pruned: a file that arrived by mail is a fragment of somebody's
calendar, not a statement about what no longer exists.  Use `org-calsync' for
a feed that owns its whole category.

CATEGORY defaults to FILE's basename; a prefix argument prompts for it."
  (interactive
   (list (read-file-name "ICS file: " (org-calsync-default-directory) nil t nil
                         (lambda (n) (or (file-directory-p n)
                                         (string-suffix-p ".ics" n))))
         (when current-prefix-arg (read-string "Category: "))))
  (let ((category (or category (file-name-base file))))
    (pcase-let ((`(,a ,u ,s ,r)
                 (org-calsync-apply (org-calsync-ics-events file category)
                                    nil nil)))
      (when (called-interactively-p 'interactive)
        (message "ics import [%s]: +%d ~%d skip %d" category a u s))
      (list a u s r))))

(provide 'org-calsync-ics)

;;; org-calsync-ics.el ends here
