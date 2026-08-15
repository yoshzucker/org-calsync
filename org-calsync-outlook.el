;;; org-calsync-outlook.el --- Outlook as an org-calsync source  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-calsync

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Windows, and an Outlook that is already running.
;;
;; Deliberately COM and not Graph: COM attaches to the copy of Outlook on the
;; desk, so it needs no application registration, no tenant consent and no
;; token, and the calendar never leaves the machine.  On a managed company
;; laptop that is often the difference between a thing you can use and a thing
;; you must ask for.
;;
;; The PowerShell that does the reading ships in this package's `script/'
;; directory rather than beside it.  Its UID scheme and its RFC5545 escaping
;; are half of a contract whose other half is org-calsync-ics.el, and a
;; contract split across two repositories is one that drifts.

;;; Code:

(require 'org-calsync)
(require 'org-calsync-ics)

(defcustom org-calsync-outlook-days 30
  "Days ahead to export from Outlook."
  :type 'integer
  :group 'org-calsync)

(defcustom org-calsync-outlook-back 1
  "Days behind to include.

More than none, so a meeting that moved earlier this morning is still seen
and updated rather than left as it was."
  :type 'integer
  :group 'org-calsync)

(defcustom org-calsync-outlook-script nil
  "Path to the exporter, or nil to use the one shipped with this package."
  :type '(choice (const :tag "the bundled script" nil) file)
  :group 'org-calsync)

(defun org-calsync-outlook--script ()
  "Return the exporter's path, resolved from the package when unset."
  (or org-calsync-outlook-script
      (org-calsync--script "outlook-export.ps1")))

;;;###autoload
(defun org-calsync-outlook-fetch ()
  "Return today's Outlook calendar as `org-calsync' events.

Runs the bundled PowerShell into a temporary ics and reads it back.  The file
is deleted whatever happens: it is a verbatim copy of a work calendar, and
leaving one in the temp directory is the sort of thing that is nobody's
problem until it is."
  (unless (eq system-type 'windows-nt)
    (user-error "The Outlook source needs Windows and an installed Outlook"))
  (let ((script (org-calsync-outlook--script))
        (out (make-temp-file "org-calsync-" nil ".ics")))
    (unless (file-exists-p script)
      (user-error "Exporter not found: %s" script))
    (unwind-protect
        (let ((rc (call-process
                   "powershell" nil "*org-calsync-outlook*" nil
                   "-NoProfile" "-ExecutionPolicy" "Bypass" "-File" script
                   "-OutFile" out
                   "-Days" (number-to-string org-calsync-outlook-days)
                   "-Back" (number-to-string org-calsync-outlook-back))))
          (unless (and (integerp rc) (= rc 0))
            (user-error "Outlook export failed (rc=%s); see *org-calsync-outlook*"
                        rc))
          (unless (> (or (file-attribute-size (file-attributes out)) 0) 0)
            (user-error "Outlook export produced no data"))
          (org-calsync-ics-events out))
      (when (file-exists-p out) (delete-file out)))))

;;;###autoload
(defun org-calsync-outlook-compose (attachment)
  "Open an Outlook compose window with ATTACHMENT, without sending it.

For `org-calsync-busy-compose-function'.  A plain mailto: URL cannot carry a
file, which is the only reason COM is involved.  The mail is displayed, never
sent: what leaves the company machine, and to whom, stays a decision somebody
makes by hand."
  (let ((script (org-calsync--script "outlook-compose.ps1")))
    (if (not (file-exists-p script))
        (message "Busy ics written: %s (compose script missing: %s)"
                 attachment script)
      (call-process "powershell" nil "*org-calsync-outlook*" nil
                    "-NoProfile" "-ExecutionPolicy" "Bypass" "-File" script
                    "-Attachment" attachment
                    "-Subject" (bound-and-true-p org-calsync-busy-label))
      (message "Busy ics written; a compose window is open: %s" attachment))))

(provide 'org-calsync-outlook)

;;; org-calsync-outlook.el ends here
