;;; org-calsync-macos.el --- Calendar.app as an org-calsync source  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-calsync

;; This file is not part of GNU Emacs.

;;; Commentary:

;; macOS, reading the calendar store through EventKit.
;;
;; There is no file to read.  The old `~/Library/Calendars/*.calendar/Events/'
;; layout, which held one ics per event, is gone from modern macOS -- what is
;; left under that path on a long-lived machine is a decade of .icbu backups.
;; So the store has to be asked, and there are two ways to ask it.
;;
;; AppleScript is the one with no dependencies, and it was tried first.  Over
;; the same four calendars and thirty-two days it took anywhere from 8 to 43
;; seconds: every property read crosses a process boundary, so the cost scales
;; with events times fields and swings with whatever else Calendar.app is
;; doing.  Batching the reads, the usual fix, does not work here -- a property
;; of a materialised list of event specifiers is an error.
;;
;; EventKit answers the same question in 40 milliseconds, deterministically.
;; It costs a Swift toolchain and a one-off two-second compile, which this
;; file does on demand and caches.  That is the whole trade, and it is only
;; worth stating because the alternative is not obviously worse until measured.
;;
;; The reader is built as an app bundle and launched through LaunchServices,
;; which looks like ceremony and is not.  macOS attributes a privacy request to
;; the *responsible* process, and for a program Emacs spawns that is Emacs
;; itself.  Emacs.app ships no `NSCalendarsUsageDescription', so the system has
;; no sentence to put in the dialog, and a request it cannot phrase is one it
;; refuses outright -- with no prompt, no entry in System Settings, and nothing
;; to grant afterwards.  The same binary run from a terminal succeeds, because
;; the terminal was granted access once, which makes the failure look like a
;; bug in the code rather than a rule about who is deemed to be asking.
;;
;; A bundle started with `open' is launched by the system and is therefore
;; responsible for itself.  It carries its own usage description, so the dialog
;; can be shown, and it then appears in System Settings under its own name
;; where the answer can be changed later.  Nothing about Emacs.app is touched:
;; patching another application's Info.plist would invalidate its signature,
;; drop the permissions it already holds, and be undone by its next update.
;;
;; Calendars are named rather than taken wholesale.  A stock macOS carries a
;; weather feed, a holidays feed, birthdays and Siri's guesses, and an agenda
;; showing tomorrow's forecast beside tomorrow's meetings is one that stops
;; being read.

;;; Code:

(require 'org-calsync)

(defcustom org-calsync-macos-calendars nil
  "Alist of (CALENDAR-NAME . CATEGORY) to import from Calendar.app.

Named one at a time, and each with the `:CATEGORY:' its events should carry --
which is how one source can bring in a family calendar and a work one and have
them read differently in the agenda.

  \\='((\"Personal\" . \"personal\")
    (\"Family\"   . \"family\"))

A name that does not exist is an error rather than a silent omission: a
calendar that was renamed would otherwise stop importing without anything
being said about it."
  :type '(alist :key-type string :value-type string)
  :group 'org-calsync)

(defcustom org-calsync-macos-days 60
  "Days ahead to read from Calendar.app."
  :type 'integer
  :group 'org-calsync)

(defcustom org-calsync-macos-back 2
  "Days behind to include.

More than none, so an event that moved earlier this week is still seen and
updated rather than left as it was."
  :type 'integer
  :group 'org-calsync)

(defcustom org-calsync-macos-cache-directory
  (locate-user-emacs-file "org-calsync/")
  "Where the compiled reader is kept.

Not the package directory: that is a checkout somebody may have made
read-only, and on a package manager that rebuilds from a clean tree a binary
left there is deleted on the next update anyway."
  :type 'directory
  :group 'org-calsync)

(defun org-calsync-macos--source ()
  "Return the path of the Swift reader shipped with this package."
  (org-calsync--script "macos-events.swift"))

(defconst org-calsync-macos--bundle-id "io.github.yoshzucker.org-calsync"
  "Bundle identifier of the reader.

Half of how macOS remembers the answer to the permission dialog, so changing
it asks again.")

(defun org-calsync-macos--info-plist ()
  "Return the reader bundle's Info.plist.

The usage descriptions are the point of the bundle: they are the sentence the
permission dialog shows, and without one the system does not ask at all.  Both
spellings are given -- macOS 14 split full access out into its own key -- and
`LSUIElement' keeps a program that lives for 40 milliseconds out of the Dock
and out of the window layer."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\""
   " \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
   "<plist version=\"1.0\">\n<dict>\n"
   "  <key>CFBundleIdentifier</key><string>"
   org-calsync-macos--bundle-id "</string>\n"
   "  <key>CFBundleName</key><string>org-calsync</string>\n"
   "  <key>CFBundleExecutable</key><string>org-calsync</string>\n"
   "  <key>CFBundlePackageType</key><string>APPL</string>\n"
   "  <key>CFBundleInfoDictionaryVersion</key><string>6.0</string>\n"
   "  <key>CFBundleShortVersionString</key><string>0.1</string>\n"
   "  <key>CFBundleVersion</key><string>1</string>\n"
   "  <key>LSUIElement</key><true/>\n"
   "  <key>NSCalendarsUsageDescription</key>"
   "<string>org-calsync copies your calendar into an Org file"
   " so it shows up in the agenda.</string>\n"
   "  <key>NSCalendarsFullAccessUsageDescription</key>"
   "<string>org-calsync copies your calendar into an Org file"
   " so it shows up in the agenda.</string>\n"
   "</dict>\n</plist>\n"))

(defun org-calsync-macos--app ()
  "Return the reader's app bundle, building it when missing or out of date.

Rebuilt when the source is newer, so editing the reader is enough -- there is
no separate step to remember and no stale binary to be puzzled by.  A rebuild
changes the signature the system recognises, so the permission dialog comes
back once afterwards."
  (let* ((src (org-calsync-macos--source))
         (app (expand-file-name "org-calsync-reader.app"
                                org-calsync-macos-cache-directory))
         (bin (expand-file-name "Contents/MacOS/org-calsync" app)))
    (unless (file-exists-p src)
      (user-error "org-calsync: reader source missing: %s" src))
    (when (or (not (file-exists-p bin))
              (file-newer-than-file-p src bin))
      (unless (executable-find "swiftc")
        (user-error
         "org-calsync: swiftc not found.  Install the command line tools with `xcode-select --install'"))
      (make-directory (file-name-directory bin) t)
      (with-temp-file (expand-file-name "Contents/Info.plist" app)
        (insert (org-calsync-macos--info-plist)))
      (with-temp-buffer
        (unless (zerop (call-process "swiftc" nil t nil "-O" "-o" bin src))
          (user-error "org-calsync: could not build the reader: %s"
                      (string-trim (buffer-string)))))
      ;; Signed, even if only ad-hoc: an unsigned bundle has no stable identity
      ;; for the system to attach the answer to, and one that is asked afresh
      ;; every morning is one nobody keeps saying yes to.
      (with-temp-buffer
        (unless (zerop (call-process "codesign" nil t nil "--force" "--sign" "-"
                                     (directory-file-name app)))
          (user-error "org-calsync: could not sign the reader: %s"
                      (string-trim (buffer-string))))))
    app))

(defun org-calsync-macos--parse-line (line)
  "Return the `org-calsync' event plist for one output LINE, or nil.

The reader writes dates as numbers, so nothing here has to know what a date
looks like in the user's language."
  (let ((f (split-string line "\t")))
    (when (>= (length f) 7)
      (pcase-let* ((`(,cal ,uid ,allday ,from ,to ,title ,location) f)
                   (`(,y1 ,m1 ,d1 ,h1 ,mi1)
                    (mapcar #'string-to-number (split-string from " ")))
                   (`(,y2 ,m2 ,d2 ,h2 ,mi2)
                    (mapcar #'string-to-number (split-string to " "))))
        (list :uid uid
              :title (if (string-empty-p title) "(no title)" title)
              :category (cdr (assoc cal org-calsync-macos-calendars))
              :location (unless (string-empty-p location) location)
              :timestamp (org-calsync-macos--timestamp
                          (equal allday "1")
                          (list y1 m1 d1 h1 mi1) (list y2 m2 d2 h2 mi2)))))))

(defun org-calsync-macos--timestamp (all-day from to)
  "Return an Org active timestamp for FROM and TO, lists of Y M D h m.

An all-day event's end comes back as the last day itself, not the exclusive
next one that ics uses, so nothing is taken off it here.  Getting this
backwards is how an imported calendar ends up a day long everywhere, which is
why it is said out loud in both readers."
  (let* ((start (encode-time 0 (nth 4 from) (nth 3 from)
                             (nth 2 from) (nth 1 from) (nth 0 from)))
         (end (encode-time 0 (nth 4 to) (nth 3 to)
                           (nth 2 to) (nth 1 to) (nth 0 to)))
         (same-day (equal (format-time-string "%Y-%m-%d" start)
                          (format-time-string "%Y-%m-%d" end))))
    (cond
     (all-day
      (if same-day
          (format "<%s>" (format-time-string "%Y-%m-%d %a" start))
        (format "<%s>--<%s>"
                (format-time-string "%Y-%m-%d %a" start)
                (format-time-string "%Y-%m-%d %a" end))))
     (same-day
      (format "<%s-%s>"
              (format-time-string "%Y-%m-%d %a %H:%M" start)
              (format-time-string "%H:%M" end)))
     (t
      (format "<%s>--<%s>"
              (format-time-string "%Y-%m-%d %a %H:%M" start)
              (format-time-string "%Y-%m-%d %a %H:%M" end))))))

;;;###autoload
(defun org-calsync-macos-fetch ()
  "Return the named Calendar.app calendars as `org-calsync' events.

The reader is launched with `open' and writes to a file, because a bundle
started any other way is not responsible for itself and can never be granted
anything; see the commentary.  The file is deleted whatever happens -- it is a
copy of a calendar, and one of those left in the temp directory is the sort of
thing that is nobody's problem until it is.

The first run raises the permission dialog, and `open' waits for the answer."
  (unless (eq system-type 'darwin)
    (user-error "The Calendar.app source needs macOS"))
  (unless org-calsync-macos-calendars
    (user-error "Set `org-calsync-macos-calendars' first: no calendar is named"))
  (let ((app (org-calsync-macos--app))
        (out (make-temp-file "org-calsync-macos-" nil ".tsv")))
    (unwind-protect
        (with-temp-buffer
          (let ((rc (apply #'call-process
                           "open" nil t nil
                           "-W" "-n" "-a" (directory-file-name app) "--args"
                           out
                           (number-to-string org-calsync-macos-back)
                           (number-to-string org-calsync-macos-days)
                           (mapcar #'car org-calsync-macos-calendars))))
            (unless (and (integerp rc) (= rc 0))
              (user-error "org-calsync: could not start the reader: %s"
                          (string-trim (buffer-string)))))
          (erase-buffer)
          (insert-file-contents out)
          (goto-char (point-min))
          (when (looking-at "!ERROR\t\\(.*\\)")
            (let ((message (match-string 1)))
              (user-error
               "%s"
               (if (string-match-p "no access" message)
                   (concat "org-calsync: the reader has not been allowed to "
                           "read calendars.  System Settings -> Privacy & "
                           "Security -> Calendars -> org-calsync")
                 message))))
          (seq-keep #'org-calsync-macos--parse-line
                    (split-string (buffer-string) "\n" t)))
      (when (file-exists-p out) (delete-file out)))))

(provide 'org-calsync-macos)

;;; org-calsync-macos.el ends here
