;;; org-calsync-test.el --- Tests for org-calsync  -*- lexical-binding: t; -*-

;;; Commentary:

;; What these fix in place, in order of how much damage getting them wrong
;; does: that a sync never eats work somebody put into the file, that the
;; busy export never leaks a word, and that dates survive the round trip.
;;
;; Run:
;;   emacs --batch -Q -L . -L test -l test/org-calsync-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-calsync)
(require 'org-calsync-ics)
(require 'org-calsync-busy)
(require 'org-calsync-macos)

;;;; Fixtures

(defmacro org-calsync-test--with-dir (&rest body)
  "Run BODY with `org-directory' and the agenda pointed at a scratch dir."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "org-calsync" t)))
          (org-directory dir)
          (org-agenda-files nil)
          (org-calsync-file-name "calendar.org")
          (org-todo-keywords '((sequence "NEXT" "|" "DONE"))))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun org-calsync-test--ics (&rest vevents)
  "Write VEVENTS into a temporary ics file and return its path."
  (let ((f (make-temp-file "org-calsync" nil ".ics")))
    (with-temp-file f
      (insert "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//test//EN\n")
      (apply #'insert vevents)
      (insert "END:VCALENDAR\n"))
    f))

(defun org-calsync-test--vevent (uid summary dtstart &optional dtend extra)
  "Return one VEVENT block."
  (concat "BEGIN:VEVENT\n"
          (format "UID:%s\n" uid)
          (format "SUMMARY:%s\n" summary)
          (format "DTSTART%s\n" dtstart)
          (if dtend (format "DTEND%s\n" dtend) "")
          (or extra "")
          "END:VEVENT\n"))

(defun org-calsync-test--soon (&optional days)
  "Return an Org date string DAYS ahead (default 7), with its real weekday.

The busy export only looks forward, so its fixtures cannot use a fixed past
date -- and a fixed future one would be a date whose weekday nobody checked.
Computed, so the tests say the same thing whenever they are run."
  (format-time-string "%Y-%m-%d %a"
                      (time-add (current-time)
                                (* (or days 7) 24 60 60))))

(defun org-calsync-test--soon-ics (&optional days)
  "Return the same day as `org-calsync-test--soon\=', in ics form."
  (format-time-string "%Y%m%d"
                      (time-add (current-time)
                                (* (or days 7) 24 60 60))))

(defun org-calsync-test--calendar ()
  "Return the calendar file's text."
  (let ((f (org-calsync-file)))
    (if (file-exists-p f)
        (with-temp-buffer (insert-file-contents f) (buffer-string))
      "")))

;;;; Times

(ert-deftest org-calsync-test-timed-event ()
  "A timed event becomes one timestamp with a start and an end."
  (org-calsync-test--with-dir
    (org-calsync-import-file
     (org-calsync-test--ics
      (org-calsync-test--vevent "a" "Standup" ":20260810T093000"
                                ":20260810T094500"))
     "work")
    (should (string-match-p "<2026-08-10 Mon 09:30-09:45>"
                            (org-calsync-test--calendar)))))

(ert-deftest org-calsync-test-all-day-end-is-inclusive ()
  "An all-day DTEND is exclusive in ics and inclusive in Org.

The single most common way an imported calendar ends up one day long
everywhere, so it is worth a test of its own: a one-day event must not become
a two-day range, and a three-day event must end on its last day."
  (org-calsync-test--with-dir
    (org-calsync-import-file
     (org-calsync-test--ics
      (org-calsync-test--vevent "one" "Holiday" ";VALUE=DATE:20260810"
                                ";VALUE=DATE:20260811")
      (org-calsync-test--vevent "three" "Trip" ";VALUE=DATE:20260812"
                                ";VALUE=DATE:20260815"))
     "life")
    (let ((text (org-calsync-test--calendar)))
      ;; one day: a plain stamp, not a range
      (should (string-match-p "<2026-08-10 Mon>\n" text))
      (should-not (string-match-p "<2026-08-10 Mon>--" text))
      ;; three days: ends on the 14th, not the 15th
      (should (string-match-p "<2026-08-12 Wed>--<2026-08-14 Fri>" text)))))

(ert-deftest org-calsync-test-multi-day-timed-event ()
  "A timed event crossing midnight becomes a range of two stamps."
  (org-calsync-test--with-dir
    (org-calsync-import-file
     (org-calsync-test--ics
      (org-calsync-test--vevent "n" "Night shift" ":20260810T220000"
                                ":20260811T060000"))
     "work")
    (should (string-match-p "<2026-08-10 Mon 22:00>--<2026-08-11 Tue 06:00>"
                            (org-calsync-test--calendar)))))

(ert-deftest org-calsync-test-rrule-becomes-a-repeater ()
  "Simple frequencies map to Org repeaters; anything else keeps the frequency."
  (should (equal " +1w" (org-calsync-ics--rrule->repeater "FREQ=WEEKLY")))
  (should (equal " +2d" (org-calsync-ics--rrule->repeater "FREQ=DAILY;INTERVAL=2")))
  (should (equal " +1m" (org-calsync-ics--rrule->repeater "FREQ=MONTHLY")))
  ;; BYDAY has no Org equivalent: approximated by the frequency, not dropped
  (should (equal " +1w" (org-calsync-ics--rrule->repeater
                         "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=8")))
  ;; and a frequency Org cannot say at all is not guessed at
  (should-not (org-calsync-ics--rrule->repeater "FREQ=SECONDLY"))
  (should-not (org-calsync-ics--rrule->repeater nil)))

(ert-deftest org-calsync-test-text-is-unescaped-and-flattened ()
  "RFC5545 escapes are reversed and newlines collapsed: a heading is one line."
  (should (equal "a, b; c d"
                 (org-calsync-ics--clean "a\\, b\\; c\\nd")))
  (should (equal "one two"
                 (org-calsync-ics--clean "one\r\n   two"))))

;;;; What a sync must never do

(ert-deftest org-calsync-test-merge-keeps-what-is-yours ()
  "A sync writes title, time and place, and touches nothing else.

This is the rule the whole package rests on.  An entry somebody clocked,
tagged, scheduled and wrote under has to survive the meeting being moved."
  (org-calsync-test--with-dir
    (let ((ics (org-calsync-test--ics
                (org-calsync-test--vevent "m" "Review" ":20260810T140000"
                                          ":20260810T150000"))))
      (org-calsync-import-file ics "work")
      ;; invest in the entry the way a person would
      (with-current-buffer (find-file-noselect (org-calsync-file))
        (goto-char (point-min))
        (search-forward "Review")
        (org-back-to-heading t)
        (org-todo "NEXT")
        (org-set-tags '("prep"))
        (org-schedule nil "2026-08-09 Sun")
        (org-end-of-meta-data t)
        (forward-line 1)
        (insert "a note of my own\n")
        (insert "** a child heading\n")
        (save-buffer))
      ;; the meeting moves an hour
      (org-calsync-import-file
       (org-calsync-test--ics
        (org-calsync-test--vevent "m" "Review (moved)" ":20260810T150000"
                                  ":20260810T160000"))
       "work")
      (let ((text (org-calsync-test--calendar)))
        ;; the source owns these
        (should (string-match-p "Review (moved)" text))
        (should (string-match-p "<2026-08-10 Mon 15:00-16:00>" text))
        ;; and none of these
        (should (string-match-p "NEXT" text))
        (should (string-match-p ":prep:" text))
        (should (string-match-p "SCHEDULED: <2026-08-09 Sun>" text))
        (should (string-match-p "a note of my own" text))
        (should (string-match-p "\\*\\* a child heading" text))))))

(ert-deftest org-calsync-test-import-is-idempotent ()
  "Importing the same file twice changes nothing -- not even the header."
  (org-calsync-test--with-dir
    (let ((ics (org-calsync-test--ics
                (org-calsync-test--vevent "a" "One" ":20260810T090000")
                (org-calsync-test--vevent "b" "Two" ":20260810T100000"))))
      (org-calsync-import-file ics "work")
      (let ((first (org-calsync-test--calendar)))
        (org-calsync-import-file ics "work")
        ;; byte-identical, not merely equivalent: a file that reformats itself
        ;; on the next sync produces a diff on every sync forever
        (should (equal first (org-calsync-test--calendar)))))))

(ert-deftest org-calsync-test-header-is-written-once ()
  "The file header goes into an empty file and is never repeated."
  (org-calsync-test--with-dir
    (let ((ics (org-calsync-test--ics
                (org-calsync-test--vevent "a" "One" ":20260810T090000"))))
      (org-calsync-import-file ics "work")
      (org-calsync-import-file ics "work")
      (let ((text (org-calsync-test--calendar))
            (n 0) (start 0))
        (while (string-match "#\\+title:" text start)
          (setq n (1+ n) start (match-end 0)))
        (should (= 1 n))))))

(ert-deftest org-calsync-test-sync-prunes-only-untouched-mirrors ()
  "A sync removes what the source dropped -- unless somebody touched it.

Without pruning, cancelled meetings accumulate forever.  With unconditional
pruning, an hour you clocked against a meeting disappears when the meeting is
cancelled, which is a record of your day being deleted to tidy a mirror."
  (org-calsync-test--with-dir
    (org-calsync-apply
     (org-calsync-ics-events
      (org-calsync-test--ics
       (org-calsync-test--vevent "keep" "Kept" ":20260810T090000")
       (org-calsync-test--vevent "gone" "Cancelled" ":20260810T100000")
       (org-calsync-test--vevent "clocked" "Attended" ":20260810T110000"))
      "work")
     'work t)
    ;; one of them gets clocked
    (with-current-buffer (find-file-noselect (org-calsync-file))
      (goto-char (point-min))
      (search-forward "Attended")
      (org-back-to-heading t)
      (org-end-of-meta-data t)
      (insert ":LOGBOOK:\n"
              "CLOCK: [2026-08-10 Mon 11:00]--[2026-08-10 Mon 11:30] =>  0:30\n"
              ":END:\n")
      (save-buffer))
    ;; the source now has only the first
    (org-calsync-apply
     (org-calsync-ics-events
      (org-calsync-test--ics
       (org-calsync-test--vevent "keep" "Kept" ":20260810T090000"))
      "work")
     'work t)
    (let ((text (org-calsync-test--calendar)))
      (should (string-match-p "Kept" text))
      (should-not (string-match-p "Cancelled" text))
      (should (string-match-p "Attended" text)))))

(ert-deftest org-calsync-test-prune-removes-the-entry-it-judged ()
  "With two entries sharing a UID, the untouched one goes, not the first one.

Duplicates happen -- a refile interrupted halfway, a copy pasted by hand --
and the entry judged pristine must be the entry deleted.  Looking the UID up
again afterwards finds whichever comes first, which may be the clocked one."
  (org-calsync-test--with-dir
    (with-temp-file (org-calsync-file)
      (insert "* Attended\n:PROPERTIES:\n:UID: dup\n:FEED: work\n:END:\n"
              ":LOGBOOK:\n"
              "CLOCK: [2026-08-10 Mon 11:00]--[2026-08-10 Mon 11:30] =>  0:30\n"
              ":END:\n"
              "<2026-08-10 Mon 11:00>\n"
              "* Mirror copy\n:PROPERTIES:\n:UID: dup\n:FEED: work\n:END:\n"
              "<2026-08-10 Mon 11:00>\n"))
    (org-calsync-apply nil 'work t)
    (let ((text (org-calsync-test--calendar)))
      (should (string-match-p "Attended" text))
      (should-not (string-match-p "Mirror copy" text)))))

(ert-deftest org-calsync-test-feed-prunes-across-categories ()
  "One source may import several kinds of thing and still own all of them.

Pruning used to key on the category, which meant a feed could carry exactly
one -- and it meant the category, the thing the agenda displays, had to be the
name of wherever the calendar happened to live."
  (org-calsync-test--with-dir
    (org-calsync-apply
     (list (list :uid "a" :title "A meeting" :category "meeting"
                 :timestamp "<2026-08-10 Mon 09:00>")
           (list :uid "b" :title "A fixture" :category "family"
                 :timestamp "<2026-08-10 Mon 18:00>"))
     'phone t)
    ;; both are written, with different categories and the one feed
    (let ((text (org-calsync-test--calendar)))
      (should (string-match-p ":CATEGORY: meeting" text))
      (should (string-match-p ":CATEGORY: family" text))
      (should (= 2 (cl-count-if (lambda (l) (string-match-p ":FEED: *phone" l))
                                (split-string text "\n")))))
    ;; the source drops both: one feed, two categories, both pruned
    (org-calsync-apply nil 'phone t)
    (let ((text (org-calsync-test--calendar)))
      (should-not (string-match-p "A meeting" text))
      (should-not (string-match-p "A fixture" text)))))

(ert-deftest org-calsync-test-another-feed-is-left-alone ()
  "A sync owns its own feed and nothing else."
  (org-calsync-test--with-dir
    (org-calsync-apply
     (list (list :uid "mine" :title "Mine" :category "meeting"
                 :timestamp "<2026-08-10 Mon 09:00>"))
     'work t)
    (org-calsync-apply
     (list (list :uid "theirs" :title "Theirs" :category "meeting"
                 :timestamp "<2026-08-10 Mon 10:00>"))
     'phone t)
    ;; work syncs to empty; the phone's entry is not its business
    (org-calsync-apply nil 'work t)
    (let ((text (org-calsync-test--calendar)))
      (should-not (string-match-p "Mine" text))
      (should (string-match-p "Theirs" text)))))

(ert-deftest org-calsync-test-entries-without-a-feed-are-never-pruned ()
  "What predates the bookkeeping, or was written by hand, is not ours to drop.

The first sync after an upgrade adopts whatever it can match by UID.  What it
cannot match it leaves: an entry with no feed is one nothing here can claim to
know should be gone."
  (org-calsync-test--with-dir
    ;; imported before feeds existed: a UID and a category, no feed
    (with-temp-file (org-calsync-file)
      (insert "* Old import\n:PROPERTIES:\n:UID: old\n:CATEGORY: outlook\n"
              ":END:\n<2026-08-10 Mon 09:00>\n"
              "* Hand written\n:PROPERTIES:\n:UID: hand\n:END:\n"
              "<2026-08-10 Mon 10:00>\n"))
    (org-calsync-apply nil 'work t)
    (let ((text (org-calsync-test--calendar)))
      (should (string-match-p "Old import" text))
      (should (string-match-p "Hand written" text)))
    ;; but a sync that still has it adopts it: the feed and the new category
    ;; are written on the merge, so the next sync owns it
    (org-calsync-apply
     (list (list :uid "old" :title "Old import" :category "meeting"
                 :timestamp "<2026-08-10 Mon 09:00>"))
     'work t)
    (let ((text (org-calsync-test--calendar)))
      (should (string-match-p ":FEED: *work" text))
      (should (string-match-p ":CATEGORY: meeting" text))
      (should-not (string-match-p ":CATEGORY: outlook" text)))
    ;; and now it is prunable
    (org-calsync-apply nil 'work t)
    (should-not (string-match-p "Old import" (org-calsync-test--calendar)))))

(ert-deftest org-calsync-test-adopt-claims-only-unowned-entries ()
  "Adoption reaches what a sync cannot: entries the source has already forgotten.

And it stops there -- an entry owned by another feed is not taken from it, and
one written by hand without a UID is not claimed at all."
  (org-calsync-test--with-dir
    (with-temp-file (org-calsync-file)
      (insert "* Old import\n:PROPERTIES:\n:UID: old\n:CATEGORY: outlook\n"
              ":END:\n<2026-08-10 Mon 09:00>\n"
              "* Someone else's\n:PROPERTIES:\n:UID: other\n:CATEGORY: club\n"
              ":FEED: private\n:END:\n<2026-08-10 Mon 10:00>\n"
              "* Hand written\n<2026-08-10 Mon 11:00>\n"))
    (should (= 1 (org-calsync-adopt 'work "outlook")))
    (with-temp-buffer
      (insert-file-contents (org-calsync-file))
      (goto-char (org-find-property "UID" "old"))
      (should (equal "work" (org-entry-get nil "FEED")))
      (goto-char (org-find-property "UID" "other"))
      (should (equal "private" (org-entry-get nil "FEED")))
      (should (string-match-p "Hand written" (buffer-string))))
    ;; and once owned, the next sync may retire it
    (org-calsync-apply nil 'work t)
    (should-not (string-match-p "Old import" (org-calsync-test--calendar)))))

(ert-deftest org-calsync-test-adopt-does-not-guess-a-category ()
  "A file-level category is not a category the entry has.

`org-entry-get' answers CATEGORY with the file's name when no drawer says
otherwise, so a naive read would claim every entry in `calendar.org' the
moment somebody asked for the ones categorised `calendar'."
  (org-calsync-test--with-dir
    (with-temp-file (org-calsync-file)
      (insert "* No category of its own\n:PROPERTIES:\n:UID: bare\n:END:\n"
              "<2026-08-10 Mon 09:00>\n"))
    (should (= 0 (org-calsync-adopt 'work "calendar")))))

(ert-deftest org-calsync-test-import-never-prunes ()
  "A one-off file import is a fragment, not a statement about what is gone."
  (org-calsync-test--with-dir
    (org-calsync-import-file
     (org-calsync-test--ics
      (org-calsync-test--vevent "a" "One" ":20260810T090000")
      (org-calsync-test--vevent "b" "Two" ":20260810T100000"))
     "work")
    (org-calsync-import-file
     (org-calsync-test--ics
      (org-calsync-test--vevent "a" "One" ":20260810T090000"))
     "work")
    (should (string-match-p "Two" (org-calsync-test--calendar)))))

(ert-deftest org-calsync-test-refiled-event-is-updated-in-place ()
  "An event moved into another file is updated there, not cloned back."
  (org-calsync-test--with-dir
    (let ((project (expand-file-name "project.org" org-directory))
          (ics (org-calsync-test--ics
                (org-calsync-test--vevent "r" "Kickoff" ":20260810T090000"))))
      (with-temp-file project
        (insert "* Kickoff\n:PROPERTIES:\n:UID: r\n:CATEGORY: work\n:END:\n"
                "<2026-08-10 Mon 09:00>\n"))
      (let ((org-agenda-files (list project)))
        (org-calsync-import-file
         (org-calsync-test--ics
          (org-calsync-test--vevent "r" "Kickoff (moved)" ":20260810T110000"))
         "work")
        (ignore ics)
        ;; updated where it lives ...
        (with-temp-buffer
          (insert-file-contents project)
          (should (string-match-p "Kickoff (moved)" (buffer-string)))
          (should (string-match-p "<2026-08-10 Mon 11:00>" (buffer-string))))
        ;; ... and not copied back into the calendar
        (should-not (string-match-p "Kickoff" (org-calsync-test--calendar)))))))

(ert-deftest org-calsync-test-undated-event-is-dropped ()
  "An event with no usable start is skipped, not placed at an invented hour."
  (org-calsync-test--with-dir
    (let ((events (org-calsync-ics-events
                   (org-calsync-test--ics
                    (concat "BEGIN:VEVENT\nUID:x\nSUMMARY:No date\n"
                            "END:VEVENT\n"))
                   "work")))
      (should (equal '(nil) events))
      (pcase-let ((`(,a ,_ ,s ,_) (org-calsync-apply events "work" nil)))
        (should (= 0 a))
        (should (= 1 s))))))

;;;; Bundled scripts

(ert-deftest org-calsync-test-bundled-scripts-are-found ()
  "Each adapter finds its half of the contract.

The scripts share an escaping rule, a UID scheme and an all-day convention
with the Lisp, which is why they ship here.  That only helps if they are still
findable once a package manager has installed the package, so the resolution
is asserted rather than assumed."
  (dolist (name '("outlook-export.ps1" "outlook-compose.ps1"
                  "macos-events.swift"))
    (should (file-exists-p (org-calsync--script name)))))

(ert-deftest org-calsync-test-scripts-are-found-when-flattened ()
  "A recipe that drops the `script/' directory is survived.

MELPA's file directive maps a bare wildcard onto the package root, so an
install can legitimately end up with the helpers beside the Lisp rather than
under it.  Resolution falls back to that layout instead of failing at the
moment somebody runs a sync."
  (let* ((dir (file-name-as-directory (make-temp-file "org-calsync-flat" t)))
         (load-path (cons dir load-path)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "org-calsync.el" dir)
            (insert ";;; org-calsync.el --- stand-in  -*- lexical-binding: t; -*-\n"))
          (with-temp-file (expand-file-name "macos-events.swift" dir) (insert ""))
          (should (equal (expand-file-name "macos-events.swift" dir)
                         (org-calsync--script "macos-events.swift"))))
      (delete-directory dir t))))

;;;; The macOS reader

(ert-deftest org-calsync-test-macos-parses-numeric-dates ()
  "The reader writes dates as numbers, so nothing here reads a locale.

The output is parsed rather than the store queried: `EKEventStore\' needs a
real calendar and a permission dialog, and neither belongs in a test.  What is
worth fixing is the shape of the line and the four kinds of time it can carry."
  (let ((org-calsync-macos-calendars '(("Personal" . "personal")
                                       ("Family" . "family"))))
    ;; a timed event inside one day
    (let ((e (org-calsync-macos--parse-line
              (mapconcat #'identity
                         '("Personal" "uid-1" "0"
                           "2026 1 5 9 0" "2026 1 5 10 0"
                           "打ち合わせ" "会議室A")
                         "\t"))))
      (should (equal "uid-1" (plist-get e :uid)))
      (should (equal "打ち合わせ" (plist-get e :title)))
      (should (equal "personal" (plist-get e :category)))
      (should (equal "会議室A" (plist-get e :location)))
      (should (equal "<2026-01-05 Mon 09:00-10:00>" (plist-get e :timestamp))))
    ;; an all-day event over several days: the end is the last day itself,
    ;; not the exclusive next one ics uses
    (let ((e (org-calsync-macos--parse-line
              (mapconcat #'identity
                         '("Family" "uid-2" "1"
                           "2026 8 8 0 0" "2026 8 16 23 59" "Away" "")
                         "\t"))))
      (should (equal "<2026-08-08 Sat>--<2026-08-16 Sun>"
                     (plist-get e :timestamp)))
      (should-not (plist-get e :location)))
    ;; one all-day day is a stamp, not a range
    (let ((e (org-calsync-macos--parse-line
              (mapconcat #'identity
                         '("Family" "uid-3" "1"
                           "2026 8 10 0 0" "2026 8 10 23 59" "Holiday" "")
                         "\t"))))
      (should (equal "<2026-08-10 Mon>" (plist-get e :timestamp))))
    ;; and a timed event crossing midnight is a range of two stamps
    (let ((e (org-calsync-macos--parse-line
              (mapconcat #'identity
                         '("Personal" "uid-4" "0"
                           "2026 8 12 15 0" "2026 8 14 11 0" "Trip" "")
                         "\t"))))
      (should (equal "<2026-08-12 Wed 15:00>--<2026-08-14 Fri 11:00>"
                     (plist-get e :timestamp))))))

(ert-deftest org-calsync-test-macos-untitled-event-still-has-a-heading ()
  "An event with no title gets a placeholder: a blank heading is not an entry."
  (let ((org-calsync-macos-calendars '(("Personal" . "personal"))))
    (should (equal "(no title)"
                   (plist-get (org-calsync-macos--parse-line
                               (mapconcat #'identity
                                          '("Personal" "u" "0"
                                            "2026 8 14 9 0" "2026 8 14 10 0"
                                            "" "")
                                          "\t"))
                              :title)))))

(ert-deftest org-calsync-test-macos-bundle-can-be-asked-for-permission ()
  "The reader's bundle carries what the permission dialog needs to be shown.

Without a usage description macOS does not ask -- it refuses, with no prompt
and no entry in System Settings -- and the failure looks like a bug in the
code rather than a missing string.  Both spellings are required: macOS 14 put
full access behind its own key, and the older one still answers for macOS 13.
The bundle identifier is what the answer is remembered against, so it is
fixed here too."
  (let ((plist (org-calsync-macos--info-plist)))
    (should (string-match-p "NSCalendarsUsageDescription" plist))
    (should (string-match-p "NSCalendarsFullAccessUsageDescription" plist))
    (should (string-match-p "io\\.github\\.yoshzucker\\.org-calsync" plist))
    ;; and the dialog says what it is for, in words somebody can act on
    (should (string-match-p "org-calsync copies your calendar" plist))))

(ert-deftest org-calsync-test-macos-short-line-is-dropped ()
  "A line that is not an event is skipped rather than half-read."
  (should-not (org-calsync-macos--parse-line "not\tan\tevent")))

;;;; The busy export

(ert-deftest org-calsync-test-busy-export-leaks-nothing ()
  "The abstracted export carries a label and a time.  Nothing else.

This is the property the whole work/private bridge rests on, so it is checked
against the file itself rather than against the function that built it."
  (org-calsync-test--with-dir
    (let ((org-calsync-busy-label "Busy")
          (org-calsync-busy-days 3650))
      (with-temp-file (org-calsync-file)
        (insert "* Acme Corp contract negotiation\n"
                ":PROPERTIES:\n:UID: secret-1\n:CATEGORY: work\n"
                ":LOCATION: Acme HQ, room 12\n:END:\n"
                (format "<%s 14:00-15:30>\n" (org-calsync-test--soon))
                "a private note about the deal\n"))
      (let* ((out (org-calsync-export-busy))
             (text (with-temp-buffer (insert-file-contents out) (buffer-string)))
             (day (org-calsync-test--soon-ics)))
        (should (string-match-p "SUMMARY:Busy" text))
        (should (string-match-p (format "DTSTART:%sT140000" day) text))
        (should (string-match-p (format "DTEND:%sT153000" day) text))
        (dolist (leak '("Acme" "contract" "negotiation" "room 12"
                        "private note" "LOCATION" "DESCRIPTION"))
          (should-not (string-match-p (regexp-quote leak) text)))))))

(ert-deftest org-calsync-test-busy-export-reads-only-the-entry-itself ()
  "A child's time is exported once, under the child, never under its parent.

A parent with no time of its own is not an event; reading into its children
would export the child's time under the parent's UID as well."
  (org-calsync-test--with-dir
    (let ((org-calsync-busy-days 3650))
      (with-temp-file (org-calsync-file)
        (insert "* Project\n:PROPERTIES:\n:UID: parent\n:END:\n"
                "** Kickoff\n:PROPERTIES:\n:UID: child\n:END:\n"
                (format "<%s 14:00-15:00>\n" (org-calsync-test--soon))))
      (let* ((out (org-calsync-export-busy))
             (text (with-temp-buffer (insert-file-contents out) (buffer-string))))
        (should (string-match-p "UID:child" text))
        (should-not (string-match-p "UID:parent" text))))))

(ert-deftest org-calsync-test-busy-export-skips-excluded-categories ()
  "Categories the receiving device already has are left out."
  (org-calsync-test--with-dir
    (let ((org-calsync-busy-days 3650)
          (org-calsync-busy-exclude-categories '("family")))
      (with-temp-file (org-calsync-file)
        (insert "* Work thing\n:PROPERTIES:\n:UID: w\n:CATEGORY: work\n:END:\n"
                (format "<%s 14:00-15:00>\n" (org-calsync-test--soon))
                "* Family thing\n:PROPERTIES:\n:UID: f\n:CATEGORY: family\n:END:\n"
                (format "<%s 18:00-19:00>\n" (org-calsync-test--soon))))
      (let* ((out (org-calsync-export-busy))
             (text (with-temp-buffer (insert-file-contents out) (buffer-string)))
             (n 0) (start 0))
        (while (string-match "BEGIN:VEVENT" text start)
          (setq n (1+ n) start (match-end 0)))
        (should (= 1 n))
        (should (string-match-p "UID:w" text))
        (should-not (string-match-p "UID:f" text))))))

(ert-deftest org-calsync-test-busy-export-round-trips-a-repeater ()
  "A repeating event exports as an RRULE and reads back as the same repeater."
  (org-calsync-test--with-dir
    (let ((org-calsync-busy-days 3650))
      (with-temp-file (org-calsync-file)
        (insert "* Weekly\n:PROPERTIES:\n:UID: r\n:CATEGORY: work\n:END:\n"
                (format "<%s 09:00-09:30 +1w>\n" (org-calsync-test--soon))))
      (let* ((out (org-calsync-export-busy))
             (text (with-temp-buffer (insert-file-contents out) (buffer-string))))
        (should (string-match-p "RRULE:FREQ=WEEKLY;INTERVAL=1" text))
        ;; and back again
        (should (equal " +1w"
                       (org-calsync-ics--rrule->repeater
                        "FREQ=WEEKLY;INTERVAL=1")))))))

(ert-deftest org-calsync-test-busy-export-all-day-end-is-exclusive ()
  "An all-day event exports with the exclusive next-day DTEND ics expects."
  (org-calsync-test--with-dir
    (let ((org-calsync-busy-days 3650))
      (with-temp-file (org-calsync-file)
        (insert "* Away\n:PROPERTIES:\n:UID: a\n:CATEGORY: work\n:END:\n"
                (format "<%s>\n" (org-calsync-test--soon))))
      (let* ((out (org-calsync-export-busy))
             (text (with-temp-buffer (insert-file-contents out) (buffer-string))))
        (should (string-match-p (format "DTSTART;VALUE=DATE:%s"
                                        (org-calsync-test--soon-ics))
                                text))
        ;; exclusive: the day after
        (should (string-match-p (format "DTEND;VALUE=DATE:%s"
                                        (org-calsync-test--soon-ics 8))
                                text))))))

(provide 'org-calsync-test)

;;; org-calsync-test.el ends here
