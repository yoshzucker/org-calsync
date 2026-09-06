;;; org-calsync.el --- Mirror external calendars into Org, non-destructively  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-calsync
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: calendar, outlines

;; This file is not part of GNU Emacs.

;;; Commentary:

;; External calendars, mirrored into one Org file you can clock and annotate.
;;
;; The problem this solves is not reading a calendar -- Emacs has done that for
;; decades -- but reading it *repeatedly* into a file somebody is also editing.
;; A meeting arrives from the source, you clock it, you write a note under it,
;; you refile it into a project.  The next sync must find that entry, update
;; only what the source owns, and leave the rest alone.  Anything less and the
;; file is either stale or hostile to work in.
;;
;; So three rules run through the whole package:
;;
;;   Identity is the UID.  An entry is matched by `:UID:' wherever it has been
;;   moved to, so a refiled meeting is updated in place rather than duplicated
;;   back into the calendar file.
;;
;;   Only machine-owned fields are written.  Title, event timestamp, and the
;;   `:UID:'/`:CATEGORY:'/`:LOCATION:' properties come from the source.  TODO
;;   state, tags, priority, LOGBOOK and clock, SCHEDULED/DEADLINE, body notes
;;   and sub-headings are yours and are never touched.
;;
;;   A mirror is pruned, but only where it is still a mirror.  A sync removes
;;   entries the source no longer has -- otherwise cancelled meetings linger
;;   forever -- but only those that carry no sign of your having touched them.
;;
;; Sources are pluggable.  The engine takes a list of event plists:
;;
;;   (:uid :title :category :location :timestamp)
;;
;; and `org-calsync-sources' names the functions that produce them.  Reading an
;; ics file is one such producer (org-calsync-ics.el); the two adapters that
;; ship with this package -- Outlook via COM on Windows, Calendar.app via
;; EventKit on macOS -- are two more, and are examples of the interface as
;; much as they are features.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'cl-lib)

(defgroup org-calsync nil
  "Mirror external calendars into Org."
  :group 'org
  :prefix "org-calsync-")

(defcustom org-calsync-file-name "calendar.org"
  "Basename of the aggregated calendar file, under `org-directory'."
  :type 'string
  :group 'org-calsync)

(defcustom org-calsync-feed-property "FEED"
  "Property naming which source owns an entry.

Kept apart from `:CATEGORY:' on purpose.  A category says what a thing is -- a
meeting, a fixture, somebody's birthday -- and it is what the agenda shows in
its narrow left column.  A feed says where it came from, which is bookkeeping:
it is how a sync knows what it may prune, and not something a reader should
have to look at.

Collapsing the two, as this package once did, has two consequences.  The
agenda ends up displaying the name of whatever product the calendar happens to
live in, which ages badly and says nothing about the day.  And a feed can only
ever carry one category, so a single source cannot import a family calendar
and a work calendar and have them read differently."
  :type 'string
  :group 'org-calsync)

(defcustom org-calsync-sources nil
  "Alist of (FEED . PLIST) naming where events come from.

FEED is a symbol identifying the source.  It is written to each imported entry
and is what a sync prunes by, so it must be stable: renaming a feed orphans
everything it ever imported.

PLIST understands:

  :fetch     a function of no arguments returning event plists.  Required.
  :category  the `:CATEGORY:' to give every event from this feed, when the
             fetcher does not set one itself.

Example:

  \\='((work    :fetch org-calsync-outlook-fetch :category \"meeting\")
     (private :fetch org-calsync-macos-fetch))"
  :type '(alist :key-type symbol :value-type plist)
  :group 'org-calsync)

(defun org-calsync-file ()
  "Absolute path of the aggregated calendar file.

Resolved on each call rather than at load: `org-directory' is often set by
configuration that runs after this package is loaded, and a path captured too
early is a path pointing at the wrong place for the rest of the session."
  (expand-file-name org-calsync-file-name
                    (or (bound-and-true-p org-directory) "~/")))

(defun org-calsync--script (name)
  "Return the path of bundled helper script NAME.

Looked for under `script/' beside the loaded library, then directly beside it:
a package manager's file directive may flatten the directory away, and the
adapters should still find their half of the contract.  When neither exists
the expected path is returned all the same, so the caller can say which file
is missing."
  (let* ((dir (file-name-directory (or (locate-library "org-calsync")
                                       (buffer-file-name)
                                       default-directory)))
         (candidates (list (expand-file-name (concat "script/" name) dir)
                           (expand-file-name name dir))))
    (or (seq-find #'file-exists-p candidates)
        (car candidates))))

(defun org-calsync-default-directory ()
  "Where an interactive file prompt should start."
  (if (and (bound-and-true-p org-directory) (file-directory-p org-directory))
      org-directory
    default-directory))

;;;; Writing an entry

(defun org-calsync--insert-entry (f)
  "Insert a brand-new entry for event plist F at point.

The properties are written with `org-entry-put\' rather than as text, which
matters more than it looks: Org aligns property values, so a hand-written
drawer is rewritten the first time anything touches the entry -- and a file
that reformats itself on the next sync produces a diff on every sync forever.
Both paths through this package now go through the same formatter, so a new
entry and a merged one are identical by construction."
  (insert (format "* %s\n%s\n" (plist-get f :title) (plist-get f :timestamp)))
  (save-excursion
    (forward-line -2)
    (org-back-to-heading t)
    (org-entry-put (point) "UID" (plist-get f :uid))
    (when (plist-get f :feed)
      (org-entry-put (point) org-calsync-feed-property
                     (symbol-name (plist-get f :feed))))
    (when (plist-get f :category)
      (org-entry-put (point) "CATEGORY" (plist-get f :category)))
    (when (plist-get f :location)
      (org-entry-put (point) "LOCATION" (plist-get f :location)))))

(defun org-calsync--update-timestamp (ts)
  "Replace this entry's event timestamp with TS, preserving all other content.

Point must be at the heading.  Replaces the first active timestamp or range in
the entry's own body -- after the planning line and drawers, and stopping
before any child -- or inserts TS when none is left.

Bounded that tightly on purpose: a search that ran into the children would
rewrite the time of a sub-heading somebody wrote, which is the one kind of
damage a calendar mirror must never do."
  (org-back-to-heading t)
  (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
         (body-start (save-excursion (org-back-to-heading t)
                                     (org-end-of-meta-data t) (point)))
         (limit (save-excursion
                  (goto-char body-start)
                  (if (re-search-forward org-heading-regexp subtree-end t)
                      (line-beginning-position)
                    subtree-end))))
    (goto-char body-start)
    (if (re-search-forward org-tsr-regexp limit t)
        (replace-match ts t t)
      (goto-char body-start)
      (insert ts "\n"))))

(defun org-calsync--merge (f)
  "Update the entry at point from event plist F, keeping what is yours.

Written: the heading text, the event timestamp, and the `:UID:',
`:CATEGORY:' and `:LOCATION:' properties.  Untouched: TODO state, tags,
priority, LOGBOOK and clock, SCHEDULED and DEADLINE, body notes, sub-headings."
  (org-back-to-heading t)
  (org-edit-headline (plist-get f :title))
  (org-back-to-heading t)
  (org-entry-put (point) "UID" (plist-get f :uid))
  ;; Written on every merge, not only on insert: that is what adopts entries
  ;; imported before feeds existed, so the first sync after an upgrade brings
  ;; the file under the new bookkeeping without anybody being asked.
  (when (plist-get f :feed)
    (org-entry-put (point) org-calsync-feed-property
                   (symbol-name (plist-get f :feed))))
  (if (plist-get f :category)
      (org-entry-put (point) "CATEGORY" (plist-get f :category))
    (org-entry-delete (point) "CATEGORY"))
  (if (plist-get f :location)
      (org-entry-put (point) "LOCATION" (plist-get f :location))
    (org-entry-delete (point) "LOCATION"))
  (org-calsync--update-timestamp (plist-get f :timestamp)))

;;;; Finding and pruning

(defun org-calsync--find-uid (uid cal-buf files)
  "Return (BUFFER . POS) of the entry whose `:UID:' is UID, or nil.

CAL-BUF is searched first because its in-memory state is authoritative during
an import -- entries added moments ago are not on disk yet.  FILES are the
other agenda files, searched so that an event refiled out of the calendar into
a project is updated where it now lives instead of being cloned back."
  (or (with-current-buffer cal-buf
        (org-with-wide-buffer
         (let ((pos (org-find-property "UID" uid)))
           (and pos (cons cal-buf pos)))))
      (catch 'found
        (dolist (file files)
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (let ((pos (org-find-property "UID" uid)))
               (when pos (throw 'found (cons (current-buffer) pos)))))))
        nil)))

(defun org-calsync--pristine-p ()
  "Non-nil when the entry at point is still an untouched mirror.

Pristine means: no TODO keyword, no tags of its own, no child heading, no
LOGBOOK, and a body that is exactly the one event timestamp.  Any of those is
a sign somebody invested something in the entry, and investment outranks the
source: a meeting you clocked is a record of your day even after the meeting
is cancelled."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (and (not (org-get-todo-state))
           (not (org-get-tags nil t))
           (not (save-excursion (org-goto-first-child)))
           (not (save-excursion
                  (org-back-to-heading t)
                  (re-search-forward "^[ \t]*:LOGBOOK:" end t)))
           (save-excursion
             (org-back-to-heading t)
             (org-end-of-meta-data t)
             (string-match-p
              (concat "\\`" org-tsr-regexp "\\'")
              (string-trim (buffer-substring-no-properties (point) end))))))))

(defun org-calsync--own-property (name)
  "Return NAME as the entry\='s own property drawer writes it, or nil.

Read from the drawer rather than with `org-entry-get\=', which treats CATEGORY
as a special property and answers with the file\='s name when no entry claims
one -- so an entry that never had a category would appear to have whichever
one somebody happened to ask about."
  (save-excursion
    (org-back-to-heading t)
    (let ((range (org-get-property-block)))
      (when range
        (goto-char (car range))
        (when (re-search-forward
               (format "^[ \t]*:%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote name))
               (cdr range) t)
          (let ((value (match-string-no-properties 1)))
            (unless (string-empty-p value) value)))))))

(defun org-calsync--prune (feed keep-uids)
  "Delete pristine FEED entries here whose UID is not in KEEP-UIDS.

Keyed on the feed rather than the category, so one source may import several
kinds of thing and still own all of them.  An entry with no feed is never
pruned: it predates this bookkeeping or was written by hand, and either way
nothing here can claim to know it should be gone.

Returns how many were removed."
  (let ((feed-name (symbol-name feed))
        stale (removed 0))
    (goto-char (point-min))
    ;; Scope nil = this (widened) buffer only.  The `file' scope would ask
    ;; about agenda files while a brand-new calendar file is still unsaved.
    (org-map-entries
     (lambda ()
       (let ((uid (org-entry-get nil "UID")))
         (when (and uid
                    (equal (org-entry-get nil org-calsync-feed-property)
                           feed-name)
                    (not (member uid keep-uids))
                    (org-calsync--pristine-p))
           (push uid stale))))
     nil)
    (dolist (uid stale)
      (let ((pos (org-find-property "UID" uid)))
        (when pos
          (goto-char pos)
          (org-back-to-heading t)
          (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
          (cl-incf removed))))
    removed))

;;;###autoload
(defun org-calsync-adopt (feed &optional category)
  "Give FEED to the calendar file's entries that have a UID but no feed.

A sync adopts an entry the moment the source still has it, so this is only
for the remainder: entries imported before feeds existed, or by an older
version of this package, whose source has already forgotten them.  Without a
feed nothing may prune them, and they would sit in the file forever under
whatever category they were imported with.

With CATEGORY, only entries carrying that `:CATEGORY:' are claimed, which is
how one feed can be given its own share of a file that several once wrote to.
Interactively, a prefix argument asks for it.

Adoption only says who owns them; the next sync still refuses to remove any
entry you have clocked, tagged, or written under."
  (interactive
   (list (intern (completing-read
                  "Adopt into feed: "
                  (mapcar (lambda (s) (symbol-name (car s))) org-calsync-sources)
                  nil nil))
         (when current-prefix-arg
           (read-string "Only entries whose CATEGORY is: "))))
  (let ((buf (find-file-noselect (org-calsync-file)))
        (adopted 0))
    (with-current-buffer buf
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (when (and (org-entry-get nil "UID")
                     (not (org-entry-get nil org-calsync-feed-property))
                     (or (null category)
                         (equal category
                                (org-calsync--own-property "CATEGORY"))))
            (org-entry-put nil org-calsync-feed-property (symbol-name feed))
            (cl-incf adopted)))
        nil))
      (when (buffer-modified-p) (save-buffer)))
    (when (called-interactively-p 'interactive)
      (message "org-calsync: %d entr%s now owned by %s"
               adopted (if (= adopted 1) "y" "ies") feed))
    adopted))

;;;; Applying a fetch

(defun org-calsync-apply (events &optional feed sync)
  "Merge EVENTS -- a list of event plists -- into the calendar file.

Each event is upserted by its `:UID:': found anywhere in the agenda files it
is updated in place, otherwise it is appended to the calendar file.  Only
machine-owned fields are written; see `org-calsync--merge'.

FEED, a symbol, is recorded on every entry written.  With SYNC non-nil the
result is a full mirror of it: entries of that feed whose UID is absent from
EVENTS are pruned, but only where they are still untouched mirrors.  Without
SYNC nothing is ever removed, which is what makes importing a one-off ics file
safe -- a file that arrived by mail is a fragment of somebody's calendar, not
a statement about what no longer exists.

Returns (ADDED UPDATED SKIPPED REMOVED)."
  (let* ((cal-file (org-calsync-file))
         (cal-buf (find-file-noselect cal-file))
         ;; The calendar file is excluded here and searched through CAL-BUF,
         ;; whose unsaved state is the truth during this run.
         (other-files (seq-filter #'file-exists-p
                                  (delete cal-file
                                          (and (fboundp 'org-agenda-files)
                                               (copy-sequence (org-agenda-files))))))
         (touched (list cal-buf))
         (added 0) (updated 0) (skipped 0) (removed 0)
         (seen '()))
    ;; The header goes into an empty file only, so it is never duplicated.
    (with-current-buffer cal-buf
      (org-with-wide-buffer
       (when (= (buffer-size) 0)
         (insert "#+title: Calendar (imported events; safe to clock/annotate/refile)\n"
                 "# Managed by org-calsync: a sync writes only title, time and place.\n\n"))))
    (dolist (f events)
      (if (null f)
          (cl-incf skipped)
        (setq f (if feed (plist-put (copy-sequence f) :feed feed) f))
        (push (plist-get f :uid) seen)
        (let ((loc (org-calsync--find-uid (plist-get f :uid) cal-buf other-files)))
          (if loc
              (with-current-buffer (car loc)
                (cl-pushnew (current-buffer) touched)
                (org-with-wide-buffer
                 (goto-char (cdr loc))
                 (org-calsync--merge f))
                (cl-incf updated))
            (with-current-buffer cal-buf
              (org-with-wide-buffer
               (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (org-calsync--insert-entry f))
              (cl-incf added))))))
    (when (and sync feed)
      (with-current-buffer cal-buf
        (org-with-wide-buffer
         (setq removed (org-calsync--prune feed seen)))))
    (dolist (b touched)
      (when (buffer-live-p b)
        (with-current-buffer b
          (when (buffer-modified-p) (save-buffer)))))
    (list added updated skipped removed)))

;;;; Syncing a source

(defvar org-calsync-after-sync-hook nil
  "Hook run after a feed has been synced into the calendar file.

A sync can create the calendar file where there was none, which a
configuration that builds `org-agenda-files' by hand has to be told
about.  What that costs -- a rescan of a directory, a cache rebuilt --
belongs to the configuration, not here.")

;;;###autoload
(defun org-calsync (&optional feed)
  "Sync FEED from `org-calsync-sources' into the calendar file.

A full mirror: what the source no longer has is pruned, so cancelled meetings
do not linger.  Interactively, prompts for the feed when there is more than
one."
  (interactive)
  (let* ((feed (or feed
                   (if (= 1 (length org-calsync-sources))
                       (caar org-calsync-sources)
                     (intern (completing-read
                              "Feed: "
                              (mapcar (lambda (s) (symbol-name (car s)))
                                      org-calsync-sources)
                              nil t)))))
         (spec (cdr (assq feed org-calsync-sources)))
         (fetch (plist-get spec :fetch)))
    (unless spec (user-error "No such feed: %s" feed))
    (unless (functionp fetch) (user-error "Feed %s has no :fetch" feed))
    (let* ((category (plist-get spec :category))
           (events (mapcar (lambda (f)
                             (if (and category (null (plist-get f :category)))
                                 (plist-put (copy-sequence f) :category category)
                               f))
                           (funcall fetch))))
      (pcase-let ((`(,a ,u ,s ,r) (org-calsync-apply events feed t)))
        (run-hooks 'org-calsync-after-sync-hook)
        (message "%s: +%d ~%d -%d skip %d" feed a u r s)
        (list a u s r)))))

(provide 'org-calsync)

;;; org-calsync.el ends here
