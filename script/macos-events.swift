// Read events from the macOS calendar store for org-calsync.
//
// Emitted as one line per event, tab-separated:
//
//   calendar<TAB>uid<TAB>allday<TAB>Y M D h m<TAB>Y M D h m<TAB>title<TAB>location
//
// Dates are written as numbers.  The alternative -- letting the system format
// them -- produces the user's locale ("2026年1月5日 月曜日 午前9:00:00"), and
// anything parsing that is parsing locale settings rather than a date.
//
// EventKit rather than AppleScript, which was measured and not assumed: over
// the same four calendars and thirty-two days, AppleScript took anywhere from
// 8 to 43 seconds -- unpredictable as much as slow, because every property
// read crosses a process boundary.  This runs in 40 milliseconds.  It costs a
// one-off 2-second compile and a working Swift toolchain, which is the whole
// of the trade.
//
// Output goes to a file rather than stdout because this is built into an app
// bundle and launched through LaunchServices; see org-calsync-macos.el for why
// that is the only way the permission dialog ever appears.  A path of "-"
// writes to stdout, which is how to run it by hand.
//
// Arguments: out-path backDays aheadDays calendarName...

import EventKit
import Foundation

var out = "-"

func emit(_ text: String) {
    if out == "-" {
        FileHandle.standardOutput.write(text.data(using: .utf8)!)
    } else {
        try? text.write(toFile: out, atomically: true, encoding: .utf8)
    }
}

// Failures travel by the same route as the events.  Launched through `open',
// this process has no stderr the caller can see and its exit status is the
// launcher's, not ours -- so a message nobody receives is a message that turns
// into an empty calendar and a puzzled morning.
func fail(_ message: String) -> Never {
    emit("!ERROR\t" + message + "\n")
    FileHandle.standardError.write((message + "\n").data(using: .utf8)!)
    exit(2)
}

let args = Array(CommandLine.arguments.dropFirst())
guard args.count >= 4 else {
    fail("usage: macos-events OUT-PATH BACK-DAYS AHEAD-DAYS CALENDAR...")
}
out = args[0]
guard let back = Int(args[1]), let ahead = Int(args[2]) else {
    fail("usage: macos-events OUT-PATH BACK-DAYS AHEAD-DAYS CALENDAR...")
}
let wanted = Set(args.dropFirst(3))

let store = EKEventStore()
let waiting = DispatchSemaphore(value: 0)
var granted = false
if #available(macOS 14.0, *) {
    store.requestFullAccessToEvents { ok, _ in granted = ok; waiting.signal() }
} else {
    store.requestAccess(to: .event) { ok, _ in granted = ok; waiting.signal() }
}
waiting.wait()
guard granted else {
    // Denied rather than crashed: the caller turns this into an instruction,
    // because the fix is in System Settings and not in anything Emacs can do.
    fail("org-calsync: no access to calendars")
}

let cal = Calendar.current
guard let start = cal.date(byAdding: .day, value: -abs(back), to: Date()),
      let end = cal.date(byAdding: .day, value: ahead, to: Date()) else {
    fail("org-calsync: could not build the date range")
}

let calendars = store.calendars(for: .event).filter { wanted.contains($0.title) }
guard !calendars.isEmpty else {
    fail("org-calsync: none of those calendars exist: "
         + wanted.sorted().joined(separator: ", "))
}

func parts(_ date: Date) -> String {
    let c = cal.dateComponents([.year, .month, .day, .hour, .minute], from: date)
    return "\(c.year!) \(c.month!) \(c.day!) \(c.hour!) \(c.minute!)"
}

// Tabs and newlines are the record separators, so they cannot survive inside a
// field.  Nothing else is touched: the text reaches Org as it was written.
func clean(_ s: String?) -> String {
    guard let s else { return "" }
    return s.map { ($0 == "\t" || $0.isNewline) ? " " : $0 }
        .reduce(into: "") { $0.append($1) }
}

let predicate = store.predicateForEvents(withStart: start, end: end,
                                         calendars: calendars)
var lines: [String] = []
for event in store.events(matching: predicate) {
    guard let uid = event.eventIdentifier, let from = event.startDate else { continue }
    let to = event.endDate ?? from
    lines.append([event.calendar.title,
                  uid,
                  event.isAllDay ? "1" : "0",
                  parts(from),
                  parts(to),
                  clean(event.title),
                  clean(event.location)].joined(separator: "\t"))
}
emit(lines.isEmpty ? "" : lines.joined(separator: "\n") + "\n")
