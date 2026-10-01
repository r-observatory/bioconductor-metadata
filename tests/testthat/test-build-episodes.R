# Status episodes from build reports.

# Source update.R if not already loaded.
if (!exists("apply_build_report", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

lines_of <- function(...) {
  parse_build_status_db(paste(c(...), collapse = "\n"))$lines
}

rep_of <- function(at, versions = c(), propagation_read = FALSE, bioc_version = "3.23") {
  list(bioc_version = bioc_version, repo = "bioc", report_at = at,
       versions = versions, propagation_read = propagation_read)
}

test_that("the first report opens one episode per result and skips NA", {
  r <- apply_build_report(empty_build_history(),
                          lines_of("a4#n1#install: OK", "a4#n1#checksrc: NA", "b#n1#install: ERROR"),
                          rep_of(T1, versions = c(a4 = "1.60.0")), empty_build_reports(), exact = 0L)
  h <- r$history
  expect_equal(nrow(h), 2L)
  expect_equal(h$status, c("OK", "ERROR"))
  expect_equal(h$episode_seq, c(1L, 1L))
  expect_equal(h$first_seen, c(T1, T1))
  expect_equal(h$first_seen_exact, c(0L, 0L))
  expect_equal(h$first_version, c("1.60.0", NA))
  expect_equal(r$counts, c(new = 2L, extended = 0L, closed = 0L))
})

test_that("the same result extends, a new one closes 'changed' and opens the next", {
  h <- apply_build_report(empty_build_history(),
                          lines_of("a4#n1#install: OK", "b#n1#install: ERROR"),
                          rep_of(T1), empty_build_reports(), exact = 0L)$history
  r <- apply_build_report(h, lines_of("a4#n1#install: OK", "b#n1#install: OK"),
                          rep_of(T2, versions = c(a4 = "1.60.1", b = "2.0.0")),
                          report_row(T1, "n1"), exact = 1L)
  h2 <- r$history
  a4 <- h2[h2$package == "a4", ]
  expect_equal(nrow(a4), 1L)
  expect_equal(a4$first_seen, T1)
  expect_equal(a4$last_seen, T2)
  expect_equal(a4$last_version, "1.60.1")
  b <- h2[h2$package == "b", ]
  expect_equal(b$episode_seq, c(1L, 2L))
  expect_equal(b$status, c("ERROR", "OK"))
  expect_equal(b$ended_on, c(T2, NA))
  expect_equal(b$end_reason, c("changed", NA))
  expect_equal(b$first_seen_exact, c(0L, 1L))
  expect_equal(r$counts, c(new = 1L, extended = 1L, closed = 1L))
})

test_that("a return to OK after an error opens a third episode", {
  h <- empty_build_history()
  h <- apply_build_report(h, lines_of("b#n1#install: OK"), rep_of(T1), empty_build_reports(), 0L)$history
  h <- apply_build_report(h, lines_of("b#n1#install: ERROR"), rep_of(T2), report_row(T1, "n1"), 1L)$history
  h <- apply_build_report(h, lines_of("b#n1#install: OK"), rep_of(T3),
                          rbind(report_row(T1, "n1"), report_row(T2, "n1")), 1L)$history
  expect_equal(h$episode_seq, 1:3)
  expect_equal(h$status, c("OK", "ERROR", "OK"))
  expect_equal(sum(is.na(h$ended_on)), 1L)
})

test_that("NA leaves the open row untouched, and the next result extends it", {
  h <- empty_build_history()
  h <- apply_build_report(h, lines_of("a4#n2#checksrc: OK", "a4#n1#checksrc: OK"),
                          rep_of(T1), empty_build_reports(), 0L)$history
  r <- apply_build_report(h, lines_of("a4#n2#checksrc: NA", "a4#n1#checksrc: OK"),
                          rep_of(T2), report_row(T1, "n2,n1"), 1L)
  n2 <- r$history[r$history$node == "n2", ]
  expect_equal(n2$last_seen, T1)
  expect_true(is.na(n2$ended_on))
  expect_equal(r$counts[["new"]], 0L)
  h3 <- apply_build_report(r$history, lines_of("a4#n2#checksrc: OK", "a4#n1#checksrc: OK"),
                           rep_of(T3), rbind(report_row(T1, "n2,n1"), report_row(T2, "n2,n1")), 1L)$history
  n2 <- h3[h3$node == "n2", ]
  expect_equal(nrow(n2), 1L)
  expect_equal(n2$last_seen, T3)
})

test_that("a package gone from a report closes 'gone' at that report", {
  h <- apply_build_report(empty_build_history(), lines_of("a4#n1#install: OK", "old#n1#install: OK"),
                          rep_of(T1), empty_build_reports(), 0L)$history
  h2 <- apply_build_report(h, lines_of("a4#n1#install: OK"), rep_of(T2), report_row(T1, "n1"), 1L)$history
  old <- h2[h2$package == "old", ]
  expect_equal(old$ended_on, T2)
  expect_equal(old$end_reason, "gone")
  expect_equal(old$last_seen, T1)
})

test_that("a node missing for six reports stays open and the seventh closes it", {
  day <- function(i) sprintf("2026-09-%02dT17:40:00Z", 10L + i)
  h <- apply_build_report(empty_build_history(),
                          lines_of("a4#n1#install: OK", "a4#arm#install: OK"),
                          rep_of(day(0)), empty_build_reports(), 0L)$history
  reports <- report_row(day(0), "n1,arm")
  for (i in 1:7) {
    h <- apply_build_report(h, lines_of("a4#n1#install: OK"), rep_of(day(i)), reports, 1L)$history
    arm <- h[h$node == "arm", ]
    if (i < 7L) {
      expect_true(is.na(arm$ended_on), label = sprintf("open after %d missing reports", i))
    }
    reports <- rbind(reports, report_row(day(i), "n1"))
  }
  expect_equal(arm$end_reason, "gone")
  expect_equal(arm$ended_on, day(1))
  expect_equal(arm$last_seen, day(0))
})

test_that("propagation rows move only when the propagation file was read", {
  prop <- "a4#source#propagate: NO, package depends on 'X' which is not available"
  h <- apply_build_report(empty_build_history(), lines_of("a4#n1#install: OK", prop),
                          rep_of(T1, propagation_read = TRUE), empty_build_reports(), 0L)$history
  expect_equal(h$detail[h$stage == "propagate"], "package depends on 'X' which is not available")
  h2 <- apply_build_report(h, lines_of("a4#n1#install: OK"),
                           rep_of(T2, propagation_read = FALSE), report_row(T1, "n1"), 1L)$history
  p <- h2[h2$stage == "propagate", ]
  expect_true(is.na(p$ended_on))
  expect_equal(p$last_seen, T1)
  h3 <- apply_build_report(h2, lines_of("a4#n1#install: OK",
                                        "a4#source#propagate: NO, package depends on 'Y' which is not available"),
                           rep_of(T3, propagation_read = TRUE),
                           rbind(report_row(T1, "n1"), report_row(T2, "n1")), 1L)$history
  p <- h3[h3$stage == "propagate", ]
  expect_equal(p$end_reason, c("changed", NA))
  expect_equal(p$detail[2], "package depends on 'Y' which is not available")
})

test_that("carried rows keep their own first_seen and last_seen", {
  # last_seen differs from first_seen and from the new report, so a broken
  # carry-forward cannot pass by coincidence.
  h <- empty_build_history()
  h[1, ] <- list("a4", "3.23", "bioc", "n1", "install", 3L, "OK", NA, "1.0", "1.1",
                 "2026-09-01T17:40:00Z", "2026-09-20T17:40:00Z", 1L, NA, NA)
  h[2, ] <- list("zz", "3.23", "bioc", "n9", "install", 1L, "OK", NA, "1.0", "1.0",
                 "2026-09-02T17:40:00Z", "2026-09-21T17:40:00Z", 1L, NA, NA)
  r <- apply_build_report(h, lines_of("a4#n1#install: NA", "b#n1#install: OK"),
                          rep_of(T3), report_row(T2, "n1,n9"), 1L)
  a4 <- r$history[r$history$package == "a4", ]
  expect_equal(a4$first_seen, "2026-09-01T17:40:00Z")
  expect_equal(a4$last_seen, "2026-09-20T17:40:00Z")
  expect_equal(a4$episode_seq, 3L)
  zz <- r$history[r$history$package == "zz", ]
  expect_equal(zz$last_seen, "2026-09-21T17:40:00Z")
  expect_true(is.na(zz$ended_on))
})

two_versions <- function() {
  h <- apply_build_report(empty_build_history(), lines_of("a4#n1#install: OK"),
                          rep_of(T1), empty_build_reports(), 0L)$history
  apply_build_report(h, lines_of("a4#n2#install: OK"),
                     rep_of(T1, bioc_version = "3.24"), empty_build_reports(), 0L)$history
}
served_of <- function(...) {
  data.frame(repo = "bioc", bioc_version = c(...), stringsAsFactors = FALSE)
}

test_that("a BioC version older than every served one closes 'retired' at the read time", {
  r <- retire_build_versions(two_versions(), served_of("3.24", "3.25"),
                             now = "2026-10-29T06:05:00Z")
  expect_equal(r$closed, 1L)
  old <- r$history[r$history$bioc_version == "3.23", ]
  expect_equal(old$end_reason, "retired")
  expect_equal(old$ended_on, "2026-10-29T06:05:00Z")
  expect_true(is.na(r$history$ended_on[r$history$bioc_version == "3.24"]))
})

test_that("a version between the served ones stays open while the aliases move", {
  # Devel has moved to 3.25 while release still serves 3.23: 3.24 is about to
  # become release and must not be retired in between.
  r <- retire_build_versions(two_versions(), served_of("3.23", "3.25"),
                             now = "2026-10-28T06:05:00Z")
  expect_equal(r$closed, 0L)
  expect_true(all(is.na(r$history$ended_on)))
})

test_that("a repo with nothing known about what it serves is left alone", {
  r <- retire_build_versions(two_versions(), served_of("3.25")[0, ], now = "2026-10-29T06:05:00Z")
  expect_equal(r$closed, 0L)
  other <- data.frame(repo = "workflows", bioc_version = "3.25", stringsAsFactors = FALSE)
  expect_equal(retire_build_versions(two_versions(), other, now = T3)$closed, 0L)
})

test_that("the real 2026-09-29 and 2026-09-30 release reports give exactly 57 changes", {
  read_fx <- function(f) paste(readLines(gzfile(test_path("fixtures", f))), collapse = "\n")
  d1 <- parse_build_status_db(read_fx("build-status-3.23-2026-09-29.txt.gz"))$lines
  d2 <- parse_build_status_db(read_fx("build-status-3.23-2026-09-30.txt.gz"))$lines
  # A day before with kunpeng2's checks present, as they were before the gap.
  d0 <- d1
  na <- d0$status == "NA"
  d0$status[na] <- d2$status[match(build_key(d0[na, ]), build_key(d2))]
  expect_equal(sum(na), 520L)

  A0 <- "2026-09-27T17:45:00Z"; A1 <- "2026-09-28T17:40:00Z"; A2 <- "2026-09-29T17:40:00Z"
  h <- apply_build_report(empty_build_history(), d0, rep_of(A0), empty_build_reports(), 0L)$history
  expect_equal(nrow(h), 14499L)
  r1 <- apply_build_report(h, d1, rep_of(A1), report_row(A0, "nebbiolo1,kunpeng2"), 1L)
  expect_equal(r1$counts, c(new = 0L, extended = 13979L, closed = 0L))
  gap <- r1$history$node == "kunpeng2" & r1$history$stage == "checksrc" &
    r1$history$last_seen == A0
  expect_equal(sum(gap), 520L)

  r2 <- apply_build_report(r1$history, d2, rep_of(A2),
                           rbind(report_row(A0, "nebbiolo1,kunpeng2"),
                                 report_row(A1, "nebbiolo1,kunpeng2")), 1L)
  expect_equal(r2$counts, c(new = 57L, extended = 14442L, closed = 57L))
  h2 <- r2$history
  expect_equal(sum(is.na(h2$ended_on)), 14499L)
  expect_true(all(h2$end_reason[!is.na(h2$ended_on)] == "changed"))
  k <- h2$node == "kunpeng2" & h2$stage == "checksrc" & h2$first_seen == A0
  expect_equal(sum(k & h2$last_seen == A2), sum(k))
})
