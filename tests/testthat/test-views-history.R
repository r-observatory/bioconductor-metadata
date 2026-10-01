# VIEWS field values kept as episodes.

# Source update.R if not already loaded.
if (!exists("apply_views_state", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

read_views_fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")
}

state <- function(pkg, field, value, category = "software") {
  data.frame(package = pkg, field = field, value = value, category = category,
             stringsAsFactors = FALSE)
}

L1 <- c(software = "2026-09-28T18:14:30Z", annotation = "2026-09-20T10:00:00Z")
L2 <- c(software = "2026-09-29T18:14:30Z", annotation = "2026-09-20T10:00:00Z")
L3 <- c(software = "2026-09-30T18:14:30Z", annotation = "2026-09-20T10:00:00Z")

test_that("views_state_rows keeps Version, Date/Publication, PackageStatus and the source, Windows and macOS .ver fields", {
  s <- views_state_rows(read_views_fixture("views-software-3.23-fields.dcf"), "software")
  expect_setequal(unique(s$field), c("Version", "Date/Publication", "PackageStatus",
                                     "source.ver", "win.binary.ver",
                                     "mac.binary.big-sur-x86_64.ver",
                                     "mac.binary.sonoma-arm64.ver"))
  cr <- s[s$package == "cummeRbund", ]
  expect_equal(cr$value[cr$field == "PackageStatus"], "Deprecated")
  expect_false("source.ver" %in% cr$field)
  expect_false("win.binary.ver" %in% s$field[s$package == "ballgown"])
  expect_equal(s$value[s$package == "Rhtslib" & s$field == "mac.binary.sonoma-arm64.ver"],
               "bin/macosx/sonoma-arm64/contrib/4.6/Rhtslib_3.8.0.tgz")
  expect_true(all(s$category == "software"))
})

test_that("views_state_rows takes the 3.22 binary field name as written", {
  s <- views_state_rows(read_views_fixture("views-software-3.22-rhtslib.dcf"), "software")
  expect_true("mac.binary.big-sur-arm64.ver" %in% s$field)
  expect_false("mac.binary.sonoma-arm64.ver" %in% s$field)
})

test_that("views_state_rows is empty for empty or unreadable text", {
  expect_equal(nrow(views_state_rows("", "software")), 0L)
  expect_equal(nrow(views_state_rows(NULL, "software")), 0L)
})

test_that("first VIEWS opens censored episodes timed by its Last-Modified", {
  r <- apply_views_state(empty_views_history(),
                         rbind(state("A", "Version", "1.0"), state("A", "PackageStatus", "Deprecated")),
                         L1, apply = c("software", "annotation"), bioc_version = "3.23")
  h <- r$history
  expect_equal(nrow(h), 2L)
  expect_equal(h$first_seen, rep(L1[["software"]], 2))
  expect_equal(h$first_seen_exact, c(0L, 0L))
  expect_equal(h$episode_seq, c(1L, 1L))
  expect_equal(r$counts, c(new = 2L, extended = 0L, closed = 0L))
})

test_that("a Deprecated flip with no version change opens an exact episode", {
  h <- apply_views_state(empty_views_history(), state("A", "Version", "1.0"),
                         L1, "software", "3.23")$history
  r <- apply_views_state(h, rbind(state("A", "Version", "1.0"),
                                  state("A", "PackageStatus", "Deprecated")),
                         L2, "software", "3.23")
  h2 <- r$history
  ver <- h2[h2$field == "Version", ]
  expect_equal(ver$last_seen, L2[["software"]])
  expect_equal(ver$first_seen, L1[["software"]])
  dep <- h2[h2$field == "PackageStatus", ]
  expect_equal(dep$first_seen, L2[["software"]])
  expect_equal(dep$first_seen_exact, 1L)
})

test_that("a changed value closes at the new file's time and opens the next episode", {
  h <- apply_views_state(empty_views_history(), state("A", "Version", "1.0"),
                         L1, "software", "3.23")$history
  h <- apply_views_state(h, state("A", "Version", "1.0.1"), L2, "software", "3.23")$history
  expect_equal(h$episode_seq, 1:2)
  expect_equal(h$ended_on, c(L2[["software"]], NA))
  expect_equal(h$value, c("1.0", "1.0.1"))
})

test_that("an absent field closes; a missing binary shows as a closed episode", {
  h <- apply_views_state(empty_views_history(),
                         rbind(state("A", "Version", "1.0"), state("A", "win.binary.ver", "a.zip")),
                         L1, "software", "3.23")$history
  r <- apply_views_state(h, state("A", "Version", "1.0"), L2, "software", "3.23")
  win <- r$history[r$history$field == "win.binary.ver", ]
  expect_equal(win$ended_on, L2[["software"]])
  expect_equal(win$last_seen, L1[["software"]])
  expect_equal(r$counts[["closed"]], 1L)
})

test_that("the release rollover turns over a value that did not change", {
  h <- apply_views_state(empty_views_history(), state("A", "PackageStatus", "Deprecated"),
                         L1, "software", "3.23")$history
  h <- apply_views_state(h, state("A", "PackageStatus", "Deprecated"), L2, "software", "3.24")$history
  expect_equal(h$bioc_version, c("3.23", "3.24"))
  expect_equal(h$ended_on, c(L2[["software"]], NA))
})

test_that("categories outside apply, and a stale file, carry forward untouched", {
  h <- apply_views_state(empty_views_history(),
                         rbind(state("A", "Version", "1.0"), state("B", "Version", "2.0", "annotation")),
                         L2, c("software", "annotation"), "3.23")$history
  r <- apply_views_state(h, state("A", "Version", "9.9"), L1, c("software", "annotation"), "3.23")
  expect_equal(r$skipped, "software")
  expect_equal(r$history$value[r$history$package == "A"], "1.0")
  expect_true(is.na(r$history$ended_on[r$history$package == "A"]))
  # annotation's file is not older, so its absent B closes.
  expect_equal(r$history$ended_on[r$history$package == "B"], L1[["annotation"]])
  r2 <- apply_views_state(h, state("A", "Version", "9.9"), L3, "annotation", "3.23")
  expect_equal(r2$history$value[r2$history$package == "A"], "1.0")
  expect_true(is.na(r2$history$ended_on[r2$history$package == "A"]))
})

test_that("a new package in a category already held opens an exact episode", {
  h <- apply_views_state(empty_views_history(), state("A", "Version", "1.0"),
                         L1, "software", "3.23")$history
  h <- apply_views_state(h, rbind(state("A", "Version", "1.0"), state("New", "Version", "0.1")),
                         L2, "software", "3.23")$history
  expect_equal(h$first_seen_exact[h$package == "New"], 1L)
})
