# Report identity: which reads apply, which change nothing, and which the
# health floor skips.

# Source update.R if not already loaded.
if (!exists("build_report_verdict", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

verdict <- function(reports, at, sha, published, n, version = "3.23") {
  build_report_verdict(reports, version, "bioc", at, sha, published, n)
}

test_that("the first report of a version applies unless it is empty", {
  expect_equal(verdict(empty_build_reports(), T1, "s", T1, 10L), "applied")
  expect_equal(verdict(empty_build_reports(), T1, "s", T1, 0L), "skipped_floor")
})

test_that("a report not newer than the last applied one changes nothing", {
  prior <- report_row(T2, "n1", n_packages = 100L)
  expect_equal(verdict(prior, T2, "other", "2026-09-30T16:00:00Z", 100L), "unchanged")
  expect_equal(verdict(prior, T1, "other", "2026-09-30T16:00:00Z", 100L), "unchanged")
})

test_that("the same file read again changes nothing when its time came from another source", {
  # Applied with the index's snapshot time; read again with the index broken,
  # so report_at falls back to Last-Modified and looks newer.
  published <- "2026-09-28T20:35:46Z"
  prior <- report_row(T2, "n1", n_packages = 100L, published_at = published,
                      status_sha256 = "abc")
  expect_equal(verdict(prior, published, "abc", published, 100L), "unchanged")
})

test_that("a newer report with the same bytes still applies", {
  # A weekly workflows report whose every result held: its rows must extend.
  prior <- report_row(T2, "n1", n_packages = 28L, published_at = "2026-09-28T01:24:06Z",
                      status_sha256 = "abc")
  expect_equal(verdict(prior, T3, "abc", "2026-09-29T01:24:06Z", 28L), "applied")
})

test_that("a report listing under half the last applied one's packages is skipped", {
  prior <- report_row(T2, "n1", n_packages = 100L)
  expect_equal(verdict(prior, T3, "new", T3, 49L), "skipped_floor")
  expect_equal(verdict(prior, T3, "new", T3, 50L), "applied")
})

propagation_lines <- function(pkgs, nodes = "source") {
  keys <- expand.grid(pkg = pkgs, node = nodes, stringsAsFactors = FALSE)
  parse_build_status_db(paste(sprintf("%s#%s#propagate: YES", keys$pkg, keys$node),
                              collapse = "\n"))$lines
}
under_floor <- function(...) propagation_floor(...)$under
open_propagation <- function(pkgs, nodes = "source") {
  apply_build_report(empty_build_history(), propagation_lines(pkgs, nodes),
                     list(bioc_version = "3.23", repo = "bioc", report_at = T1,
                          versions = c(), propagation_read = TRUE),
                     empty_build_reports(), exact = 0L)$history
}

test_that("a propagation file with no lines is under the floor", {
  none <- propagation_lines(character(0))
  expect_true(under_floor(open_propagation(c("a", "b")), none, "3.23", "bioc"))
  expect_true(under_floor(empty_build_history(), none, "3.23", "bioc"))
  # Status lines are not propagation lines.
  status <- parse_build_status_db("a#n1#install: OK")$lines
  expect_true(under_floor(empty_build_history(), status, "3.23", "bioc"))
})

test_that("a propagation file listing under half the packages with open rows is under the floor", {
  h <- open_propagation(c("a", "b", "c", "d"))
  expect_true(under_floor(h, propagation_lines("a"), "3.23", "bioc"))
  expect_false(under_floor(h, propagation_lines(c("a", "b")), "3.23", "bioc"))
  # The same line twice counts once.
  twice <- propagation_floor(h, propagation_lines(c("a", "a")), "3.23", "bioc")
  expect_equal(twice, list(packages = 1L, open = 4L, under = TRUE))
})

test_that("a platform dropped from the propagation file is not under the floor", {
  # Four lines where twelve rows are open, yet every package is still listed.
  pkgs <- c("a", "b", "c", "d")
  h <- open_propagation(pkgs, c("source", "win.binary", "mac.binary.big-sur-x86_64"))
  expect_equal(nrow(h), 12L)
  expect_equal(propagation_floor(h, propagation_lines(pkgs), "3.23", "bioc"),
               list(packages = 4L, open = 4L, under = FALSE))
})

test_that("a first propagation file, with no open propagation rows, is not under the floor", {
  expect_false(under_floor(empty_build_history(), propagation_lines("a"), "3.23", "bioc"))
  # Rows of another version or repo, and closed rows, are not a baseline.
  h <- open_propagation(c("a", "b", "c", "d"))
  expect_false(under_floor(h, propagation_lines("a"), "3.24", "bioc"))
  expect_false(under_floor(h, propagation_lines("a"), "3.23", "workflows"))
  h$ended_on <- T2; h$end_reason <- "gone"
  expect_false(under_floor(h, propagation_lines("a"), "3.23", "bioc"))
})

test_that("another BioC version, or a skipped report, is not a baseline", {
  prior <- report_row(T2, "n1", n_packages = 100L)
  expect_equal(verdict(prior, T1, "new", T1, 1L, version = "3.24"), "applied")
  skipped <- report_row(T2, "n1", n_packages = 100L, outcome = "skipped_floor")
  expect_equal(verdict(skipped, T2, "new", T2, 5L), "applied")
})
