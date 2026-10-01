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

test_that("another BioC version, or a skipped report, is not a baseline", {
  prior <- report_row(T2, "n1", n_packages = 100L)
  expect_equal(verdict(prior, T1, "new", T1, 1L, version = "3.24"), "applied")
  skipped <- report_row(T2, "n1", n_packages = 100L, outcome = "skipped_floor")
  expect_equal(verdict(skipped, T2, "new", T2, 5L), "applied")
})
