# Parsers for the build report files, on lines and pages cut from the real
# 3.23 report of 2026-09-29.

# Source update.R if not already loaded.
if (!exists("parse_build_status_db", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

read_index_fixture <- function() {
  paste(readLines(test_path("fixtures", "report-index-3.23.html"), warn = FALSE),
        collapse = "\n")
}

test_that("parse_build_status_db reads real lines, dotted names, NA and blank lines", {
  txt <- paste(
    "alabaster.base#nebbiolo1#install: OK",
    "alabaster.base#kunpeng2#checksrc: OK",
    "",
    "RiboDiPA#kunpeng2#checksrc: NA",
    "affy#nebbiolo1#checksrc: WARNINGS",
    "affy#nebbiolo1#buildbin: NewStatus",
    "", sep = "\n")
  p <- parse_build_status_db(txt)
  expect_true(p$valid)
  expect_equal(p$lines$package, c("alabaster.base", "alabaster.base", "RiboDiPA", "affy", "affy"))
  expect_equal(p$lines$node, c("nebbiolo1", "kunpeng2", "kunpeng2", "nebbiolo1", "nebbiolo1"))
  expect_equal(p$lines$stage, c("install", "checksrc", "checksrc", "checksrc", "buildbin"))
  expect_equal(p$lines$status, c("OK", "OK", "NA", "WARNINGS", "NewStatus"))
  expect_true(all(is.na(p$lines$detail)))
})

test_that("parse_build_status_db keeps only the reason given with a propagation NO", {
  txt <- paste(
    "a4#source#propagate: UNNEEDED, same version is already published",
    "DirichletMultinomial#source#propagate: YES",
    "linkSet#source#propagate: NO, package depends on 'Organism.dplyr' which is not available",
    sep = "\n")
  p <- parse_build_status_db(txt)
  expect_true(p$valid)
  expect_equal(p$lines$status, c("UNNEEDED", "YES", "NO"))
  expect_equal(p$lines$stage, rep("propagate", 3))
  expect_identical(p$lines$detail,
                   c(NA, NA, "package depends on 'Organism.dplyr' which is not available"))
})

test_that("one malformed line makes the whole file invalid", {
  expect_false(parse_build_status_db("a4#nebbiolo1#install: OK\n<html>Bad Gateway</html>")$valid)
  expect_false(parse_build_status_db("a4#nebbiolo1#install OK")$valid)
  expect_false(parse_build_status_db(NULL)$valid)
  expect_false(parse_build_status_db(NA_character_)$valid)
})

test_that("an empty file is valid and has no lines", {
  p <- parse_build_status_db("\n\n")
  expect_true(p$valid)
  expect_equal(nrow(p$lines), 0L)
})

test_that("the real 2026-09-29 release file parses whole", {
  txt <- paste(readLines(gzfile(test_path("fixtures", "build-status-3.23-2026-09-29.txt.gz"))),
               collapse = "\n")
  p <- parse_build_status_db(txt)
  expect_true(p$valid)
  expect_equal(nrow(p$lines), 14499L)
  expect_equal(length(unique(p$lines$package)), 2417L)
  expect_equal(unique(p$lines$node), c("nebbiolo1", "kunpeng2"))
  expect_equal(sum(p$lines$status == "NA"), 520L)
})

test_that("parse_report_index reads the version, both times in UTC and built versions", {
  idx <- parse_report_index(read_index_fixture())
  expect_equal(idx$bioc_version, "3.23")
  expect_equal(idx$generated_at, "2026-09-29T15:33:00Z")
  expect_equal(idx$snapshot_at, "2026-09-28T17:40:00Z")
  expect_equal(unname(idx$versions[c("a4", "affy", "alabaster.base")]),
               c("1.60.0", "1.90.0", "1.12.1"))
  # Listed in the index though the status file never names it.
  expect_equal(unname(idx$versions["RbowtieCuda"]), "1.4.3")
})

test_that("parse_report_index reads the version from the data and workflows titles", {
  expect_equal(parse_report_index(
    "<TITLE>Build/check report for BioC 3.24 experimental data</TITLE>")$bioc_version, "3.24")
  expect_equal(parse_report_index(
    "<TITLE>Workflows build report for BioC 3.23</TITLE>")$bioc_version, "3.23")
})

test_that("parse_report_index degrades to NA when the markup changes", {
  html <- read_index_fixture()
  drifted <- gsub("generated on", "built at", html, fixed = TRUE)
  drifted <- gsub("Snapshot", "Checkout", drifted, fixed = TRUE)
  idx <- parse_report_index(drifted)
  expect_equal(idx$bioc_version, "3.23")
  expect_identical(idx$generated_at, NA_character_)
  expect_identical(idx$snapshot_at, NA_character_)
  none <- parse_report_index("<html><body>maintenance</body></html>")
  expect_identical(none$bioc_version, NA_character_)
  expect_length(none$versions, 0L)
  expect_identical(parse_report_index(NULL)$bioc_version, NA_character_)
})

test_that("local_time_to_utc applies the offset's sign", {
  expect_equal(local_time_to_utc("2026-09-28 13:40", "-0400"), "2026-09-28T17:40:00Z")
  expect_equal(local_time_to_utc("2026-11-02 13:40", "-0500"), "2026-11-02T18:40:00Z")
  expect_equal(local_time_to_utc("2026-09-28 01:10", "+0200"), "2026-09-27T23:10:00Z")
})

test_that("parse_branch_versions reads the quoted release and devel versions", {
  y <- "release_version: \"3.23\"\ndevel_version: \"3.24\"\n"
  expect_equal(parse_branch_versions(y), c(release = "3.23", devel = "3.24"))
  expect_equal(parse_branch_versions("release_dates: {}\n"),
               c(release = NA_character_, devel = NA_character_))
})

test_that("build_file_url names the LATEST alias of each branch and repo", {
  expect_equal(build_file_url("devel", "data-experiment", "index.html"),
               "https://bioconductor.org/checkResults/devel/data-experiment-LATEST/index.html")
})
