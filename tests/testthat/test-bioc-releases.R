# tests/testthat/test-bioc-releases.R
# Tests for bioc_releases_from_dates() and parse_r_ver_for_bioc() in scripts/helpers.R.

# ---------------------------------------------------------------------------
# bioc_releases_from_dates
# ---------------------------------------------------------------------------

test_that("bioc_releases_from_dates orders releases numerically by release_to_numeric", {
  dates  <- c("3.9" = "2019-05-03", "3.10" = "2019-10-30", "2.10" = "2012-04-02")
  result <- bioc_releases_from_dates(dates)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("version", "released", "seq", "r_version"))
  expect_equal(result$version,   c("2.10", "3.9", "3.10"))
  expect_equal(result$seq,       c(1L, 2L, 3L))
  expect_equal(result$released,  c("2012-04-02", "2019-05-03", "2019-10-30"))
  expect_equal(result$r_version, c(NA_character_, NA_character_, NA_character_))
})

test_that("bioc_releases_from_dates returns 0-row frame with correct columns on empty input", {
  result <- bioc_releases_from_dates(setNames(character(0), character(0)))

  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("version", "released", "seq", "r_version"))
})

test_that("bioc_releases_from_dates handles a single release correctly", {
  dates  <- c("3.20" = "2024-10-30")
  result <- bioc_releases_from_dates(dates)

  expect_equal(nrow(result), 1L)
  expect_equal(result$version,  "3.20")
  expect_equal(result$released, "2024-10-30")
  expect_equal(result$seq,      1L)
})

test_that("bioc_releases_from_dates populates r_version from named map and NAs for absent versions", {
  dates     <- c("3.23" = "2026-04-29", "3.10" = "2019-10-30")
  r_versions <- c("3.23" = "4.6", "3.10" = "3.6")
  result    <- bioc_releases_from_dates(dates, r_versions)

  expect_equal(names(result), c("version", "released", "seq", "r_version"))
  expect_equal(result$version,   c("3.10", "3.23"))
  expect_equal(result$seq,       c(1L, 2L))
  expect_equal(result$r_version, c("3.6", "4.6"))
})

test_that("bioc_releases_from_dates gives NA r_version for versions absent from the map", {
  dates      <- c("3.23" = "2026-04-29", "3.10" = "2019-10-30", "1.0" = "2002-05-01")
  r_versions <- c("3.23" = "4.6", "3.10" = "3.6")
  result     <- bioc_releases_from_dates(dates, r_versions)

  expect_equal(result$version,   c("1.0", "3.10", "3.23"))
  expect_equal(result$r_version, c(NA_character_, "3.6", "4.6"))
})

# ---------------------------------------------------------------------------
# parse_r_ver_for_bioc
# ---------------------------------------------------------------------------

test_that("parse_r_ver_for_bioc returns a named character vector from yaml", {
  yaml_text <- '
r_ver_for_bioc_ver:
  "3.10": "3.6"
  "3.23": "4.6"
'
  result <- parse_r_ver_for_bioc(yaml_text)

  expect_type(result, "character")
  expect_equal(length(result), 2L)
  expect_equal(result[["3.10"]], "3.6")
  expect_equal(result[["3.23"]], "4.6")
})

test_that("parse_r_ver_for_bioc returns empty named character vector when key absent", {
  yaml_text <- "release_dates:\n  \"3.23\": 04/29/2026\n"
  result <- parse_r_ver_for_bioc(yaml_text)

  expect_type(result, "character")
  expect_equal(length(result), 0L)
  expect_equal(names(result), character(0))
})
