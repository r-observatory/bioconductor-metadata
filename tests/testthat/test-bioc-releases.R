# tests/testthat/test-bioc-releases.R
# Tests for bioc_releases_from_dates() in scripts/helpers.R.

# ---------------------------------------------------------------------------
# bioc_releases_from_dates
# ---------------------------------------------------------------------------

test_that("bioc_releases_from_dates orders releases numerically by release_to_numeric", {
  dates  <- c("3.9" = "2019-05-03", "3.10" = "2019-10-30", "2.10" = "2012-04-02")
  result <- bioc_releases_from_dates(dates)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("version", "released", "seq"))
  expect_equal(result$version,  c("2.10", "3.9", "3.10"))
  expect_equal(result$seq,      c(1L, 2L, 3L))
  expect_equal(result$released, c("2012-04-02", "2019-05-03", "2019-10-30"))
})

test_that("bioc_releases_from_dates returns 0-row frame with correct columns on empty input", {
  result <- bioc_releases_from_dates(setNames(character(0), character(0)))

  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("version", "released", "seq"))
})

test_that("bioc_releases_from_dates handles a single release correctly", {
  dates  <- c("3.20" = "2024-10-30")
  result <- bioc_releases_from_dates(dates)

  expect_equal(nrow(result), 1L)
  expect_equal(result$version,  "3.20")
  expect_equal(result$released, "2024-10-30")
  expect_equal(result$seq,      1L)
})
