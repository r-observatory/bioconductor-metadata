test_that("package_lineage derives first/last release, in_current, in_devel", {
  dates <- c("2.10" = "2012-04-02", "3.10" = "2019-10-30", "3.23" = "2026-04-15")
  br <- c("HEAD","RELEASE_2_10","RELEASE_3_9","RELEASE_3_10","devel")
  L <- package_lineage(br, current_release = "3.23", dates = dates)
  expect_equal(L$first_release, "2.10")
  expect_equal(L$first_release_date, "2012-04-02")
  expect_equal(L$last_release, "3.10")           # last RELEASE branch, not devel
  expect_equal(L$last_release_date, "2019-10-30")
  expect_false(L$in_current)                      # no RELEASE_3_23 branch
  expect_true(L$in_devel)
})

test_that("package_lineage marks in_current when the current release branch exists", {
  br <- c("RELEASE_3_22","RELEASE_3_23","devel")
  L <- package_lineage(br, "3.23", c("3.22"="2025-10-30","3.23"="2026-04-15"))
  expect_true(L$in_current)
  expect_equal(L$last_release, "3.23")
})
