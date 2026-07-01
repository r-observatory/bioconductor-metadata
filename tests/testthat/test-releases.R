test_that("parse_release_dates extracts release -> ISO date from config.yaml text", {
  yaml_text <- 'release_dates:\n  "3.20": "10/30/2024"\n  "3.21": "04/16/2025"\n'
  d <- parse_release_dates(yaml_text)
  expect_equal(unname(d["3.20"]), "2024-10-30")
  expect_equal(unname(d["3.21"]), "2025-04-16")
})
test_that("release_to_numeric orders minor versions numerically", {
  expect_true(release_to_numeric("3.10") > release_to_numeric("3.9"))
  expect_equal(release_to_numeric("3.20"), 3020)
})
