test_that("parse_authors_at_r extracts people, roles, and ORCID", {
  ar <- 'c(person("Dirk", "Eddelbuettel", role = c("aut","cre"),
             comment = c(ORCID = "0000-0001-6419-907X")),
           person("Romain", "Francois", role = "aut"))'
  out <- parse_authors_at_r(ar, "Rcpp")
  expect_equal(nrow(out), 2)
  expect_equal(out$family, c("Eddelbuettel", "Francois"))
  expect_equal(out$role[1], "aut, cre")
  expect_equal(out$orcid[1], "0000-0001-6419-907X")
  expect_equal(out$orcid[2], NA_character_)
  expect_true(all(out$package == "Rcpp"))
})

test_that("parse_authors_at_r returns empty on unparseable input", {
  out <- parse_authors_at_r("Hadley Wickham (free text, not Authors@R)", "x")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("package","given","family","email","role","orcid"))
})
