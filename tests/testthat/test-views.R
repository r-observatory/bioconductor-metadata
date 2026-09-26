test_that("parse_views turns a VIEWS DCF blob into catalog rows", {
  views <- paste(
    "Package: BiocGenerics",
    "Version: 0.52.0",
    "Title: S4 generic functions",
    "Description: Generics used across Bioconductor.",
    "Maintainer: Bioconductor Package Maintainer <maintainer@bioconductor.org>",
    "Depends: R (>= 4.0)",
    "License: Artistic-2.0",
    "biocViews: Infrastructure",
    "git_url: https://git.bioconductor.org/packages/BiocGenerics",
    "",
    "Package: S4Vectors",
    "Version: 0.44.0",
    "Title: Foundation of vector-like classes",
    "Maintainer: H. Pages <hpages@bioconductor.org>",
    "License: Artistic-2.0",
    "biocViews: Infrastructure",
    "", sep = "\n")
  out <- parse_views(views, "software")
  expect_equal(nrow(out), 2)
  expect_equal(out$name, c("BiocGenerics", "S4Vectors"))
  expect_equal(out$name_lower, c("biocgenerics", "s4vectors"))
  expect_true(all(out$category == "software"))
  expect_equal(out$maintainer_email[1], "maintainer@bioconductor.org")
  expect_equal(out$maintainer[1], "Bioconductor Package Maintainer")
})
test_that("parse_views handles empty input", {
  out <- parse_views("", "software")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("name","name_lower","category","version","title",
    "description","maintainer","maintainer_email","license","depends",
    "imports","suggests","biocviews","git_url","has_news","views_has_readme"))
  expect_type(out$has_news, "integer")
  expect_type(out$views_has_readme, "integer")
})

read_views_fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")
}

test_that("parse_views reads hasNEWS and hasREADME as 1, 0 or NA", {
  out <- parse_views(read_views_fixture("views-software.dcf"), "software")
  flags <- setNames(out$has_news, out$name)
  readme <- setNames(out$views_has_readme, out$name)
  expect_identical(unname(flags[c("ADAM", "ABarray", "cummeRbund")]), c(1L, 0L, NA_integer_))
  expect_identical(unname(readme[c("AIMS", "ADAM", "cummeRbund")]), c(1L, 0L, NA_integer_))
})

test_that("views_flag maps only TRUE and FALSE", {
  expect_identical(views_flag(c("TRUE", "FALSE", " true ", NA, "", "yes")),
                   c(1L, 0L, 1L, NA_integer_, NA_integer_, NA_integer_))
})
