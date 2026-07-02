# tests/testthat/test-biocviews-vocab.R
# Tests for parse_biocviews_dot() in scripts/helpers.R.

# ---------------------------------------------------------------------------
# parse_biocviews_dot
# ---------------------------------------------------------------------------

test_that("parse_biocviews_dot returns correct columns on empty input", {
  result <- parse_biocviews_dot("")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("parent", "child"))
})

test_that("parse_biocviews_dot returns 0-row frame on whitespace-only input", {
  result <- parse_biocviews_dot("   \n\t  ")

  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("parent", "child"))
})

test_that("parse_biocviews_dot parses 3 edges from a .dot blob with comments and braces", {
  dot <- '
digraph G {
  /* block comment line */
  BiocViews -> Software;
  Software -> AssayDomain; /* inline block comment */
  AssayDomain -> aCGH;
}
'
  result <- parse_biocviews_dot(dot)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("parent", "child"))
  expect_equal(result$parent, c("BiocViews", "Software", "AssayDomain"))
  expect_equal(result$child,  c("Software", "AssayDomain", "aCGH"))
})

test_that("parse_biocviews_dot ignores digraph header, braces, and standalone comments", {
  dot <- '
digraph G {
  // line comment
  /* another comment */
  Alpha -> Beta;
}
'
  result <- parse_biocviews_dot(dot)

  expect_equal(nrow(result), 1L)
  expect_equal(result$parent, "Alpha")
  expect_equal(result$child,  "Beta")
})

test_that("parse_biocviews_dot keeps typo edges as written without correction", {
  dot <- 'digraph G {\nSofware -> Technology;\n}'
  result <- parse_biocviews_dot(dot)

  expect_equal(nrow(result), 1L)
  expect_equal(result$parent, "Sofware")
  expect_equal(result$child,  "Technology")
})

test_that("parse_biocviews_dot handles names with dots and digits", {
  dot <- 'digraph G {\nPkg.1 -> Pkg.2;\n}'
  result <- parse_biocviews_dot(dot)

  expect_equal(nrow(result), 1L)
  expect_equal(result$parent, "Pkg.1")
  expect_equal(result$child,  "Pkg.2")
})
