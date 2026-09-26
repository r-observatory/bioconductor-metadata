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

test_that("parse_views_vignettes pairs files and titles from a real VIEWS excerpt", {
  v <- parse_views_vignettes(read_views_fixture("views-software.dcf"), "software", "3.23")
  expect_equal(names(v), c("package", "release", "category", "version", "seq",
                           "file", "title", "output", "url"))
  expect_setequal(unique(v$package), c("ABarray", "ADAM", "AIMS", "BgeeDB", "BioQC", "NADfinder"))
  expect_false("cummeRbund" %in% v$package)

  bgee <- v[v$package == "BgeeDB", ]
  expect_equal(bgee$title, paste("BgeeDB, an R package for retrieval of curated",
                                 "expression datasets and for gene list enrichment tests"))

  bioqc <- v[v$package == "BioQC", ]
  expect_equal(bioqc$seq, 1:6)
  expect_equal(bioqc$file[6], "vignettes/BioQC/inst/doc/BioQC.html")
  expect_equal(bioqc$title[4], paste("BioQC-benchmark: Testing Efficiency, Sensitivity and",
                                     "Specificity of BioQC on simulated and real-world data"))
  expect_equal(bioqc$title[1], "BioQC Algorithm: Speeding up the Wilcoxon-Mann-Whitney Test")

  abarray <- v[v$package == "ABarray", ]
  expect_equal(abarray$title, c("ABarray gene expression", "ABarray gene expression GUI interface"))
  expect_equal(abarray$output, c("pdf", "pdf"))
  expect_equal(abarray$version, c("1.80.0", "1.80.0"))

  expect_equal(v$title[v$package == "ADAM"], "Using ADAM")
  expect_identical(v$title[v$package == "NADfinder"], NA_character_)
  expect_equal(v$url[v$package == "ADAM"],
               "https://bioconductor.org/packages/3.23/bioc/vignettes/ADAM/inst/doc/ADAM.html")
  expect_true(all(v$release == "3.23") && all(v$category == "software"))
  expect_type(v$seq, "integer")
})

test_that("parse_views_vignettes builds data/annotation links for an annotation package", {
  v <- parse_views_vignettes(read_views_fixture("views-annotation.dcf"), "annotation", "3.23")
  expect_equal(nrow(v), 1L)
  expect_equal(v$url, paste0("https://bioconductor.org/packages/3.23/data/annotation/",
                             "vignettes/AHEnsDbs/inst/doc/creating-EnsDbs.html"))
  expect_equal(v$title, "Provide EnsDb databases for AnnotationHub")
  expect_equal(v$output, "html")
})

test_that("parse_views_vignettes gives an empty typed frame for empty input", {
  v <- parse_views_vignettes("", "software", "3.23")
  expect_equal(nrow(v), 0L)
  expect_type(v$seq, "integer")
  expect_equal(nrow(parse_views_vignettes(NULL, "workflows", "3.23")), 0L)
})

test_that("split_vignette_titles keeps a doubled comma inside one title", {
  expect_equal(split_vignette_titles("A,, B, C"), c("A, B", "C"))
  expect_equal(split_vignette_titles(NA_character_), character(0))
})

test_that("a title count that differs from the file count leaves every title NA", {
  views <- paste(
    "Package: PkgTwo",
    "Version: 0.1.0",
    "vignettes: vignettes/PkgTwo/inst/doc/a.html,",
    "        vignettes/PkgTwo/inst/doc/b.html",
    "vignetteTitles: Only one title",
    "", sep = "\n")
  v <- parse_views_vignettes(views, "workflows", "3.23")
  expect_equal(nrow(v), 2L)
  expect_identical(v$title, c(NA_character_, NA_character_))
  expect_equal(v$url[2], "https://bioconductor.org/packages/3.23/workflows/vignettes/PkgTwo/inst/doc/b.html")
})

test_that("build_bioc_vignettes keeps the first category of a package listed twice", {
  annot <- read_views_fixture("views-annotation.dcf")
  expect_message(
    v <- build_bioc_vignettes(list(software = annot, annotation = annot), "3.23"),
    "AHEnsDbs")
  expect_equal(nrow(v), 1L)
  expect_equal(v$category, "software")
  expect_match(v$url, "/3.23/bioc/vignettes/AHEnsDbs/", fixed = TRUE)
})
