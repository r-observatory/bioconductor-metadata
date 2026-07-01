library(RSQLite)
library(jsonlite)

# ---------------------------------------------------------------------------
# Shared fixture builders
# ---------------------------------------------------------------------------

make_packages_df <- function() {
  data.frame(
    name              = c("PkgAlpha", "PkgBeta"),
    name_lower        = c("pkgalpha", "pkgbeta"),
    category          = c("software", "annotation"),
    version           = c("1.0.0", "2.1.3"),
    title             = c("Alpha package", "Beta package"),
    description       = c("Does alpha things.", "Does beta things."),
    maintainer        = c("Alice Smith", "Bob Jones"),
    maintainer_email  = c("alice@example.com", "bob@example.com"),
    license           = c("MIT", "GPL-3"),
    depends           = c(NA_character_, "R (>= 4.0)"),
    imports           = c("methods", NA_character_),
    suggests          = c(NA_character_, NA_character_),
    biocviews         = c("Infrastructure", "Annotation"),
    git_url           = c("https://git.bioconductor.org/packages/PkgAlpha",
                          "https://git.bioconductor.org/packages/PkgBeta"),
    first_release     = c("3.10", "3.18"),
    first_release_date = c("2018-10-31", "2022-04-27"),
    last_release      = c("3.21", "3.21"),
    last_release_date  = c("2024-10-30", "2024-10-30"),
    in_current        = c(1L, 1L),
    in_devel          = c(1L, 0L),
    updated_at        = c("2025-01-01", "2025-01-01"),
    stringsAsFactors  = FALSE
  )
}

make_authors_df <- function() {
  data.frame(
    package = c("PkgAlpha", "PkgAlpha", "PkgBeta"),
    given   = c("Alice", "Carol", "Bob"),
    family  = c("Smith", "White", "Jones"),
    email   = c("alice@example.com", NA_character_, "bob@example.com"),
    role    = c("aut, cre", "ctb", "aut, cre"),
    orcid   = c("0000-0001-2345-6789", NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# export_catalog
# ---------------------------------------------------------------------------

test_that("export_catalog writes bioc_packages with correct row count and values", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  pkgs  <- make_packages_df()
  auths <- make_authors_df()
  export_catalog(tmp, pkgs, auths)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_packages")
  expect_equal(nrow(rows), 2L)
  alpha <- rows[rows$name == "PkgAlpha", ]
  expect_equal(alpha$first_release, "3.10")
  expect_equal(alpha$first_release_date, "2018-10-31")
  expect_equal(alpha$in_current, 1L)
  expect_equal(alpha$in_devel, 1L)
})

test_that("export_catalog writes bioc_authors with correct row count and orcid", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors")
  expect_equal(nrow(rows), 3L)
  alpha_aut <- rows[rows$package == "PkgAlpha" & rows$given == "Alice", ]
  expect_equal(alpha_aut$orcid, "0000-0001-2345-6789")
})

test_that("export_catalog creates all three required indexes", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  idx <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' ORDER BY name"
  )$name
  expect_true("idx_bioc_meta_lower"    %in% idx)
  expect_true("idx_bioc_authors_package" %in% idx)
  expect_true("idx_bioc_authors_name"  %in% idx)
})

test_that("export_catalog overwrites an existing DB file cleanly", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  # First write
  export_catalog(tmp, make_packages_df(), make_authors_df())
  # Second write with a single-row frame -- must not double-insert
  single <- make_packages_df()[1L, ]
  export_catalog(tmp, single, make_authors_df()[1L, ])

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM bioc_packages")$n, 1L)
})

# ---------------------------------------------------------------------------
# write_manifest
# ---------------------------------------------------------------------------

test_that("write_manifest writes valid JSON readable by jsonlite::read_json", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  obj <- list(pipeline = "bioconductor-metadata", version = "1.0", packages = 42L)
  write_manifest(tmp, obj)

  result <- jsonlite::read_json(tmp)
  expect_equal(result$pipeline, "bioconductor-metadata")
  expect_equal(result$version, "1.0")
  expect_equal(result$packages, 42L)
})
