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

test_that("export_catalog stores has_news and views_has_readme as INTEGER after updated_at", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  pkgs <- make_packages_df()
  pkgs$has_news <- c(1L, NA_integer_)
  pkgs$views_has_readme <- c(0L, 1L)
  export_catalog(tmp, pkgs, make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_packages)")
  at <- match("updated_at", info$name)
  expect_equal(info$name[at + 0:2], c("updated_at", "has_news", "views_has_readme"))
  expect_equal(info$type[info$name %in% c("has_news", "views_has_readme")],
               c("INTEGER", "INTEGER"))
  rows <- RSQLite::dbGetQuery(con, "SELECT name, has_news, views_has_readme FROM bioc_packages ORDER BY name")
  expect_identical(rows$has_news, c(1L, NA_integer_))
  expect_identical(rows$views_has_readme, c(0L, 1L))
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

test_that("export_catalog writes ror_id and comment after orcid in bioc_authors", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  auths <- make_authors_df()
  auths$ror_id  <- c(NA_character_, "02nr0ka47", NA_character_)
  auths$comment <- c("University X", NA_character_, NA_character_)
  export_catalog(tmp, make_packages_df(), auths)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_authors)")
  expect_equal(info$name, BIOC_AUTHOR_COLS)
  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors ORDER BY given")
  expect_equal(rows$comment[rows$given == "Alice"], "University X")
  expect_equal(rows$ror_id[rows$given == "Carol"], "02nr0ka47")
})

test_that("export_catalog creates all required indexes including bioc_releases", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  idx <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' ORDER BY name"
  )$name
  expect_true("idx_bioc_meta_lower"      %in% idx)
  expect_true("idx_bioc_authors_package" %in% idx)
  expect_true("idx_bioc_authors_name"    %in% idx)
  expect_true("idx_bioc_releases_seq"    %in% idx)
})

test_that("export_catalog with releases_df writes bioc_releases rows in seq order", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  releases <- data.frame(
    version   = c("3.9", "3.10"),
    released  = c("2019-05-03", "2019-10-30"),
    seq       = c(1L, 2L),
    r_version = c(NA_character_, "3.6"),
    stringsAsFactors = FALSE
  )
  export_catalog(tmp, make_packages_df(), make_authors_df(), releases)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_releases ORDER BY seq")
  expect_equal(nrow(rows), 2L)
  expect_equal(rows$version,   c("3.9", "3.10"))
  expect_equal(rows$released,  c("2019-05-03", "2019-10-30"))
  expect_equal(rows$seq,       c(1L, 2L))
  expect_equal(rows$r_version, c(NA_character_, "3.6"))
})

test_that("export_catalog without releases_df creates empty bioc_releases table", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_releases")
  expect_equal(nrow(rows), 0L)

  # Table must still carry the right column names
  tbl_info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_releases)")
  expect_true("version"   %in% tbl_info$name)
  expect_true("released"  %in% tbl_info$name)
  expect_true("seq"       %in% tbl_info$name)
  expect_true("r_version" %in% tbl_info$name)
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

# ---------------------------------------------------------------------------
# export_catalog -- bioc_view_edges
# ---------------------------------------------------------------------------

test_that("export_catalog with view_edges_df writes bioc_view_edges rows", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  ve <- data.frame(
    release = c("3.20", "3.20", "3.20"),
    parent  = c("BiocViews", "Software", "AssayDomain"),
    child   = c("Software",  "AssayDomain", "aCGH"),
    stringsAsFactors = FALSE
  )
  export_catalog(tmp, make_packages_df(), make_authors_df(),
                 view_edges_df = ve)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_view_edges ORDER BY rowid")
  expect_equal(nrow(rows), 3L)
  expect_equal(rows$release, c("3.20", "3.20", "3.20"))
  expect_equal(rows$parent,  c("BiocViews", "Software", "AssayDomain"))
  expect_equal(rows$child,   c("Software",  "AssayDomain", "aCGH"))
})

test_that("export_catalog with view_edges_df creates the two view_edges indexes", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  ve <- data.frame(
    release = "3.20", parent = "BiocViews", child = "Software",
    stringsAsFactors = FALSE
  )
  export_catalog(tmp, make_packages_df(), make_authors_df(),
                 view_edges_df = ve)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  idx <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' ORDER BY name"
  )$name
  expect_true("idx_bioc_view_edges_rel"   %in% idx)
  expect_true("idx_bioc_view_edges_child" %in% idx)
})

test_that("export_catalog without view_edges_df creates empty bioc_view_edges table", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_view_edges")
  expect_equal(nrow(rows), 0L)

  tbl_info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_view_edges)")
  expect_true("release" %in% tbl_info$name)
  expect_true("parent"  %in% tbl_info$name)
  expect_true("child"   %in% tbl_info$name)

  idx <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' ORDER BY name"
  )$name
  expect_true("idx_bioc_view_edges_rel"   %in% idx)
  expect_true("idx_bioc_view_edges_child" %in% idx)
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

# ---------------------------------------------------------------------------
# export_catalog -- bioc_vignettes
# ---------------------------------------------------------------------------

make_vignettes_df <- function() {
  data.frame(
    package  = c("PkgAlpha", "PkgAlpha"),
    release  = c("3.23", "3.23"),
    category = c("software", "software"),
    version  = c("1.0.0", "1.0.0"),
    seq      = c(1L, 2L),
    file     = c("vignettes/PkgAlpha/inst/doc/intro.html",
                 "vignettes/PkgAlpha/inst/doc/more.pdf"),
    title    = c("Intro", NA_character_),
    output   = c("html", "pdf"),
    url      = c("https://bioconductor.org/packages/3.23/bioc/vignettes/PkgAlpha/inst/doc/intro.html",
                 "https://bioconductor.org/packages/3.23/bioc/vignettes/PkgAlpha/inst/doc/more.pdf"),
    stringsAsFactors = FALSE
  )
}

test_that("export_catalog writes bioc_vignettes rows keyed by package and seq", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df(),
                 vignettes_df = make_vignettes_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_vignettes ORDER BY seq")
  expect_equal(nrow(rows), 2L)
  expect_equal(rows$title, c("Intro", NA_character_))
  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_vignettes)")
  expect_equal(info$name, c("package", "release", "category", "version", "seq",
                            "file", "title", "output", "url"))
  expect_equal(info$pk[info$name %in% c("package", "seq")], c(1L, 2L))
  dup <- make_vignettes_df()[1, ]
  expect_error(RSQLite::dbWriteTable(con, "bioc_vignettes", dup, append = TRUE),
               "UNIQUE")
})

test_that("export_catalog without vignettes_df creates an empty bioc_vignettes table", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_catalog(tmp, make_packages_df(), make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_true(RSQLite::dbExistsTable(con, "bioc_vignettes"))
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM bioc_vignettes")$n, 0L)
})

test_that("export_catalog writes the build tables with their open-row indexes and checks", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  reports <- data.frame(
    bioc_version = "3.23", repo = "bioc", report_at = "2026-09-28T17:40:00Z",
    branch = "release", snapshot_at = "2026-09-28T17:40:00Z", generated_at = NA_character_,
    published_at = "2026-09-29T16:35:46Z", status_sha256 = "abc", n_packages = 2417L,
    n_lines = 14499L, n_na = 520L, nodes = "nebbiolo1,kunpeng2",
    read_at = "2026-09-30T12:20:00Z", outcome = "applied", stringsAsFactors = FALSE)
  status <- data.frame(
    package = "a4", bioc_version = "3.23", repo = "bioc", node = "nebbiolo1",
    stage = "checksrc", episode_seq = 1L, status = "OK", detail = NA_character_,
    first_version = "1.60.0", last_version = "1.60.0",
    first_seen = "2026-09-28T17:40:00Z", last_seen = "2026-09-28T17:40:00Z",
    first_seen_exact = 0L, ended_on = NA_character_, end_reason = NA_character_,
    stringsAsFactors = FALSE)
  export_catalog(tmp, make_packages_df(), make_authors_df(),
                 build_reports_df = reports, build_status_df = status)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT n_na FROM bioc_build_reports")$n_na, 520L)
  idx <- RSQLite::dbGetQuery(con, paste(
    "SELECT name, sql FROM sqlite_master",
    "WHERE type = 'index' AND tbl_name = 'bioc_build_status_history' AND sql IS NOT NULL"))
  expect_setequal(idx$name, c("ux_bioc_build_open", "idx_bioc_build_open_status"))
  expect_match(idx$sql[idx$name == "ux_bioc_build_open"], "WHERE ended_on IS NULL", fixed = TRUE)
  # A second open row for the same package, node and stage is refused.
  expect_error(RSQLite::dbExecute(con, paste(
    "INSERT INTO bioc_build_status_history VALUES ('a4', '3.23', 'bioc', 'nebbiolo1',",
    "'checksrc', 2, 'ERROR', NULL, NULL, NULL, '2026-09-29T17:40:00Z',",
    "'2026-09-29T17:40:00Z', 1, NULL, NULL)")), "UNIQUE")
  # An end without its reason is refused.
  expect_error(RSQLite::dbExecute(con, paste(
    "UPDATE bioc_build_status_history SET ended_on = '2026-09-29T17:40:00Z'")), "CHECK")
})

test_that("export_catalog without build frames creates no build tables", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  export_catalog(tmp, make_packages_df(), make_authors_df())
  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_false(RSQLite::dbExistsTable(con, "bioc_build_reports"))
  expect_false(RSQLite::dbExistsTable(con, "bioc_build_status_history"))
})

test_that("export_catalog writes the VIEWS columns after views_has_readme", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  pkgs <- make_packages_df()
  pkgs$package_status <- c("Deprecated", NA_character_)
  pkgs$dependency_count <- c(12L, NA_integer_)
  export_catalog(tmp, pkgs, make_authors_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_packages)")
  expect_equal(tail(info$name, 7), c("views_has_readme", VIEWS_EXTRA_COLS))
  expect_equal(info$type[info$name == "dependency_count"], "INTEGER")
  rows <- RSQLite::dbGetQuery(con, paste("SELECT name, package_status, dependency_count,",
                                         "linking_to FROM bioc_packages ORDER BY name"))
  expect_equal(rows$package_status, c("Deprecated", NA))
  expect_identical(rows$dependency_count, c(12L, NA_integer_))
  expect_identical(rows$linking_to, c(NA_character_, NA_character_))
})

test_that("export_catalog writes bioc_views_history with its open-row index", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  views <- data.frame(
    package = "cummeRbund", field = "PackageStatus", episode_seq = 1L, value = "Deprecated",
    bioc_version = "3.23", category = "software", first_seen = "2026-09-29T18:14:30Z",
    last_seen = "2026-09-29T18:14:30Z", first_seen_exact = 0L, ended_on = NA_character_,
    stringsAsFactors = FALSE)
  export_catalog(tmp, make_packages_df(), make_authors_df(), views_history_df = views)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT value FROM bioc_views_history")$value,
               "Deprecated")
  sql <- RSQLite::dbGetQuery(con,
    "SELECT sql FROM sqlite_master WHERE name = 'ux_bioc_views_open'")$sql
  expect_match(sql, "WHERE ended_on IS NULL", fixed = TRUE)
  expect_error(RSQLite::dbExecute(con, paste(
    "INSERT INTO bioc_views_history VALUES ('cummeRbund', 'PackageStatus', 2, 'Active',",
    "'3.23', 'software', '2026-09-30T18:14:30Z', '2026-09-30T18:14:30Z', 1, NULL)")),
    "UNIQUE")
})
