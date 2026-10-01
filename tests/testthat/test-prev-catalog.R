library(RSQLite)

# The published db is the only copy of state the pipeline cannot rebuild from
# upstream, so every failure short of "no release yet" stops the run.

# Source update.R if not already loaded.
if (!exists("read_prev_catalog", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

# A fake gh on PATH. `api` prints the given status line; `release download`
# copies the named asset from src_dir, or fails when src_dir lacks it.
local_fake_release <- function(status_line, src_dir, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  gh  <- file.path(dir, "gh")
  writeLines(c(
    "#!/bin/sh",
    "if [ \"$1\" = api ]; then",
    sprintf("  echo '%s'", status_line),
    if (grepl(" 200 ", status_line, fixed = TRUE)) "  exit 0" else "  exit 1",
    "fi",
    "if [ \"$1 $2\" = 'release download' ]; then",
    "  pat=''; out=''",
    "  while [ $# -gt 0 ]; do",
    "    case \"$1\" in --pattern) pat=\"$2\"; shift ;; --dir) out=\"$2\"; shift ;; esac",
    "    shift",
    "  done",
    sprintf("  if [ -f '%s'/\"$pat\" ]; then cp '%s'/\"$pat\" \"$out\"/\"$pat\"; exit 0; fi",
            src_dir, src_dir),
    "  exit 1",
    "fi",
    "exit 1"), gh)
  Sys.chmod(gh, "0755")
  withr::local_path(dir, action = "prefix", .local_envir = env)
  invisible(gh)
}

# A catalog as schema 2 publishes it, with its manifest. extra_sql runs on the
# db afterwards; tables is the manifest's table list and n_packages its
# n_packages, left out when NULL.
write_prior_catalog <- function(dir, tables = list(bioc_packages = 1L), extra_sql = character(0),
                                n_packages = NULL) {
  pkgs <- data.frame(
    name = "PkgSoft", name_lower = "pkgsoft", category = "software",
    version = "1.2.0", title = "t", description = "d", maintainer = "m",
    maintainer_email = "e", license = "MIT", depends = NA_character_,
    imports = NA_character_, suggests = NA_character_, biocviews = "Software",
    git_url = NA_character_, first_release = "3.20", first_release_date = "2024-10-30",
    last_release = "3.23", last_release_date = "2026-04-29", in_current = 1L,
    in_devel = 1L, updated_at = "2026-09-29T06:00:00Z", stringsAsFactors = FALSE)
  db <- file.path(dir, "bioconductor-metadata.db")
  export_catalog(db, pkgs, empty_bioc_authors(), names_all_df = build_bioc_names_all(pkgs))
  if (length(extra_sql) > 0L) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
    for (sql in extra_sql) RSQLite::dbExecute(con, sql)
    RSQLite::dbDisconnect(con)
  }
  manifest <- list(source = list(schema = 2L), tables = tables)
  if (!is.null(n_packages)) manifest$n_packages <- n_packages
  jsonlite::write_json(manifest, file.path(dir, "manifest.json"), auto_unbox = TRUE)
}

no_sleep <- function(s) invisible(NULL)

test_that("no current release is a bootstrap with no prior packages", {
  src <- withr::local_tempdir()
  local_fake_release("HTTP/2.0 404 Not Found", src)
  prev <- read_prev_catalog(sleep = no_sleep)
  expect_null(prev$packages)
  expect_null(prev$build_reports)
  expect_equal(nrow(prev$names_all), 0L)
})

test_that("a release whose db cannot be downloaded stops the run after the retries", {
  src <- withr::local_tempdir()
  write_prior_catalog(src)
  unlink(file.path(src, "bioconductor-metadata.db"))
  local_fake_release("HTTP/2.0 200 OK", src)
  slept <- numeric(0)
  expect_error(read_prev_catalog(sleep = function(s) slept <<- c(slept, s)),
               "Prior bioconductor-metadata.db download failed", fixed = TRUE)
  expect_length(slept, length(RETRY_WAITS_S))
})

test_that("a release whose manifest cannot be downloaded stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src)
  unlink(file.path(src, "manifest.json"))
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior manifest.json download failed", fixed = TRUE)
})

test_that("a release that cannot be looked up stops the run after the retries", {
  src <- withr::local_tempdir()
  local_fake_release("HTTP/2.0 502 Bad Gateway", src)
  slept <- numeric(0)
  expect_error(read_prev_catalog(sleep = function(s) slept <<- c(slept, s)),
               "Could not tell whether the current release exists (status 502)",
               fixed = TRUE)
  expect_length(slept, length(RETRY_WAITS_S))
})

test_that("a downloaded file that is not a database stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src)
  writeLines("<html>Service Unavailable</html>", file.path(src, "bioconductor-metadata.db"))
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep), "Prior catalog cannot be read")
})

test_that("a schema 2 db keeps the catalog and has no episode tables", {
  src <- withr::local_tempdir()
  write_prior_catalog(src)
  local_fake_release("HTTP/2.0 200 OK", src)
  prev <- read_prev_catalog(sleep = no_sleep)
  expect_equal(prev$packages$name, "PkgSoft")
  expect_equal(prev$manifest$source$schema, 2L)
  expect_equal(prev$names_all$name_lower, "pkgsoft")
  expect_equal(nrow(prev$view_edges), 0L)
  expect_null(prev$build_reports)
  expect_null(prev$build_status)
  expect_null(prev$views_history)
})

test_that("a db missing a state table its manifest lists stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, tables = list(bioc_packages = 1L, bioc_build_status_history = 3L))
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior catalog lacks bioc_build_status_history, which its manifest lists with 3 rows",
               fixed = TRUE)
})

test_that("a state table with fewer rows than its manifest lists stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, tables = list(bioc_views_history = 3L),
                      extra_sql = c("CREATE TABLE bioc_views_history (package TEXT)",
                                    "INSERT INTO bioc_views_history VALUES ('a'), ('b')"))
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior catalog holds 2 rows of bioc_views_history; its manifest lists 3",
               fixed = TRUE)
})

test_that("a state table holding at least its listed rows is read", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, tables = list(bioc_views_history = 2L),
                      extra_sql = c("CREATE TABLE bioc_views_history (package TEXT)",
                                    "INSERT INTO bioc_views_history VALUES ('a'), ('b')"))
  local_fake_release("HTTP/2.0 200 OK", src)
  prev <- read_prev_catalog(sleep = no_sleep)
  expect_equal(prev$views_history$package, c("a", "b"))
})

test_that("a db with no package rows whose manifest counts packages stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, n_packages = 1L, extra_sql = "DELETE FROM bioc_packages")
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior catalog holds 0 rows of bioc_packages; its manifest gives n_packages 1",
               fixed = TRUE)
})

test_that("a db with fewer package rows than its manifest's n_packages stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, n_packages = 4693L)
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior catalog holds 1 rows of bioc_packages; its manifest gives n_packages 4693",
               fixed = TRUE)
})

test_that("a db with no package rows and a manifest without n_packages stops the run", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, extra_sql = "DELETE FROM bioc_packages")
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_error(read_prev_catalog(sleep = no_sleep),
               "Prior catalog holds no bioc_packages rows and its manifest gives no n_packages",
               fixed = TRUE)
})

test_that("a db holding the package rows its manifest counts is read", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, n_packages = 1L)
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_equal(read_prev_catalog(sleep = no_sleep)$packages$name, "PkgSoft")
})

test_that("an empty catalog whose manifest says n_packages 0 is read as no prior packages", {
  src <- withr::local_tempdir()
  write_prior_catalog(src, n_packages = 0L, extra_sql = "DELETE FROM bioc_packages")
  local_fake_release("HTTP/2.0 200 OK", src)
  expect_equal(nrow(read_prev_catalog(sleep = no_sleep)$packages), 0L)
})

test_that("run_update stops on an unreadable prior and writes nothing", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  io <- list(
    config_yaml = function() "release_dates:\n  '3.23': 04/29/2026\n",
    fetch_views = function(cat) "",
    prev_catalog = function() stop("Prior bioconductor-metadata.db download failed (gh release download current)"))
  expect_error(run_update(io, out, force_full = FALSE), "download failed")
  expect_false(file.exists(file.path(out, "bioconductor-metadata.db")))
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("--bootstrap starts from scratch past an unreadable prior and says so", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  io <- list(
    config_yaml = function() "release_dates:\n  '3.23': 04/29/2026\n",
    fetch_views = function(cat) "",
    list_repos = function() character(0),
    fetch_biocviews_dot = function(branch) NULL,
    prev_catalog = function() stop("Prior catalog cannot be read: file is not a database"))
  expect_message(res <- run_update(io, out, force_full = TRUE),
                 "--bootstrap starts from scratch", fixed = TRUE)
  expect_true(res$manifest$cold_start)
  expect_true(file.exists(file.path(out, "bioconductor-metadata.db")))
})

test_that("the default io reads the prior catalog through read_prev_catalog", {
  expect_match(paste(deparse(default_io()$prev_catalog), collapse = " "),
               "read_prev_catalog", fixed = TRUE)
})
