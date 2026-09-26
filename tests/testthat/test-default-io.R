library(RSQLite)

# Source update.R if not already loaded.
if (!exists("default_io", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

# ---------------------------------------------------------------------------
# Structure tests (no network calls)
# ---------------------------------------------------------------------------

test_that("default_io returns a list with all required io interface methods", {
  io       <- default_io()
  required <- c("config_yaml", "fetch_views", "list_repos",
                "ls_remote", "fetch_description", "prev_catalog")
  expect_type(io, "list")
  for (m in required) {
    expect_true(is.function(io[[m]]),
                info = sprintf("default_io()$%s must be a function", m))
  }
})

test_that("default_io fetch_views accepts a 'cat' argument", {
  io <- default_io()
  expect_true("cat" %in% names(formals(io$fetch_views)))
})

test_that("default_io ls_remote accepts a 'pkg' argument", {
  io <- default_io()
  expect_true("pkg" %in% names(formals(io$ls_remote)))
})

test_that("default_io fetch_description accepts 'pkg' and 'branch' arguments", {
  io <- default_io()
  args <- names(formals(io$fetch_description))
  expect_true("pkg"    %in% args)
  expect_true("branch" %in% args)
})

# ---------------------------------------------------------------------------
# list_repos against a fake gh on PATH
# ---------------------------------------------------------------------------

local_fake_gh <- function(lines, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  gh  <- file.path(dir, "gh")
  writeLines(c("#!/bin/sh", lines), gh)
  Sys.chmod(gh, "0755")
  withr::local_path(dir, action = "prefix", .local_envir = env)
  invisible(gh)
}

test_that("default_io list_repos stops when gh fails after printing some names", {
  # gh prints a failed page's error body to stdout even under --jq
  local_fake_gh(c(
    "echo PkgB",
    "echo PkgA",
    "echo '{\"message\":\"Server Error\",\"status\":\"500\"}'",
    "exit 1"))
  expect_error(default_io()$list_repos(),
               "Repository listing failed (gh exit 1); not crawling a partial listing",
               fixed = TRUE)
})

test_that("default_io list_repos returns the sorted names when gh succeeds", {
  local_fake_gh(c("echo PkgB", "echo PkgA", "echo", "exit 0"))
  expect_equal(default_io()$list_repos(), c("PkgA", "PkgB"))
})

# ---------------------------------------------------------------------------
# Config constant sanity checks (offline)
# ---------------------------------------------------------------------------

test_that("VIEWS_URLS covers the four Bioconductor package categories", {
  expect_setequal(names(VIEWS_URLS),
                  c("software", "annotation", "experiment", "workflows"))
  expect_true(all(grepl("^https://bioconductor.org/", VIEWS_URLS)))
})

test_that("CONFIG_YAML_URL points to bioconductor.org", {
  expect_match(CONFIG_YAML_URL, "^https://bioconductor\\.org/")
})

test_that("BIOC_RAW_BASE and BIOC_GIT_BASE are set to github.com/bioc", {
  expect_match(BIOC_RAW_BASE, "raw\\.githubusercontent\\.com/bioc")
  expect_match(BIOC_GIT_BASE, "github\\.com/bioc")
})

test_that("PUBLISH_REPO is the r-observatory metadata repo", {
  expect_equal(PUBLISH_REPO, "r-observatory/bioconductor-metadata")
})
