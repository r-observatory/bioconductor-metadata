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
