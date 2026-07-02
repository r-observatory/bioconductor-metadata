library(RSQLite)
library(jsonlite)

# Source update.R if not already loaded. When test_dir() runs it changes cwd to
# tests/testthat; when called from the project root directly, cwd stays there.
if (!exists("run_update", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

# ---------------------------------------------------------------------------
# Fixture constants
# ---------------------------------------------------------------------------

FIXTURE_CONFIG_YAML <- "
release_dates:
  3.22: 10/30/2025
  3.23: 04/15/2026
r_ver_for_bioc_ver:
  '3.22': '4.5'
  '3.23': '4.6'
"

# software VIEWS: only PkgSoft (PkgOld is absent -- it has been removed)
FIXTURE_VIEWS_SOFTWARE <- paste(
  "Package: PkgSoft",
  "Version: 1.2.0",
  "Title: The Soft Package",
  "Description: Does soft things.",
  "Maintainer: Alice Smith <alice@example.com>",
  "License: MIT",
  "biocViews: Software, Infrastructure",
  "git_url: https://git.bioconductor.org/packages/PkgSoft",
  "", sep = "\n")

# annotation VIEWS: PkgAnnot
FIXTURE_VIEWS_ANNOTATION <- paste(
  "Package: PkgAnnot",
  "Version: 2.0.0",
  "Title: The Annotation Package",
  "Description: Does annotation things.",
  "Maintainer: Bob Jones <bob@example.com>",
  "License: GPL-3",
  "biocViews: Annotation, GenomicAnnotation",
  "git_url: https://git.bioconductor.org/packages/PkgAnnot",
  "", sep = "\n")

FIXTURE_DESC <- list(
  PkgSoft = paste(
    'Package: PkgSoft',
    'Version: 1.2.0',
    'Title: The Soft Package',
    'Authors@R: person("Alice", "Smith", email = "alice@example.com", role = c("aut", "cre"))',
    'License: MIT',
    '', sep = "\n"),
  PkgAnnot = paste(
    'Package: PkgAnnot',
    'Version: 2.0.0',
    'Title: The Annotation Package',
    'Authors@R: person("Bob", "Jones", email = "bob@example.com", role = c("aut", "cre"))',
    'License: GPL-3',
    '', sep = "\n"),
  PkgOld = paste(
    'Package: PkgOld',
    'Version: 0.9.0',
    'Title: The Old Package',
    'Authors@R: person("Carol", "White", role = "aut")',
    'License: LGPL',
    '', sep = "\n")
)

# Branch vectors per package.
# PkgSoft and PkgAnnot are current (have RELEASE_3_23).
# PkgOld stopped at 3.22 -- it was removed before the current release.
FIXTURE_BRANCHES <- list(
  PkgSoft  = c("RELEASE_3_22", "RELEASE_3_23", "devel"),
  PkgAnnot = c("RELEASE_3_22", "RELEASE_3_23", "devel"),
  PkgOld   = c("RELEASE_3_22")
)

# ---------------------------------------------------------------------------
# Stub io builder
# ---------------------------------------------------------------------------

make_stub_io <- function(prev_pkgs = NULL, prev_auths = NULL, prev_manifest = list()) {
  all_repos <- c("PkgSoft", "PkgAnnot", "PkgOld")

  list(
    config_yaml = function() FIXTURE_CONFIG_YAML,

    fetch_views = function(cat) {
      switch(cat,
        software   = FIXTURE_VIEWS_SOFTWARE,
        annotation = FIXTURE_VIEWS_ANNOTATION,
        "")  # experiment, workflows: empty
    },

    list_repos = function() all_repos,

    ls_remote = function(pkg) {
      br <- FIXTURE_BRANCHES[[pkg]]
      if (is.null(br)) character(0L) else br
    },

    fetch_description = function(pkg, branch) {
      FIXTURE_DESC[[pkg]] %||% ""
    },

    prev_catalog = function() {
      if (is.null(prev_pkgs)) return(list(manifest = prev_manifest))
      list(
        packages = prev_pkgs,
        authors  = prev_auths %||% data.frame(
          package = character(0), given = character(0), family = character(0),
          email   = character(0), role  = character(0), orcid  = character(0),
          stringsAsFactors = FALSE),
        manifest = prev_manifest
      )
    }
  )
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("run_update force_full writes 3 packages (2 current, 1 removed) and authors", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io  <- make_stub_io()
  res <- run_update(io, out, force_full = TRUE)

  db_path <- file.path(out, "bioconductor-metadata.db")
  expect_true(file.exists(db_path))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  pkgs <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_packages ORDER BY name")
  expect_equal(nrow(pkgs), 3L)

  soft <- pkgs[pkgs$name == "PkgSoft", ]
  expect_equal(soft$in_current, 1L)
  expect_equal(soft$last_release, "3.23")
  expect_equal(soft$category, "software")

  annot <- pkgs[pkgs$name == "PkgAnnot", ]
  expect_equal(annot$in_current, 1L)
  expect_equal(annot$last_release, "3.23")
  expect_equal(annot$category, "annotation")

  old <- pkgs[pkgs$name == "PkgOld", ]
  expect_equal(old$in_current, 0L)
  expect_equal(old$last_release, "3.22")
  expect_true(old$last_release < "3.23")

  auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors ORDER BY family")
  expect_equal(nrow(auths), 3L)
  expect_true("Smith" %in% auths$family)
  expect_true("Jones" %in% auths$family)
  expect_true("White" %in% auths$family)
})

test_that("run_update returns a manifest with current_release and n_packages", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)

  expect_true(res$changed)
  man <- res$manifest
  expect_equal(man$current_release, "3.23")
  expect_equal(man$n_packages, 3L)
  expect_true(nzchar(man$generated_at))

  man_file <- file.path(out, "manifest.json")
  expect_true(file.exists(man_file))
  from_disk <- jsonlite::read_json(man_file)
  expect_equal(from_disk$current_release, "3.23")
})

test_that("run_update skips a package whose ls_remote throws and continues", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Override ls_remote to throw for PkgAnnot
  bad_io <- make_stub_io()
  bad_io$ls_remote <- function(pkg) {
    if (pkg == "PkgAnnot") stop("simulated network failure")
    br <- FIXTURE_BRANCHES[[pkg]]
    if (is.null(br)) character(0L) else br
  }

  res <- run_update(bad_io, out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con, "SELECT name FROM bioc_packages ORDER BY name")

  # PkgAnnot is skipped; PkgSoft and PkgOld should be present
  expect_false("PkgAnnot" %in% pkgs$name)
  expect_true("PkgSoft"  %in% pkgs$name)
  expect_true("PkgOld"   %in% pkgs$name)
})

test_that("run_update incremental only crawls new and removed packages", {
  tmp  <- withr::local_tempdir()
  out  <- file.path(tmp, "out")

  # Simulate a prior catalog that knows PkgSoft and PkgAnnot as current.
  # PkgOld was already removed (in_current=0) from a previous run.
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.1.0", "1.9.0", "0.9.0"),
    title              = c("Old Soft", "Old Annot", "Old Old"),
    description        = c("d", "d", "d"),
    maintainer         = c("Alice Smith", "Bob Jones", "Carol White"),
    maintainer_email   = c("alice@example.com", "bob@example.com", "carol@example.com"),
    license            = c("MIT", "GPL-3", "LGPL"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation", "Software"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.20", "3.21", "3.18"),
    first_release_date = c("2024-10-30", "2025-04-16", "2022-04-27"),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 0L),
    in_devel           = c(1L, 1L, 0L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )

  crawled <- character(0L)
  spy_io  <- make_stub_io(prev_pkgs = prev_pkgs)
  orig_ls <- spy_io$ls_remote
  spy_io$ls_remote <- function(pkg) {
    crawled <<- c(crawled, pkg)
    orig_ls(pkg)
  }

  run_update(spy_io, out, force_full = FALSE)

  # No new packages in views and both current packages already in prev ->
  # only packages newly added to or removed from views are crawled.
  # Here views has PkgSoft + PkgAnnot (already in prev), so crawl_set is empty.
  expect_equal(sort(crawled), character(0L))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con, "SELECT name, in_current FROM bioc_packages ORDER BY name")
  expect_equal(nrow(pkgs), 3L)
  expect_equal(pkgs$in_current[pkgs$name == "PkgSoft"],  1L)
  expect_equal(pkgs$in_current[pkgs$name == "PkgAnnot"], 1L)
  expect_equal(pkgs$in_current[pkgs$name == "PkgOld"],   0L)
})

test_that("run_update incremental crawls and includes new-in-views package absent from prior catalog", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # PkgNew is in software VIEWS but NOT in the prior catalog
  views_software_with_new <- paste(
    FIXTURE_VIEWS_SOFTWARE,
    "Package: PkgNew",
    "Version: 0.1.0",
    "Title: The New Package",
    "Description: Newly added.",
    "Maintainer: Dan Green <dan@example.com>",
    "License: MIT",
    "biocViews: Software",
    "git_url: https://git.bioconductor.org/packages/PkgNew",
    "", sep = "\n")

  # Prior catalog knows PkgSoft and PkgAnnot as current, PkgOld as removed.
  # PkgNew is deliberately absent -- it is new this cycle.
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.1.0", "1.9.0", "0.9.0"),
    title              = c("Old Soft", "Old Annot", "Old Old"),
    description        = c("d", "d", "d"),
    maintainer         = c("Alice Smith", "Bob Jones", "Carol White"),
    maintainer_email   = c("alice@example.com", "bob@example.com", "carol@example.com"),
    license            = c("MIT", "GPL-3", "LGPL"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation", "Software"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.20", "3.21", "3.18"),
    first_release_date = c("2024-10-30", "2025-04-16", "2022-04-27"),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 0L),
    in_devel           = c(1L, 1L, 0L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )

  crawled_ls   <- character(0L)
  crawled_desc <- character(0L)
  spy_io <- make_stub_io(prev_pkgs = prev_pkgs)

  # Inject PkgNew into software VIEWS
  spy_io$fetch_views <- function(cat) {
    if (cat == "software") return(views_software_with_new)
    switch(cat, annotation = FIXTURE_VIEWS_ANNOTATION, "")
  }

  # Spy on ls_remote and fetch_description
  orig_ls   <- spy_io$ls_remote
  orig_desc <- spy_io$fetch_description
  spy_io$ls_remote <- function(pkg) {
    crawled_ls <<- c(crawled_ls, pkg)
    if (pkg == "PkgNew") return(c("RELEASE_3_23", "devel"))
    orig_ls(pkg)
  }
  spy_io$fetch_description <- function(pkg, branch) {
    crawled_desc <<- c(crawled_desc, pkg)
    if (pkg == "PkgNew") return(paste(
      "Package: PkgNew",
      "Version: 0.1.0",
      "Title: The New Package",
      'Authors@R: person("Dan", "Green", email = "dan@example.com", role = c("aut", "cre"))',
      "License: MIT",
      "", sep = "\n"))
    orig_desc(pkg, branch)
  }

  run_update(spy_io, out, force_full = FALSE)

  # PkgNew must have been crawled via both ls_remote and fetch_description
  expect_true("PkgNew" %in% crawled_ls,
              label = "ls_remote called for new-in-views PkgNew")
  expect_true("PkgNew" %in% crawled_desc,
              label = "fetch_description called for PkgNew")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con, "SELECT name, in_current FROM bioc_packages ORDER BY name")

  # PkgNew must be present in the catalog with in_current = 1
  expect_true("PkgNew" %in% pkgs$name)
  expect_equal(pkgs$in_current[pkgs$name == "PkgNew"], 1L)
})

# ---------------------------------------------------------------------------
# Self-heal: current packages with NULL first_release are re-crawled
# ---------------------------------------------------------------------------

test_that("run_update incremental self-heals software packages with NULL first_release, excludes annotation", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Three current packages in the prior catalog:
  # - PkgSoft: software, first_release already populated -> NOT crawled (no null)
  # - PkgSoftNull: software, first_release = NA -> IS crawled (software + null)
  # - PkgAnnot: annotation, first_release = NA -> NOT crawled (annotation excluded)
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgSoftNull", "PkgAnnot"),
    name_lower         = c("pkgsoft", "pkgsoftnull", "pkgannot"),
    category           = c("software", "software", "annotation"),
    version            = c("1.2.0", "1.0.0", "2.0.0"),
    title              = c("The Soft Package", "The Soft Null Package", "The Annotation Package"),
    description        = c("Does soft things.", "Does more soft things.", "Does annotation things."),
    maintainer         = c("Alice Smith", "Alice Smith", "Bob Jones"),
    maintainer_email   = c("alice@example.com", "alice@example.com", "bob@example.com"),
    license            = c("MIT", "MIT", "GPL-3"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Software", "Annotation"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.20", NA_character_, NA_character_),
    first_release_date = c("2024-10-30", NA_character_, NA_character_),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 1L),
    in_devel           = c(1L, 1L, 1L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )

  crawled_ls   <- character(0L)
  crawled_desc <- character(0L)
  io <- make_stub_io(prev_pkgs = prev_pkgs)

  # Include PkgSoftNull in the software VIEWS so it passes the views_names filter
  orig_fetch_views <- io$fetch_views
  io$fetch_views <- function(cat) {
    if (cat == "software") return(paste(
      FIXTURE_VIEWS_SOFTWARE,
      "Package: PkgSoftNull",
      "Version: 1.0.0",
      "Title: The Soft Null Package",
      "Description: Does more soft things.",
      "Maintainer: Alice Smith <alice@example.com>",
      "License: MIT",
      "biocViews: Software",
      "git_url: https://git.bioconductor.org/packages/PkgSoftNull",
      "", sep = "\n"))
    orig_fetch_views(cat)
  }

  orig_ls   <- io$ls_remote
  orig_desc <- io$fetch_description
  io$ls_remote <- function(pkg) {
    crawled_ls <<- c(crawled_ls, pkg)
    if (pkg == "PkgSoftNull") return(c("RELEASE_3_22", "RELEASE_3_23", "devel"))
    orig_ls(pkg)
  }
  io$fetch_description <- function(pkg, branch) {
    crawled_desc <<- c(crawled_desc, pkg)
    if (pkg == "PkgSoftNull") return(paste(
      "Package: PkgSoftNull",
      "Version: 1.0.0",
      "Title: The Soft Null Package",
      'Authors@R: person("Alice", "Smith", email = "alice@example.com", role = c("aut", "cre"))',
      "License: MIT",
      "", sep = "\n"))
    orig_desc(pkg, branch)
  }

  run_update(io, out, force_full = FALSE)

  # PkgSoftNull (software, null first_release) must be crawled via both hooks
  expect_true("PkgSoftNull" %in% crawled_ls,
              label = "ls_remote called for PkgSoftNull (software, null first_release)")
  expect_true("PkgSoftNull" %in% crawled_desc,
              label = "fetch_description called for PkgSoftNull")

  # PkgAnnot (annotation, null first_release) must NOT be crawled
  expect_false("PkgAnnot" %in% crawled_ls,
               label = "PkgAnnot not crawled (annotation excluded from backfill)")
  expect_false("PkgAnnot" %in% crawled_desc,
               label = "fetch_description not called for PkgAnnot")

  # PkgSoft already has a first_release and must NOT be re-crawled
  expect_false("PkgSoft" %in% crawled_ls,
               label = "PkgSoft not re-crawled (already has first_release)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(
    con, "SELECT name, first_release FROM bioc_packages ORDER BY name")

  # PkgSoftNull must now have a non-NULL first_release (self-healed)
  snull <- pkgs[pkgs$name == "PkgSoftNull", ]
  expect_true(!is.na(snull$first_release) && nzchar(snull$first_release),
              label = "PkgSoftNull first_release self-healed from NULL")

  # PkgSoft first_release carries forward from prev unchanged
  soft <- pkgs[pkgs$name == "PkgSoft", ]
  expect_equal(soft$first_release, "3.20",
               label = "PkgSoft first_release unchanged (not re-crawled)")
})

# ---------------------------------------------------------------------------
# C1 regression: authors survive an incremental run that does not re-crawl
# ---------------------------------------------------------------------------

test_that("C1: bioc_authors carries forward for non-recrawled packages on incremental run", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Prior catalog: PkgSoft and PkgAnnot are current, PkgOld removed.
  # The incremental crawl_set will be empty (no new / removed packages in views).
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.2.0", "2.0.0", "0.9.0"),
    title              = c("The Soft Package", "The Annotation Package", "The Old Package"),
    description        = c("Does soft things.", "Does annotation things.", "d"),
    maintainer         = c("Alice Smith", "Bob Jones", "Carol White"),
    maintainer_email   = c("alice@example.com", "bob@example.com", "carol@example.com"),
    license            = c("MIT", "GPL-3", "LGPL"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation", "Software"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.22", "3.22", "3.18"),
    first_release_date = c("2025-10-30", "2025-10-30", "2022-04-27"),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 0L),
    in_devel           = c(1L, 1L, 0L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )
  prev_auths <- data.frame(
    package = c("PkgSoft", "PkgAnnot"),
    given   = c("Alice", "Bob"),
    family  = c("Smith", "Jones"),
    email   = c("alice@example.com", "bob@example.com"),
    role    = c("aut, cre", "aut, cre"),
    orcid   = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  crawled <- character(0L)
  io <- make_stub_io(prev_pkgs = prev_pkgs, prev_auths = prev_auths)
  orig_ls <- io$ls_remote
  io$ls_remote <- function(pkg) { crawled <<- c(crawled, pkg); orig_ls(pkg) }

  run_update(io, out, force_full = FALSE)

  # Confirm no packages were re-crawled (pure carry-forward run)
  expect_equal(crawled, character(0L), label = "crawl_set empty")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors ORDER BY family")
  expect_equal(nrow(auths), 2L,
               label = "authors carried forward from prior catalog")
  expect_true("Smith" %in% auths$family,
              label = "PkgSoft author Smith present")
  expect_true("Jones" %in% auths$family,
              label = "PkgAnnot author Jones present")
})

# ---------------------------------------------------------------------------
# Change detection: manifest$changed reflects real differences
# ---------------------------------------------------------------------------

# The fingerprint for the fixture VIEWS (PkgSoft 1.2.0, PkgAnnot 2.0.0):
# sorted "name:version" pairs joined by commas.
.FIXTURE_FP <- "PkgAnnot:2.0.0,PkgSoft:1.2.0"

# The releases fingerprint for the fixture config (3.22, 3.23 ordered ascending,
# each with their R version from r_ver_for_bioc_ver).
.FIXTURE_RELEASES_FP <- "3.22:4.5,3.23:4.6"

test_that("manifest$changed is FALSE on steady-state incremental run", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.2.0", "2.0.0", "0.9.0"),
    title              = c("The Soft Package", "The Annotation Package", "The Old Package"),
    description        = c("d", "d", "d"),
    maintainer         = c("Alice Smith", "Bob Jones", "Carol White"),
    maintainer_email   = c("alice@example.com", "bob@example.com", "carol@example.com"),
    license            = c("MIT", "GPL-3", "LGPL"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation", "Software"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.22", "3.22", "3.18"),
    first_release_date = c("2025-10-30", "2025-10-30", "2022-04-27"),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 0L),
    in_devel           = c(1L, 1L, 0L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )
  prev_manifest <- list(source = list(
    views_fingerprint    = .FIXTURE_FP,
    releases_fingerprint = .FIXTURE_RELEASES_FP
  ))

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)

  expect_false(res$manifest$changed,
               label = "changed=FALSE when nothing new or modified")
  expect_equal(res$manifest$source$views_fingerprint, .FIXTURE_FP,
               label = "fingerprint round-trips through manifest")
  expect_equal(res$manifest$source$releases_fingerprint, .FIXTURE_RELEASES_FP,
               label = "releases_fingerprint round-trips through manifest")
})

test_that("manifest$changed is TRUE on force_full even with matching fingerprint", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev_manifest <- list(source = list(views_fingerprint = .FIXTURE_FP))
  io  <- make_stub_io(prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = TRUE)

  expect_true(res$manifest$changed,
              label = "changed=TRUE when force_full=TRUE")
})

test_that("manifest$changed is TRUE when views fingerprint differs from prior", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Simulate a prior manifest with a stale fingerprint
  prev_manifest <- list(source = list(views_fingerprint = "PkgAnnot:1.0.0,PkgSoft:1.0.0"))

  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot"),
    name_lower         = c("pkgsoft", "pkgannot"),
    category           = c("software", "annotation"),
    version            = c("1.2.0", "2.0.0"),
    title              = c("The Soft Package", "The Annotation Package"),
    description        = c("d", "d"),
    maintainer         = c("Alice Smith", "Bob Jones"),
    maintainer_email   = c("alice@example.com", "bob@example.com"),
    license            = c("MIT", "GPL-3"),
    depends            = c(NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation"),
    git_url            = c(NA_character_, NA_character_),
    first_release      = c("3.22", "3.22"),
    first_release_date = c("2025-10-30", "2025-10-30"),
    last_release       = c("3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L),
    in_devel           = c(1L, 1L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)

  expect_true(res$manifest$changed,
              label = "changed=TRUE when VIEWS fingerprint changed")
})

# ---------------------------------------------------------------------------
# bioc_releases table in the written DB
# ---------------------------------------------------------------------------

test_that("run_update writes bioc_releases table with ordered rows and r_version", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  # Fixture has releases 3.22 and 3.23 with R versions from r_ver_for_bioc_ver
  rels <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_releases ORDER BY seq")
  expect_equal(nrow(rels), 2L)
  expect_equal(rels$version,   c("3.22", "3.23"))
  expect_equal(rels$seq,       c(1L, 2L))
  expect_equal(rels$released,  c("2025-10-30", "2026-04-15"))
  expect_equal(rels$r_version, c("4.5", "4.6"))
})

test_that("run_update manifest includes n_releases and releases_fingerprint", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)

  expect_equal(res$manifest$n_releases, 2L,
               label = "n_releases equals number of release entries")
  expect_equal(res$manifest$source$releases_fingerprint, .FIXTURE_RELEASES_FP,
               label = "releases_fingerprint in manifest source")
})

# ---------------------------------------------------------------------------
# Self-healing: prior manifest lacks releases_fingerprint -> changed=TRUE
# ---------------------------------------------------------------------------

test_that("manifest$changed is TRUE when prior manifest lacks releases_fingerprint", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Prior manifest matches current views fingerprint but predates releases_fingerprint
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.2.0", "2.0.0", "0.9.0"),
    title              = c("The Soft Package", "The Annotation Package", "The Old Package"),
    description        = c("d", "d", "d"),
    maintainer         = c("Alice Smith", "Bob Jones", "Carol White"),
    maintainer_email   = c("alice@example.com", "bob@example.com", "carol@example.com"),
    license            = c("MIT", "GPL-3", "LGPL"),
    depends            = c(NA_character_, NA_character_, NA_character_),
    imports            = c(NA_character_, NA_character_, NA_character_),
    suggests           = c(NA_character_, NA_character_, NA_character_),
    biocviews          = c("Software", "Annotation", "Software"),
    git_url            = c(NA_character_, NA_character_, NA_character_),
    first_release      = c("3.22", "3.22", "3.18"),
    first_release_date = c("2025-10-30", "2025-10-30", "2022-04-27"),
    last_release       = c("3.22", "3.22", "3.22"),
    last_release_date  = c("2025-10-30", "2025-10-30", "2025-10-30"),
    in_current         = c(1L, 1L, 0L),
    in_devel           = c(1L, 1L, 0L),
    updated_at         = c("2025-10-30T00:00:00Z", "2025-10-30T00:00:00Z",
                           "2025-10-30T00:00:00Z"),
    stringsAsFactors   = FALSE
  )
  # Old manifest has views_fingerprint but NO releases_fingerprint
  prev_manifest <- list(source = list(views_fingerprint = .FIXTURE_FP))

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)

  expect_true(res$manifest$changed,
              label = "changed=TRUE when prior manifest predates releases_fingerprint")
})
