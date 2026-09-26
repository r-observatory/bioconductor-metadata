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
  "vignettes: vignettes/PkgSoft/inst/doc/intro.html,",
  "        vignettes/PkgSoft/inst/doc/advanced.pdf",
  "vignetteTitles: Getting started,, briefly, Advanced use",
  "hasREADME: FALSE",
  "hasNEWS: TRUE",
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

FIXTURE_BIOCVIEWS_DOT_3_22 <- paste(
  'digraph G {',
  '  BiocViews -> OldView;',
  '  OldView -> OldChild;',
  '}', sep = "\n")

FIXTURE_BIOCVIEWS_DOT_3_23 <- paste(
  'digraph G {',
  '  BiocViews -> Software;',
  '  Software -> Infrastructure;',
  '}', sep = "\n")

.FIXTURE_BIOCVIEWS_FP_3_23 <- paste0(
  sort(c("BiocViews->Software", "Software->Infrastructure")),
  collapse = ","
)

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

make_stub_io <- function(prev_pkgs = NULL, prev_auths = NULL, prev_manifest = list(),
                         prev_view_edges = NULL, prev_names_all = NULL) {
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

    fetch_biocviews_dot = function(branch) {
      switch(branch,
        RELEASE_3_22 = FIXTURE_BIOCVIEWS_DOT_3_22,
        RELEASE_3_23 = FIXTURE_BIOCVIEWS_DOT_3_23,
        NULL)
    },

    prev_catalog = function() {
      if (is.null(prev_pkgs)) return(list(manifest = prev_manifest))
      list(
        packages = prev_pkgs,
        authors  = prev_auths %||% data.frame(
          package = character(0), given = character(0), family = character(0),
          email   = character(0), role  = character(0), orcid  = character(0),
          ror_id  = character(0), comment = character(0),
          stringsAsFactors = FALSE),
        view_edges = prev_view_edges %||% data.frame(
          release = character(0), parent = character(0), child = character(0),
          stringsAsFactors = FALSE),
        manifest = prev_manifest,
        names_all = prev_names_all %||% data.frame(
          name_lower = character(0), canonical_name = character(0),
          identity_state = character(0), first_seen = character(0),
          last_seen = character(0), stringsAsFactors = FALSE)
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

  # Simulate a prior catalog that knows PkgSoft and PkgAnnot as current, at the
  # versions VIEWS lists, so no version bump is re-crawled either.
  # PkgOld was already removed (in_current=0) from a previous run.
  prev_pkgs <- data.frame(
    name               = c("PkgSoft", "PkgAnnot", "PkgOld"),
    name_lower         = c("pkgsoft", "pkgannot", "pkgold"),
    category           = c("software", "annotation", "software"),
    version            = c("1.2.0", "2.0.0", "0.9.0"),
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
    ror_id  = c(NA_character_, "02nr0ka47"),
    comment = c("University X", NA_character_),
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
  expect_equal(auths$comment[auths$family == "Smith"], "University X")
  expect_equal(auths$ror_id[auths$family == "Jones"], "02nr0ka47")
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
    views_fingerprint     = .FIXTURE_FP,
    releases_fingerprint  = .FIXTURE_RELEASES_FP,
    biocviews_fingerprint = .FIXTURE_BIOCVIEWS_FP_3_23
  ))

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)

  expect_false(res$manifest$changed,
               label = "changed=FALSE when nothing new or modified")
  expect_equal(res$manifest$source$views_fingerprint, .FIXTURE_FP,
               label = "fingerprint round-trips through manifest")
  expect_equal(res$manifest$source$releases_fingerprint, .FIXTURE_RELEASES_FP,
               label = "releases_fingerprint round-trips through manifest")
  expect_equal(res$manifest$source$biocviews_fingerprint, .FIXTURE_BIOCVIEWS_FP_3_23,
               label = "biocviews_fingerprint round-trips through manifest")
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

# ---------------------------------------------------------------------------
# biocViews vocabulary per release
# ---------------------------------------------------------------------------

# Shared prev_pkgs fixture for biocviews tests.
.bv_prev_pkgs <- data.frame(
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

test_that("biocviews: current-release edges fetched; past-captured edges carried forward without refetch", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # 3.22 is already captured in prev; 3.23 is current (always re-fetched).
  prev_view_edges <- data.frame(
    release = c("3.22", "3.22"),
    parent  = c("BiocViews", "OldView"),
    child   = c("OldView",   "OldChild"),
    stringsAsFactors = FALSE
  )

  dot_calls <- character(0L)
  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_view_edges = prev_view_edges)
  io$fetch_biocviews_dot <- function(branch) {
    dot_calls <<- c(dot_calls, branch)
    if (branch == "RELEASE_3_23") return(FIXTURE_BIOCVIEWS_DOT_3_23)
    NULL  # 3.22 must not be called; guard returns NULL in case it is
  }

  run_update(io, out, force_full = FALSE)

  expect_false("RELEASE_3_22" %in% dot_calls,
               label = "past captured release 3.22 not re-fetched")
  expect_true("RELEASE_3_23" %in% dot_calls,
              label = "current release 3.23 always re-fetched")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  edges <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_view_edges ORDER BY release, parent, child")

  edges_23 <- edges[edges$release == "3.23", ]
  expect_true(nrow(edges_23) > 0L,
              label = "current-release 3.23 edges written to DB")
  expect_true(any(edges_23$parent == "BiocViews" & edges_23$child == "Software"),
              label = "BiocViews->Software edge present for 3.23")

  edges_22 <- edges[edges$release == "3.22", ]
  expect_equal(nrow(edges_22), 2L,
               label = "past 3.22 edges carried forward (2 rows)")
  expect_true(any(edges_22$parent == "BiocViews" & edges_22$child == "OldView"),
              label = "carried-forward BiocViews->OldView edge present for 3.22")
})

test_that("biocviews: fetch returning NULL for all branches contributes no edges and does not error", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io <- make_stub_io()
  io$fetch_biocviews_dot <- function(branch) NULL

  expect_no_error(run_update(io, out, force_full = TRUE))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  edges <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_view_edges")
  expect_equal(nrow(edges), 0L,
               label = "no edges written when all fetches return NULL")
})

# ---------------------------------------------------------------------------
# bioc_names_all table published each run
# ---------------------------------------------------------------------------

test_that("run_update writes bioc_names_all and records n_names/names_gate_ok in the manifest", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # The stub fixture only has 2 live packages, well under BIOC_LIVE_FLOOR, so
  # the gate fails; make_stub_io() has no prior names_all, so run_update falls
  # back to building the projection fresh from the just-assembled packages_df.
  res <- run_update(make_stub_io(), out, force_full = TRUE)

  expect_false(res$manifest$names_gate_ok,
               label = "gate fails below BIOC_LIVE_FLOOR with only 2 live packages")
  expect_equal(res$manifest$n_names, 3L,
               label = "n_names counts every row in the fallback projection")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  names_all <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_names_all ORDER BY name_lower")

  expect_equal(nrow(names_all), 3L)
  expect_equal(names_all$identity_state[names_all$name_lower == "pkgsoft"],  "live")
  expect_equal(names_all$identity_state[names_all$name_lower == "pkgannot"], "live")
  expect_equal(names_all$identity_state[names_all$name_lower == "pkgold"],   "archived")
})

test_that("run_update reuses a non-empty prior bioc_names_all on a gate-failing run instead of wiping it", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Prior catalog: PkgSoft and PkgAnnot current, PkgOld already removed. With
  # only 2 live packages the fresh projection would also fail well below
  # BIOC_LIVE_FLOOR, so the gate fails here too -- but a real, non-empty prior
  # bioc_names_all is injected. The regression this guards: %||% on a
  # data.frame tests column count, not row count, so an empty-but-columned
  # prior (the realistic shape prev_catalog() returns on a failed download or
  # a pre-feature DB) would previously be treated as "present" and passed
  # through unchanged; a genuinely non-empty prior must be reused verbatim.
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
  prev_names_all <- data.frame(
    name_lower = "prevonly", canonical_name = "PrevOnly",
    identity_state = "archived", first_seen = "2015-01-01",
    last_seen = "2025-01-01", stringsAsFactors = FALSE
  )

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_names_all = prev_names_all)
  res <- run_update(io, out, force_full = FALSE)

  expect_false(res$manifest$names_gate_ok,
               label = "gate fails below BIOC_LIVE_FLOOR with only 2 live packages")
  expect_equal(res$manifest$n_names, 1L,
               label = "n_names reflects the reused prior, not a fresh 3-row build")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  names_all <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_names_all")

  expect_equal(nrow(names_all), 1L,
               label = "the prior row is written, not wiped to empty")
  expect_equal(names_all$name_lower, "prevonly")
  expect_equal(names_all$canonical_name, "PrevOnly")
})

test_that("run_update builds bioc_names_all when the gate fails and the prior is empty-columned", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Prior catalog: PkgSoft and PkgAnnot current, PkgOld already removed. This
  # non-empty prev_pkgs makes has_prev TRUE and carries names forward into
  # packages_df, so build_bioc_names_all(packages_df) yields > 0 rows. With
  # only 2 live packages the gate fails well below BIOC_LIVE_FLOOR (1500).
  #
  # prev_names_all is left NULL, which is the realistic shape: prev_catalog()
  # falls back via %||% to an empty-but-5-columned data.frame, NOT a true
  # NULL. The buggy form (`prev$names_all %||% build_bioc_names_all(...)`)
  # tests column count, and an empty 5-column frame has length 5, so %||%
  # treats it as "present" and reuses it verbatim -- publishing an empty
  # bioc_names_all even though a real, non-empty projection could have been
  # built from packages_df. The fix instead checks nrow(prev$names_all) > 0L
  # explicitly, so it falls through to building fresh from packages_df.
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

  io  <- make_stub_io(prev_pkgs = prev_pkgs, prev_names_all = NULL)
  res <- run_update(io, out, force_full = FALSE)

  expect_false(res$manifest$names_gate_ok,
               label = "gate fails below BIOC_LIVE_FLOOR with only 2 live packages")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  names_all <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_names_all")

  expect_true(nrow(names_all) > 0L,
              label = "bioc_names_all built from packages_df, not the empty-columned prior")
})

test_that("biocviews: changed=TRUE when prior manifest lacks biocviews_fingerprint (self-heal)", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # Prior manifest has views + releases fingerprints but NO biocviews_fingerprint.
  prev_manifest <- list(source = list(
    views_fingerprint    = .FIXTURE_FP,
    releases_fingerprint = .FIXTURE_RELEASES_FP
  ))

  io  <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)

  expect_true(res$manifest$changed,
              label = "changed=TRUE when prior manifest predates biocviews_fingerprint")
  expect_true(nzchar(res$manifest$source$biocviews_fingerprint),
              label = "biocviews_fingerprint written to new manifest")
})

# ---------------------------------------------------------------------------
# VIEWS NEWS and README flags
# ---------------------------------------------------------------------------

test_that("run_update writes VIEWS flags for current packages and NA for packages outside VIEWS", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con,
    "SELECT name, has_news, views_has_readme FROM bioc_packages ORDER BY name")

  expect_identical(pkgs$has_news[pkgs$name == "PkgSoft"], 1L)
  expect_identical(pkgs$views_has_readme[pkgs$name == "PkgSoft"], 0L)
  # PkgAnnot's VIEWS record has neither field
  expect_identical(pkgs$has_news[pkgs$name == "PkgAnnot"], NA_integer_)
  # PkgOld is not in the current VIEWS
  expect_identical(pkgs$has_news[pkgs$name == "PkgOld"], NA_integer_)
  expect_identical(pkgs$views_has_readme[pkgs$name == "PkgOld"], NA_integer_)
})

test_that("run_update never carries VIEWS flags forward from the prior catalog", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev <- .bv_prev_pkgs
  prev$has_news <- c(0L, 1L, 1L)
  prev$views_has_readme <- c(1L, 1L, 1L)
  run_update(make_stub_io(prev_pkgs = prev), out, force_full = FALSE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con,
    "SELECT name, has_news, views_has_readme FROM bioc_packages ORDER BY name")

  expect_identical(pkgs$has_news[pkgs$name == "PkgSoft"], 1L)
  expect_identical(pkgs$views_has_readme[pkgs$name == "PkgSoft"], 0L)
  expect_identical(pkgs$has_news[pkgs$name %in% c("PkgAnnot", "PkgOld")],
                   c(NA_integer_, NA_integer_))
  expect_identical(pkgs$views_has_readme[pkgs$name %in% c("PkgAnnot", "PkgOld")],
                   c(NA_integer_, NA_integer_))
})

# ---------------------------------------------------------------------------
# bioc_vignettes from the current VIEWS
# ---------------------------------------------------------------------------

test_that("run_update writes bioc_vignettes for the current release and counts them in the manifest", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  v <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_vignettes ORDER BY package, seq")

  expect_equal(nrow(v), 2L)
  expect_equal(v$package, c("PkgSoft", "PkgSoft"))
  expect_equal(v$release, c("3.23", "3.23"))
  expect_equal(v$version, c("1.2.0", "1.2.0"))
  expect_equal(v$title, c("Getting started, briefly", "Advanced use"))
  expect_equal(v$url[1],
               "https://bioconductor.org/packages/3.23/bioc/vignettes/PkgSoft/inst/doc/intro.html")
  expect_equal(res$manifest$n_vignettes, 2L)
  expect_equal(res$manifest$tables$bioc_vignettes, 2L)
})

test_that("a package listed twice in one VIEWS file is written once instead of stopping the run", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io <- make_stub_io()
  io$fetch_views <- function(cat) {
    switch(cat,
      software   = paste(FIXTURE_VIEWS_SOFTWARE, FIXTURE_VIEWS_SOFTWARE, sep = "\n"),
      annotation = FIXTURE_VIEWS_ANNOTATION,
      "")
  }

  expect_message(run_update(io, out, force_full = TRUE), "PkgSoft more than once")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  v <- RSQLite::dbGetQuery(con, "SELECT package, seq FROM bioc_vignettes ORDER BY seq")
  expect_equal(v$package, c("PkgSoft", "PkgSoft"))
  expect_equal(v$seq, 1:2)
})

# ---------------------------------------------------------------------------
# Author refresh: one full crawl for the new columns, then version bumps
# ---------------------------------------------------------------------------

.prev_auths_six_cols <- data.frame(
  package = c("PkgSoft", "PkgAnnot"),
  given   = c("Alice", "Bob"),
  family  = c("Smith", "Jones"),
  email   = c("alice@example.com", "bob@example.com"),
  role    = c("aut, cre", "aut, cre"),
  orcid   = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
)

test_that("a prior bioc_authors without ror_id and comment triggers one full crawl", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  crawled <- character(0L)
  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_auths = .prev_auths_six_cols)
  orig_ls <- io$ls_remote
  io$ls_remote <- function(pkg) { crawled <<- c(crawled, pkg); orig_ls(pkg) }

  msgs <- capture_messages(run_update(io, out, force_full = FALSE))
  expect_true(any(grepl("lacks ror_id or comment", msgs)))
  expect_true(any(grepl("Crawled 3 packages in [0-9.]+ min; DESCRIPTION read for 3", msgs)))

  expect_setequal(crawled, c("PkgSoft", "PkgAnnot", "PkgOld"))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(bioc_authors)")
  expect_equal(info$name, BIOC_AUTHOR_COLS)
})

test_that("a failed DESCRIPTION read during the full crawl keeps that package's prior authors", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_auths = .prev_auths_six_cols)
  orig_desc <- io$fetch_description
  io$fetch_description <- function(pkg, branch) {
    if (pkg == "PkgAnnot") stop("simulated 504")
    orig_desc(pkg, branch)
  }

  suppressMessages(run_update(io, out, force_full = FALSE))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors ORDER BY package")
  annot <- auths[auths$package == "PkgAnnot", ]
  expect_equal(nrow(annot), 1L)
  expect_equal(annot$family, "Jones")
  expect_identical(annot$ror_id, NA_character_)
  expect_identical(annot$comment, NA_character_)
})

test_that("a VIEWS version bump re-reads that software package only, not a data package", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev <- .bv_prev_pkgs
  prev$version <- c("1.1.0", "1.9.0", "0.9.0")  # VIEWS lists 1.2.0 and 2.0.0
  prev_auths <- data.frame(
    package = c("PkgSoft", "PkgAnnot"), given = c("Old", "Bob"),
    family = c("Name", "Jones"), email = NA_character_, role = "aut, cre",
    orcid = NA_character_, ror_id = NA_character_, comment = NA_character_,
    stringsAsFactors = FALSE)

  crawled <- character(0L)
  io <- make_stub_io(prev_pkgs = prev, prev_auths = prev_auths)
  orig_desc <- io$fetch_description
  io$fetch_description <- function(pkg, branch) {
    crawled <<- c(crawled, pkg)
    orig_desc(pkg, branch)
  }

  expect_message(run_update(io, out, force_full = FALSE), "VIEWS version changed")

  expect_equal(crawled, "PkgSoft")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  auths <- RSQLite::dbGetQuery(con, "SELECT package, given, family FROM bioc_authors")
  expect_equal(auths$family[auths$package == "PkgSoft"], "Smith")
  expect_equal(auths$family[auths$package == "PkgAnnot"], "Jones")
})

test_that("a failed DESCRIPTION read on a version bump keeps the prior authors", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev <- .bv_prev_pkgs
  prev$version <- c("1.1.0", "2.0.0", "0.9.0")
  prev_auths <- data.frame(
    package = "PkgSoft", given = "Old", family = "Name", email = NA_character_,
    role = "aut, cre", orcid = NA_character_, ror_id = NA_character_,
    comment = "Kept", stringsAsFactors = FALSE)

  io <- make_stub_io(prev_pkgs = prev, prev_auths = prev_auths)
  io$fetch_description <- function(pkg, branch) stop("simulated 504")

  suppressMessages(run_update(io, out, force_full = FALSE))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors WHERE package = 'PkgSoft'")
  expect_equal(auths$family, "Name")
  expect_equal(auths$comment, "Kept")
})

test_that("version_bumped_packages ignores data packages, new packages and equal versions", {
  prev <- data.frame(name = c("S", "W", "A", "E", "Same"),
                     version = c("1.0", "1.0", "1.0", "1.0", "2.0"),
                     stringsAsFactors = FALSE)
  views <- data.frame(name = c("S", "W", "A", "E", "Same", "New"),
                      category = c("software", "workflows", "annotation",
                                   "experiment", "software", "software"),
                      version = c("1.1", "1.1", "1.1", "1.1", "2.0", "0.1"),
                      stringsAsFactors = FALSE)
  expect_equal(version_bumped_packages(prev, views), c("S", "W"))
  expect_equal(version_bumped_packages(NULL, views), character(0))
})

test_that("authors_need_migration is TRUE only when ror_id or comment is missing", {
  expect_true(authors_need_migration(.prev_auths_six_cols))
  expect_true(authors_need_migration(.prev_auths_six_cols[0, ]))
  expect_true(authors_need_migration(NULL))
  expect_false(authors_need_migration(empty_bioc_authors()))
})

test_that("a short repository listing stops the one-time author crawl before anything is written", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_auths = .prev_auths_six_cols)
  # PkgSoft is missing, as when gh api --paginate fails after a few pages
  io$list_repos <- function() c("PkgAnnot", "PkgOld")

  expect_error(suppressMessages(run_update(io, out, force_full = FALSE)),
               "Repository listing holds 0.0% of current software and workflows packages")
  expect_false(file.exists(file.path(out, "bioconductor-metadata.db")))
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("views_code_coverage counts only current software and workflows packages", {
  views <- data.frame(name = c("S1", "S2", "W1", "A1"),
                      category = c("software", "software", "workflows", "annotation"),
                      stringsAsFactors = FALSE)
  expect_equal(views_code_coverage(c("S1", "W1", "A1", "extra"), views), 2 / 3)
  expect_equal(views_code_coverage(character(0), views), 0)
  expect_equal(views_code_coverage(character(0), views[4, ]), 1)
})

test_that("a version bump whose DESCRIPTION has no Authors@R drops that package's prior rows", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev <- .bv_prev_pkgs
  prev$version <- c("1.1.0", "2.0.0", "0.9.0")
  prev_auths <- data.frame(
    package = c("PkgSoft", "PkgAnnot"), given = c("Old", "Bob"),
    family = c("Name", "Jones"), email = NA_character_, role = "aut, cre",
    orcid = NA_character_, ror_id = NA_character_, comment = NA_character_,
    stringsAsFactors = FALSE)

  io <- make_stub_io(prev_pkgs = prev, prev_auths = prev_auths)
  io$fetch_description <- function(pkg, branch) {
    paste("Package: PkgSoft", "Version: 1.2.0", "Author: Alice Smith",
          "Maintainer: Alice Smith <alice@example.com>", "", sep = "\n")
  }

  suppressMessages(run_update(io, out, force_full = FALSE))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  auths <- RSQLite::dbGetQuery(con, "SELECT package, family FROM bioc_authors")
  expect_false("PkgSoft" %in% auths$package)
  expect_equal(auths$family[auths$package == "PkgAnnot"], "Jones")
})

test_that("a version bump whose branch listing fails keeps the prior authors and lineage", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev <- .bv_prev_pkgs
  prev$version <- c("1.1.0", "2.0.0", "0.9.0")
  prev_auths <- data.frame(
    package = "PkgSoft", given = "Old", family = "Name", email = NA_character_,
    role = "aut, cre", orcid = NA_character_, ror_id = NA_character_,
    comment = "Kept", stringsAsFactors = FALSE)

  io <- make_stub_io(prev_pkgs = prev, prev_auths = prev_auths)
  # default_io's ls_remote returns no branches when git fails; it never throws
  io$ls_remote <- function(pkg) character(0L)
  fetched <- character(0L)
  io$fetch_description <- function(pkg, branch) {
    fetched <<- c(fetched, pkg)
    FIXTURE_DESC[[pkg]] %||% ""
  }

  msgs <- capture_messages(run_update(io, out, force_full = FALSE))
  expect_true(any(grepl(
    "Skipping PkgSoft: empty branch listing; keeping the prior catalog row", msgs,
    fixed = TRUE)))
  expect_equal(fetched, character(0L))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors WHERE package = 'PkgSoft'")
  expect_equal(auths$family, "Name")
  expect_equal(auths$comment, "Kept")
  soft <- RSQLite::dbGetQuery(con,
    "SELECT version, first_release, last_release, in_current FROM bioc_packages WHERE name = 'PkgSoft'")
  expect_equal(soft$version, "1.2.0")
  expect_equal(soft$first_release, "3.22")
  expect_equal(soft$last_release, "3.23")
  expect_equal(soft$in_current, 1L)
})

test_that("a new package with an empty branch listing still enters the catalog with NA lineage", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs)
  io$fetch_views <- function(cat) {
    if (cat == "software") return(paste(
      FIXTURE_VIEWS_SOFTWARE,
      "Package: PkgNew",
      "Version: 0.1.0",
      "Title: The New Package",
      "Description: Newly added.",
      "Maintainer: Dan Green <dan@example.com>",
      "License: MIT",
      "biocViews: Software",
      "", sep = "\n"))
    switch(cat, annotation = FIXTURE_VIEWS_ANNOTATION, "")
  }
  orig_ls <- io$ls_remote
  io$ls_remote <- function(pkg) if (pkg == "PkgNew") character(0L) else orig_ls(pkg)
  orig_desc <- io$fetch_description
  io$fetch_description <- function(pkg, branch) {
    if (pkg == "PkgNew") return(paste(
      "Package: PkgNew", "Version: 0.1.0",
      'Authors@R: person("Dan", "Green", role = c("aut", "cre"))',
      "", sep = "\n"))
    orig_desc(pkg, branch)
  }

  msgs <- capture_messages(run_update(io, out, force_full = FALSE))
  expect_false(any(grepl("Skipping PkgNew", msgs, fixed = TRUE)))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  new <- RSQLite::dbGetQuery(con,
    "SELECT first_release, last_release, in_current FROM bioc_packages WHERE name = 'PkgNew'")
  expect_equal(nrow(new), 1L)
  expect_identical(new$first_release, NA_character_)
  expect_identical(new$last_release, NA_character_)
  expect_equal(new$in_current, 1L)
})
