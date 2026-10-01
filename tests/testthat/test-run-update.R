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
                         prev_view_edges = NULL, prev_names_all = NULL,
                         build_files = list(), prev_state = list()) {
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

    # build_files is keyed "branch/repo/file"; anything else is a 404.
    fetch_build_file = function(branch, repo, file) {
      f <- build_files[[paste(branch, repo, file, sep = "/")]]
      if (is.null(f)) list(status = 404L, body = "", last_modified = NA_character_) else f
    },

    # prev_state carries the episode tables of an earlier run (see state_of).
    prev_catalog = function() {
      if (is.null(prev_pkgs)) return(c(list(manifest = prev_manifest), prev_state))
      c(list(
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
      ), prev_state)
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

# The publish-gate fingerprint of the stub's four VIEWS texts.
.FIXTURE_VIEWS_SHA <- views_sha256(list(software = FIXTURE_VIEWS_SOFTWARE,
                                        annotation = FIXTURE_VIEWS_ANNOTATION,
                                        experiment = "", workflows = ""))

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
    biocviews_fingerprint = .FIXTURE_BIOCVIEWS_FP_3_23,
    views_sha256          = .FIXTURE_VIEWS_SHA,
    builds_fingerprint    = "",
    schema                = BIOC_METADATA_SCHEMA
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
  prev_manifest <- list(source = list(views_fingerprint = .FIXTURE_FP,
                                       schema = BIOC_METADATA_SCHEMA))

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
    releases_fingerprint = .FIXTURE_RELEASES_FP,
    schema               = BIOC_METADATA_SCHEMA
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

# ---------------------------------------------------------------------------
# Schema version in the manifest
# ---------------------------------------------------------------------------

test_that("manifest$changed is TRUE on a schema bump with unchanged VIEWS", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  crawled <- character(0L)
  prev_manifest <- list(source = list(
    views_fingerprint     = .FIXTURE_FP,
    releases_fingerprint  = .FIXTURE_RELEASES_FP,
    biocviews_fingerprint = .FIXTURE_BIOCVIEWS_FP_3_23,
    schema                = 1L
  ))
  io  <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = prev_manifest)
  orig_ls <- io$ls_remote
  io$ls_remote <- function(pkg) { crawled <<- c(crawled, pkg); orig_ls(pkg) }
  res <- run_update(io, out, force_full = FALSE)

  expect_equal(crawled, character(0L), label = "nothing re-crawled")
  expect_true(res$manifest$changed)
  expect_equal(res$manifest$source$schema, BIOC_METADATA_SCHEMA)
  from_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_equal(from_disk$source$schema, 3L)
})

test_that("manifest$changed is TRUE when the prior manifest has no schema", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  prev_manifest <- list(source = list(
    views_fingerprint     = .FIXTURE_FP,
    releases_fingerprint  = .FIXTURE_RELEASES_FP,
    biocviews_fingerprint = .FIXTURE_BIOCVIEWS_FP_3_23
  ))
  io  <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = prev_manifest)
  res <- run_update(io, out, force_full = FALSE)
  expect_true(res$manifest$changed)
})

# ---------------------------------------------------------------------------
# Build reports
# ---------------------------------------------------------------------------

stub_file <- function(body, last_modified = "2026-09-29T16:35:46Z") {
  list(status = 200L, body = body, last_modified = last_modified)
}

# One stream's files: status lines, an index page naming the BioC version, the
# snapshot time (local, -0400) and the built versions, and optionally a
# propagation file.
stub_report <- function(version, snapshot, lines, prop = NULL,
                        versions = c(PkgSoft = "1.2.0")) {
  pkgs <- paste(sprintf('<B><A href="%s/">%s</A>&nbsp;%s</B>', names(versions),
                        names(versions), versions), collapse = "\n")
  index <- paste0(
    "<TITLE>Multiple platform build/check report for BioC ", version, "</TITLE>\n",
    "This page was generated on 2026-09-29 11:33 -0400 (Tue, 29 Sep 2026).\n",
    "<TD>Approx.&nbsp;Package&nbsp;Snapshot&nbsp;Date/Time&nbsp;(<SPAN>git&nbsp;pull</SPAN>):",
    "&nbsp;<SPAN>", snapshot, "&nbsp;-0400</SPAN></TD>\n", pkgs, "\n")
  out <- list(BUILD_STATUS_DB.txt = stub_file(paste(c(lines, ""), collapse = "\n")),
              index.html = stub_file(index))
  if (!is.null(prop)) {
    out$PROPAGATION_STATUS_DB.txt <- stub_file(paste(c(prop, ""), collapse = "\n"))
  }
  out
}

# All six streams, readable; software carries a propagation file, the data and
# workflows reports have none. release_check is PkgSoft's release check line.
all_build_files <- function(release_check = "PkgSoft#nebbiolo1#checksrc: ERROR",
                            release_snapshot = "2026-09-28&nbsp;13:40") {
  files <- list()
  add <- function(branch, repo, rep) {
    for (f in names(rep)) files[[paste(branch, repo, f, sep = "/")]] <<- rep[[f]]
  }
  prop <- "PkgSoft#source#propagate: UNNEEDED, same version is already published"
  add("release", "bioc", stub_report("3.23", release_snapshot,
                                     c("PkgSoft#nebbiolo1#install: OK", release_check),
                                     prop = prop))
  add("devel", "bioc", stub_report("3.24", "2026-09-28&nbsp;13:45",
                                   "PkgSoft#nebbiolo2#install: OK", prop = prop,
                                   versions = c(PkgSoft = "1.3.0")))
  for (b in c("release", "devel")) for (r in c("data-experiment", "workflows")) {
    add(b, r, stub_report(if (b == "release") "3.23" else "3.24", "2026-09-29&nbsp;07:00",
                          "PkgExp#nebbiolo1#install: OK", versions = c(PkgExp = "1.0.0")))
  }
  files
}

# The episode tables a run wrote, in the shape prev_catalog returns them.
state_of <- function(out) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con))
  opt <- function(t) {
    if (RSQLite::dbExistsTable(con, t)) RSQLite::dbGetQuery(con, sprintf("SELECT * FROM %s", t)) else NULL
  }
  list(build_reports = opt("bioc_build_reports"),
       build_status = opt("bioc_build_status_history"),
       views_history = opt("bioc_views_history"))
}

stream_of <- function(res, branch, repo) {
  Filter(function(x) x$branch == branch && x$repo == repo, res$manifest$builds)[[1]]
}

test_that("run_update writes the build tables with censored first episodes", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- suppressMessages(run_update(make_stub_io(build_files = all_build_files()),
                                     out, force_full = TRUE))

  st <- state_of(out)
  expect_equal(nrow(st$build_reports), 6L)
  expect_setequal(st$build_reports$outcome, "applied")
  rel <- st$build_reports[st$build_reports$branch == "release" & st$build_reports$repo == "bioc", ]
  expect_equal(rel$bioc_version, "3.23")
  expect_equal(rel$report_at, "2026-09-28T17:40:00Z")
  expect_equal(rel$published_at, "2026-09-29T16:35:46Z")
  expect_equal(rel$nodes, "nebbiolo1")

  h <- st$build_status
  soft <- h[h$package == "PkgSoft" & h$bioc_version == "3.23", ]
  expect_setequal(soft$stage, c("install", "checksrc", "propagate"))
  expect_equal(soft$status[soft$stage == "checksrc"], "ERROR")
  expect_true(all(soft$first_seen_exact == 0L))
  expect_true(all(soft$first_version == "1.2.0"))
  expect_equal(h$first_version[h$package == "PkgSoft" & h$bioc_version == "3.24" &
                                 h$stage == "install"], "1.3.0")

  expect_true(res$manifest$builds_ok)
  expect_length(res$manifest$builds, 6L)
  expect_equal(stream_of(res, "release", "workflows")$propagation, "absent")
  expect_equal(res$manifest$tables$bioc_build_status_history, nrow(h))
})

test_that("the same reports read twice change nothing", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  files <- all_build_files()
  suppressMessages(run_update(make_stub_io(build_files = files), out1, force_full = TRUE))
  first <- state_of(out1)

  res <- suppressMessages(run_update(
    make_stub_io(build_files = files, prev_state = first), out2, force_full = TRUE))
  second <- state_of(out2)

  expect_equal(second$build_reports, first$build_reports)
  expect_equal(second$build_status, first$build_status)
  expect_setequal(vapply(res$manifest$builds, `[[`, "", "outcome"), "unchanged")
  expect_true(res$manifest$builds_ok)
})

test_that("a status flip in a newer report closes one episode and opens the next", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE))
  newer <- all_build_files(release_check = "PkgSoft#nebbiolo1#checksrc: OK",
                           release_snapshot = "2026-09-29&nbsp;13:40")
  suppressMessages(run_update(make_stub_io(build_files = newer, prev_state = state_of(out1)),
                              out2, force_full = TRUE))
  h <- state_of(out2)$build_status
  chk <- h[h$package == "PkgSoft" & h$bioc_version == "3.23" & h$stage == "checksrc", ]
  expect_equal(chk$status, c("ERROR", "OK"))
  expect_equal(chk$end_reason, c("changed", NA))
  expect_equal(chk$ended_on[1], "2026-09-29T17:40:00Z")
  expect_equal(chk$first_seen_exact, c(0L, 1L))
})

test_that("a failed build stream keeps its prior rows and never stops the catalog", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE))
  prior <- state_of(out1)
  broken <- all_build_files(release_snapshot = "2026-09-29&nbsp;13:40")
  broken[["release/bioc/BUILD_STATUS_DB.txt"]] <-
    stub_file("<html><body>502 Bad Gateway</body></html>")

  res <- suppressMessages(run_update(make_stub_io(build_files = broken, prev_state = prior),
                                     out2, force_full = TRUE))

  expect_false(res$manifest$builds_ok)
  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "fetch_failed")
  expect_equal(rel$reason, "status file failed validation")
  h <- state_of(out2)$build_status
  old <- prior$build_status
  keep <- h$bioc_version == "3.23" & h$repo == "bioc"
  expect_equal(h[keep, ], old[old$bioc_version == "3.23" & old$repo == "bioc", ],
               ignore_attr = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out2, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM bioc_packages")$n, 3L)
})

test_that("a propagation file that is there but unreadable fails its stream", {
  tmp <- withr::local_tempdir()
  files <- all_build_files()
  files[["release/bioc/PROPAGATION_STATUS_DB.txt"]] <- stub_file("<html>Gateway Timeout</html>")
  res <- suppressMessages(run_update(make_stub_io(build_files = files),
                                     file.path(tmp, "out"), force_full = TRUE))
  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "fetch_failed")
  expect_equal(rel$reason, "propagation file not read")
  expect_false(res$manifest$builds_ok)
  h <- state_of(file.path(tmp, "out"))$build_status
  expect_false(any(h$bioc_version == "3.23" & h$repo == "bioc"))
})

# Text ending in one byte that is not valid UTF-8, marked the way http_get
# marks a body.
with_bad_byte <- function(text) {
  b <- rawToChar(c(charToRaw(text), as.raw(0xe9)))
  Encoding(b) <- "UTF-8"
  b
}

test_that("an index page with an invalid byte never stops the catalog", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  files <- all_build_files()
  files[["release/bioc/index.html"]]$body <-
    with_bad_byte(files[["release/bioc/index.html"]]$body)
  io <- make_stub_io(build_files = files)
  io$config_yaml <- function() {
    paste0(FIXTURE_CONFIG_YAML, "release_version: \"3.23\"\ndevel_version: \"3.24\"\n")
  }

  res <- suppressMessages(run_update(io, out, force_full = TRUE, live_floor = 1L))

  expect_true(res$status$catalog_ok)
  expect_true(file.exists(file.path(out, "status.json")))
  # The page gives nothing, so config.yaml names the version and the status
  # file's Last-Modified times the report.
  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "applied")
  expect_equal(rel$bioc_version, "3.23")
  expect_equal(rel$report_at, "2026-09-29T16:35:46Z")
  expect_equal(stream_of(res, "devel", "bioc")$report_at, "2026-09-28T17:45:00Z")
})

test_that("an error while one stream is read fails that stream only", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE))
  prior <- state_of(out1)
  broken <- all_build_files(release_snapshot = "2026-09-29&nbsp;13:40")
  # A body that is not text makes the parser throw instead of returning invalid.
  broken[["release/bioc/BUILD_STATUS_DB.txt"]]$body <- 1L

  res <- suppressMessages(run_update(make_stub_io(build_files = broken, prev_state = prior),
                                     out2, force_full = TRUE, live_floor = 1L))

  expect_true(res$status$catalog_ok)
  expect_false(res$status$builds_ok)
  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "fetch_failed")
  expect_match(rel$reason, "non-character argument", fixed = TRUE)
  expect_equal(stream_of(res, "devel", "bioc")$outcome, "unchanged")
  h <- state_of(out2)$build_status
  old <- prior$build_status
  expect_equal(h[h$bioc_version == "3.23" & h$repo == "bioc", ],
               old[old$bioc_version == "3.23" & old$repo == "bioc", ], ignore_attr = TRUE)
})

test_that("an index page that cannot be fetched fails its stream, and a missing one does not", {
  tmp <- withr::local_tempdir()
  files <- all_build_files()
  files[["release/bioc/index.html"]] <- list(status = 503L, body = "",
                                             last_modified = NA_character_)
  files[["devel/bioc/index.html"]] <- NULL
  io <- make_stub_io(build_files = files)
  io$config_yaml <- function() {
    paste0(FIXTURE_CONFIG_YAML, "release_version: \"3.23\"\ndevel_version: \"3.24\"\n")
  }
  fetch <- io$fetch_build_file
  io$fetch_build_file <- function(branch, repo, file) {
    if (branch == "devel" && repo == "workflows" && file == "index.html") {
      stop("Timeout was reached")
    }
    fetch(branch, repo, file)
  }

  res <- suppressMessages(run_update(io, file.path(tmp, "out"), force_full = TRUE,
                                     live_floor = 1L))

  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "fetch_failed")
  expect_equal(rel$reason, "index page not read (HTTP 503)")
  wf <- stream_of(res, "devel", "workflows")
  expect_equal(wf$outcome, "fetch_failed")
  expect_equal(wf$reason, "index page not read (HTTP error)")
  # A 404 is a report without the page: it applies on config.yaml's version.
  dev <- stream_of(res, "devel", "bioc")
  expect_equal(dev$outcome, "applied")
  expect_equal(dev$bioc_version, "3.24")
  expect_true(res$status$catalog_ok)
  expect_false(res$status$builds_ok)
})

test_that("a config.yaml that cannot be parsed gives no branch versions and says why", {
  for (text in list("release_version: [unclosed", "just text", NA_character_,
                    "release_version: ['3.22', '3.23']\n")) {
    expect_message(v <- config_branch_versions(text),
                   "config.yaml release and devel versions not read", fixed = TRUE)
    expect_equal(v, c(release = NA_character_, devel = NA_character_))
  }
  expect_no_message(v <- config_branch_versions("release_version: \"3.23\"\n"))
  expect_equal(v, c(release = "3.23", devel = NA_character_))
})

test_that("unparsed branch versions skip only the streams whose index page gives no version", {
  tmp <- withr::local_tempdir()
  files <- all_build_files()
  files[["devel/bioc/index.html"]] <- NULL
  files[["devel/workflows/index.html"]] <- NULL
  # The parser is stood in for, since release dates come from the same text.
  real <- parse_branch_versions
  withr::defer(assign("parse_branch_versions", real, envir = globalenv()))
  assign("parse_branch_versions", function(yaml_text) stop("Parser error: bad yaml"),
         envir = globalenv())

  msgs <- character(0)
  expect_no_warning(withCallingHandlers(
    res <- run_update(make_stub_io(build_files = files), file.path(tmp, "out"),
                      force_full = TRUE, live_floor = 1L),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }))

  expect_equal(sum(grepl("config.yaml release and devel versions not read: Parser error: bad yaml",
                         msgs, fixed = TRUE)), 1L)
  for (repo in c("bioc", "workflows")) {
    dev <- stream_of(res, "devel", repo)
    expect_equal(dev$outcome, "fetch_failed", label = repo)
    expect_equal(dev$reason, "BioC version unknown", label = repo)
  }
  expect_true(any(grepl("Build report devel/bioc: BioC version unknown", msgs, fixed = TRUE)))
  expect_equal(stream_of(res, "release", "bioc")$outcome, "applied")
  expect_equal(stream_of(res, "devel", "data-experiment")$outcome, "applied")
  expect_true(res$status$catalog_ok)
  expect_false(res$status$builds_ok)
})

# all_build_files() with `prop` as the release software propagation lines. The
# status file's own Last-Modified makes it a new report though its lines match.
with_release_propagation <- function(prop, snapshot = "2026-09-28&nbsp;13:40",
                                     last_modified = "2026-09-29T16:35:46Z") {
  files <- all_build_files(release_snapshot = snapshot)
  files[["release/bioc/BUILD_STATUS_DB.txt"]]$last_modified <- last_modified
  files[["release/bioc/PROPAGATION_STATUS_DB.txt"]] <-
    if (is.null(prop)) NULL else stub_file(paste(c(prop, ""), collapse = "\n"))
  files
}
release_propagation_rows <- function(out) {
  h <- state_of(out)$build_status
  h[h$bioc_version == "3.23" & h$repo == "bioc" & h$stage == "propagate", ]
}
DAY2_SNAPSHOT <- "2026-09-29&nbsp;13:40"
DAY2_MODIFIED <- "2026-09-30T16:35:46Z"

test_that("an empty propagation file leaves the propagation rows open and fails the build check", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE))
  day2 <- all_build_files(release_check = "PkgSoft#nebbiolo1#checksrc: OK",
                          release_snapshot = DAY2_SNAPSHOT)
  day2[["release/bioc/PROPAGATION_STATUS_DB.txt"]] <- stub_file("")

  res <- suppressMessages(run_update(
    make_stub_io(build_files = day2, prev_state = state_of(out1)), out2,
    force_full = TRUE, live_floor = 1L))

  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "applied")
  expect_equal(rel$propagation, "skipped_floor")
  expect_equal(rel$reason, "propagation file lists 0 packages against 1 with open rows")
  expect_true(res$status$catalog_ok)
  expect_false(res$status$builds_ok)
  p <- release_propagation_rows(out2)
  expect_equal(nrow(p), 1L)
  expect_true(is.na(p$ended_on))
  expect_equal(p$last_seen, "2026-09-28T17:40:00Z")
  # The status lines of the same report still apply.
  h <- state_of(out2)$build_status
  chk <- h[h$package == "PkgSoft" & h$bioc_version == "3.23" & h$stage == "checksrc", ]
  expect_equal(chk$status, c("ERROR", "OK"))
  # The file that was not trusted is not archived over the last good one.
  archived <- unlist(res$archive_files)
  expect_true("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% archived)
  expect_false("3.23/builds/bioc/PROPAGATION_STATUS_DB.txt" %in% archived)
})

test_that("a propagation file under half its open rows leaves them open, and half applies", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one")
  four <- sprintf("Pkg%s#source#propagate: UNNEEDED, same version is already published",
                  c("A", "B", "C", "D"))
  suppressMessages(run_update(make_stub_io(build_files = with_release_propagation(four)),
                              out1, force_full = TRUE))
  prior <- state_of(out1)
  day2 <- function(prop, out) {
    files <- with_release_propagation(prop, DAY2_SNAPSHOT, DAY2_MODIFIED)
    suppressMessages(run_update(make_stub_io(build_files = files, prev_state = prior),
                                file.path(tmp, out), force_full = TRUE, live_floor = 1L))
  }

  res <- day2(four[1], "one-of-four")
  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "applied")
  expect_equal(rel$propagation, "skipped_floor")
  expect_false(res$status$builds_ok)
  p <- release_propagation_rows(file.path(tmp, "one-of-four"))
  expect_equal(nrow(p), 4L)
  expect_true(all(is.na(p$ended_on)))
  expect_setequal(p$last_seen, "2026-09-28T17:40:00Z")

  res <- day2(four[1:2], "two-of-four")
  expect_equal(stream_of(res, "release", "bioc")$propagation, "read")
  expect_true(res$status$builds_ok)
  p <- release_propagation_rows(file.path(tmp, "two-of-four"))
  expect_equal(p$end_reason[match(c("PkgA", "PkgB", "PkgC", "PkgD"), p$package)],
               c(NA, NA, "gone", "gone"))
})

test_that("a first propagation file applies when no propagation rows are open", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  res1 <- suppressMessages(run_update(
    make_stub_io(build_files = with_release_propagation(NULL)), out1, force_full = TRUE))
  expect_equal(stream_of(res1, "release", "bioc")$propagation, "absent")
  expect_equal(nrow(release_propagation_rows(out1)), 0L)

  files <- with_release_propagation("PkgSoft#source#propagate: YES", DAY2_SNAPSHOT,
                                    DAY2_MODIFIED)
  res <- suppressMessages(run_update(
    make_stub_io(build_files = files, prev_state = state_of(out1)), out2,
    force_full = TRUE, live_floor = 1L))

  rel <- stream_of(res, "release", "bioc")
  expect_equal(rel$outcome, "applied")
  expect_equal(rel$propagation, "read")
  expect_true(res$status$builds_ok)
  p <- release_propagation_rows(out2)
  expect_equal(p$status, "YES")
  expect_equal(p$first_seen, "2026-09-29T17:40:00Z")
  expect_true(is.na(p$ended_on))
})

test_that("the release rollover retires the old version's open rows", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE))
  rolled <- list()
  add <- function(branch, repo, rep) {
    for (f in names(rep)) rolled[[paste(branch, repo, f, sep = "/")]] <<- rep[[f]]
  }
  for (r in BUILD_REPOS) {
    add("release", r, stub_report("3.24", "2026-10-29&nbsp;13:40", "PkgSoft#nebbiolo2#install: OK",
                                  prop = "PkgSoft#source#propagate: YES"))
    add("devel", r, stub_report("3.25", "2026-10-29&nbsp;13:45", "PkgSoft#nebbiolo1#install: OK",
                                prop = "PkgSoft#source#propagate: YES"))
  }
  res <- suppressMessages(run_update(make_stub_io(build_files = rolled, prev_state = state_of(out1)),
                                     out2, force_full = TRUE))
  h <- state_of(out2)$build_status
  old <- h[h$bioc_version == "3.23", ]
  expect_true(nrow(old) > 0L)
  expect_setequal(old$end_reason, "retired")
  expect_true(all(is.na(h$ended_on[h$bioc_version == "3.25"])))
  expect_gt(res$manifest$builds_retired, 0L)
})

test_that("run_update writes the VIEWS columns for current packages and NA for removed ones", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  io <- make_stub_io()
  io$fetch_views <- function(cat) {
    switch(cat,
      software = sub("hasNEWS: TRUE", paste("hasNEWS: TRUE", "PackageStatus: Deprecated",
                                            "LinkingTo: Rhtslib", "dependencyCount: 4",
                                            "Author: Alice Smith [aut, cre]", sep = "\n"),
                     FIXTURE_VIEWS_SOFTWARE, fixed = TRUE),
      annotation = FIXTURE_VIEWS_ANNOTATION,
      "")
  }
  suppressMessages(run_update(io, out, force_full = TRUE))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "bioconductor-metadata.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  pkgs <- RSQLite::dbGetQuery(con, paste(
    "SELECT name, package_status, linking_to, dependency_count, author_text",
    "FROM bioc_packages ORDER BY name"))
  soft <- pkgs[pkgs$name == "PkgSoft", ]
  expect_equal(soft$package_status, "Deprecated")
  expect_equal(soft$linking_to, "Rhtslib")
  expect_identical(soft$dependency_count, 4L)
  expect_equal(soft$author_text, "Alice Smith [aut, cre]")
  old <- pkgs[pkgs$name == "PkgOld", ]
  expect_identical(old$package_status, NA_character_)
  expect_identical(old$dependency_count, NA_integer_)
})

# ---------------------------------------------------------------------------
# VIEWS history
# ---------------------------------------------------------------------------

with_last_modified <- function(text, at) structure(text, last_modified = at)

test_that("run_update keeps VIEWS values as episodes timed by each file's Last-Modified", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  io <- make_stub_io()
  io$fetch_views <- function(cat) {
    switch(cat,
      software = with_last_modified(FIXTURE_VIEWS_SOFTWARE, "2026-09-29T18:14:30Z"),
      annotation = with_last_modified(FIXTURE_VIEWS_ANNOTATION, "2026-09-20T10:00:00Z"),
      "")
  }
  res1 <- suppressMessages(run_update(io, out1, force_full = TRUE, live_floor = 1L))
  h1 <- state_of(out1)$views_history
  v <- h1[h1$package == "PkgSoft" & h1$field == "Version", ]
  expect_equal(v$value, "1.2.0")
  expect_equal(v$first_seen, "2026-09-29T18:14:30Z")
  expect_equal(v$first_seen_exact, 0L)
  expect_equal(v$bioc_version, "3.23")
  expect_equal(h1$first_seen[h1$package == "PkgAnnot"], "2026-09-20T10:00:00Z")
  expect_equal(res1$manifest$views_history$new, 2L)

  io2 <- make_stub_io(prev_state = state_of(out1))
  io2$fetch_views <- function(cat) {
    switch(cat,
      software = with_last_modified(
        sub("hasNEWS: TRUE", "hasNEWS: TRUE\nPackageStatus: Deprecated",
            FIXTURE_VIEWS_SOFTWARE, fixed = TRUE), "2026-09-30T18:14:30Z"),
      annotation = with_last_modified(FIXTURE_VIEWS_ANNOTATION, "2026-09-20T10:00:00Z"),
      "")
  }
  suppressMessages(run_update(io2, out2, force_full = TRUE, live_floor = 1L))
  h2 <- state_of(out2)$views_history
  dep <- h2[h2$package == "PkgSoft" & h2$field == "PackageStatus", ]
  expect_equal(dep$value, "Deprecated")
  expect_equal(dep$first_seen, "2026-09-30T18:14:30Z")
  expect_equal(dep$first_seen_exact, 1L)
  expect_equal(h2$last_seen[h2$package == "PkgSoft" & h2$field == "Version"],
               "2026-09-30T18:14:30Z")
})

test_that("a failed names gate leaves the VIEWS history as it was", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(), out1, force_full = TRUE, live_floor = 1L))
  prior <- state_of(out1)$views_history
  io <- make_stub_io(prev_state = state_of(out1))
  io$fetch_views <- function(cat) if (cat == "annotation") FIXTURE_VIEWS_ANNOTATION else ""
  res <- suppressMessages(run_update(io, out2, force_full = TRUE))
  expect_false(res$manifest$names_gate_ok)
  after <- state_of(out2)$views_history
  expect_equal(after[order(after$package, after$field), ], prior[order(prior$package, prior$field), ],
               ignore_attr = TRUE)
})

test_that("a category that parses to nothing keeps its episodes open", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  suppressMessages(run_update(make_stub_io(), out1, force_full = TRUE, live_floor = 1L))
  io <- make_stub_io(prev_state = state_of(out1))
  io$fetch_views <- function(cat) if (cat == "software") FIXTURE_VIEWS_SOFTWARE else ""
  res <- suppressMessages(run_update(io, out2, force_full = TRUE, live_floor = 1L))
  annot <- state_of(out2)$views_history
  annot <- annot[annot$package == "PkgAnnot", ]
  expect_equal(nrow(annot), 1L)
  expect_true(is.na(annot$ended_on))
  expect_false("annotation" %in% unlist(res$manifest$views_history$applied))
})

# ---------------------------------------------------------------------------
# Upstream files for the archive branch
# ---------------------------------------------------------------------------

test_that("VIEWS and the newest applied reports are written under upstream/", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  io <- make_stub_io(build_files = all_build_files())
  io$fetch_views <- function(cat) {
    switch(cat,
      software = structure(FIXTURE_VIEWS_SOFTWARE, last_modified = "2026-09-29T18:14:30Z",
                           raw = paste0(FIXTURE_VIEWS_SOFTWARE, "\n")),
      annotation = FIXTURE_VIEWS_ANNOTATION, "")
  }
  res <- suppressMessages(run_update(io, out, force_full = TRUE, live_floor = 1L))
  files <- res$archive_files
  expect_true("3.23/views/software/VIEWS" %in% files)
  expect_false("3.23/views/experiment/VIEWS" %in% files)
  expect_true("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% files)
  expect_true("3.24/builds/bioc/PROPAGATION_STATUS_DB.txt" %in% files)
  expect_false("3.23/builds/workflows/PROPAGATION_STATUS_DB.txt" %in% files)
  expect_equal(readLines(file.path(out, "upstream/3.23/views/software/VIEWS"))[1],
               "Package: PkgSoft")
  expect_match(paste(readLines(file.path(out, "archive-message.txt")), collapse = "\n"),
               "3.23/views/software/VIEWS (published 2026-09-29T18:14:30Z)", fixed = TRUE)
})

test_that("the archive message leaves out the published time of a file that has none", {
  files <- list(list(path = "3.23/views/software/VIEWS", last_modified = "2026-09-29T18:14:30Z"),
                list(path = "3.23/builds/bioc/BUILD_STATUS_DB.txt", last_modified = NA_character_),
                list(path = "3.23/builds/bioc/PROPAGATION_STATUS_DB.txt", last_modified = NULL),
                list(path = "3.23/views/workflows/VIEWS", last_modified = ""))
  expect_equal(strsplit(archive_message(files, "2026-10-01T06:20:00Z"), "\n")[[1]],
               c("Bioconductor files as read at 2026-10-01T06:20:00Z", "",
                 "3.23/views/software/VIEWS (published 2026-09-29T18:14:30Z)",
                 "3.23/builds/bioc/BUILD_STATUS_DB.txt",
                 "3.23/builds/bioc/PROPAGATION_STATUS_DB.txt",
                 "3.23/views/workflows/VIEWS"))
})

test_that("a file fetched without a Last-Modified is archived without a published time", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  files <- all_build_files()
  files[["release/bioc/BUILD_STATUS_DB.txt"]]$last_modified <- NA_character_
  files[["devel/bioc/PROPAGATION_STATUS_DB.txt"]]$last_modified <- NA_character_
  io <- make_stub_io(build_files = files)
  io$fetch_views <- function(cat) {
    switch(cat,
      software = structure(FIXTURE_VIEWS_SOFTWARE, last_modified = "2026-09-29T18:14:30Z"),
      annotation = FIXTURE_VIEWS_ANNOTATION, "")
  }
  suppressMessages(run_update(io, out, force_full = TRUE, live_floor = 1L))
  msg <- readLines(file.path(out, "archive-message.txt"))
  expect_true("3.23/views/software/VIEWS (published 2026-09-29T18:14:30Z)" %in% msg)
  expect_true("3.23/views/annotation/VIEWS" %in% msg)
  expect_true("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% msg)
  expect_true("3.23/builds/bioc/PROPAGATION_STATUS_DB.txt (published 2026-09-29T16:35:46Z)" %in% msg)
  expect_true("3.24/builds/bioc/PROPAGATION_STATUS_DB.txt" %in% msg)
})

test_that("a report read again stays in the archive list and a stale copy does not", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two"); out3 <- file.path(tmp, "three")
  suppressMessages(run_update(make_stub_io(build_files = all_build_files()), out1,
                              force_full = TRUE, live_floor = 1L))
  first <- state_of(out1)

  # The same reports again: unchanged, yet still archived, so a push that
  # failed after the first run is made good by the next one.
  again <- suppressMessages(run_update(
    make_stub_io(build_files = all_build_files(), prev_state = first), out2,
    force_full = TRUE, live_floor = 1L))
  expect_true("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% again$archive_files)

  # An older copy of the release report must never replace the newer file.
  stale <- suppressMessages(run_update(
    make_stub_io(build_files = all_build_files(release_snapshot = "2026-09-27&nbsp;13:40"),
                 prev_state = first), out3, force_full = TRUE, live_floor = 1L))
  expect_false("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% stale$archive_files)
  expect_true("3.24/builds/bioc/BUILD_STATUS_DB.txt" %in% stale$archive_files)
  expect_false(file.exists(file.path(out3, "upstream/3.23/builds/bioc/BUILD_STATUS_DB.txt")))
})

test_that("a run clears the upstream files an earlier run left in the same out dir", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  dir.create(file.path(out, "upstream", "3.22"), recursive = TRUE)
  writeLines("old", file.path(out, "upstream", "3.22", "VIEWS"))
  suppressMessages(run_update(make_stub_io(), out, force_full = TRUE))
  expect_false(file.exists(file.path(out, "upstream", "3.22", "VIEWS")))
})

# ---------------------------------------------------------------------------
# Publish gate
# ---------------------------------------------------------------------------

.steady_manifest <- function() {
  list(source = list(
    views_fingerprint     = .FIXTURE_FP,
    releases_fingerprint  = .FIXTURE_RELEASES_FP,
    biocviews_fingerprint = .FIXTURE_BIOCVIEWS_FP_3_23,
    views_sha256          = .FIXTURE_VIEWS_SHA,
    builds_fingerprint    = "",
    schema                = BIOC_METADATA_SCHEMA))
}

test_that("a PackageStatus flip with no version change republishes", {
  tmp <- withr::local_tempdir()
  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = .steady_manifest())
  io$fetch_views <- function(cat) {
    switch(cat,
      software = sub("hasNEWS: TRUE", "hasNEWS: TRUE\nPackageStatus: Deprecated",
                     FIXTURE_VIEWS_SOFTWARE, fixed = TRUE),
      annotation = FIXTURE_VIEWS_ANNOTATION, "")
  }
  res <- suppressMessages(run_update(io, file.path(tmp, "out"), force_full = FALSE))
  expect_true(res$manifest$changed)
  expect_equal(res$manifest$source$views_fingerprint, .FIXTURE_FP)
})

test_that("a newly applied build report republishes", {
  tmp <- withr::local_tempdir()
  io <- make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = .steady_manifest(),
                     build_files = all_build_files())
  res <- suppressMessages(run_update(io, file.path(tmp, "out"), force_full = FALSE))
  expect_true(res$manifest$changed)
  expect_match(res$manifest$source$builds_fingerprint, "3.23:bioc:2026-09-28T17:40:00Z", fixed = TRUE)
})

test_that("the same VIEWS and reports again leave the gate closed", {
  tmp <- withr::local_tempdir()
  out1 <- file.path(tmp, "one"); out2 <- file.path(tmp, "two")
  first <- suppressMessages(run_update(
    make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = .steady_manifest(),
                 build_files = all_build_files()), out1, force_full = FALSE))
  res <- suppressMessages(run_update(
    make_stub_io(prev_pkgs = .bv_prev_pkgs, prev_manifest = list(source = first$manifest$source),
                 build_files = all_build_files(), prev_state = state_of(out1)),
    out2, force_full = FALSE))
  expect_false(res$manifest$changed)
})

test_that("builds_fingerprint keeps the newest applied report per version and repo", {
  r <- data.frame(bioc_version = c("3.23", "3.23", "3.24", "3.23"),
                  repo = c("bioc", "bioc", "bioc", "workflows"),
                  report_at = c("2026-09-27T17:40:00Z", "2026-09-28T17:40:00Z",
                                "2026-09-28T17:45:00Z", "2026-09-29T16:45:00Z"),
                  outcome = c("applied", "applied", "skipped_floor", "applied"),
                  stringsAsFactors = FALSE)
  expect_equal(builds_fingerprint(r),
               "3.23:bioc:2026-09-28T17:40:00Z,3.23:workflows:2026-09-29T16:45:00Z")
  expect_equal(builds_fingerprint(r[0, ]), "")
})

test_that("views_sha256 ignores the attributes fetch_views adds", {
  plain <- list(software = "Package: a", annotation = "")
  marked <- list(software = structure("Package: a", last_modified = "x", raw = "Package: a\n"),
                 annotation = "")
  expect_equal(views_sha256(marked), views_sha256(plain))
  expect_false(views_sha256(list(software = "Package: b", annotation = "")) ==
                 views_sha256(plain))
})


# ---------------------------------------------------------------------------
# Status file and exit status
# ---------------------------------------------------------------------------

read_status <- function(out) jsonlite::read_json(file.path(out, "status.json"))

test_that("a failed build stream still writes catalog_ok true, builds_ok false, and exits 0", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  files <- all_build_files()
  files[["devel/workflows/BUILD_STATUS_DB.txt"]] <- NULL
  code <- suppressMessages(main(out, io = make_stub_io(build_files = files), live_floor = 1L))
  expect_identical(code, 0L)
  st <- read_status(out)
  expect_true(st$catalog_ok)
  expect_false(st$builds_ok)
  wf <- Filter(function(x) x$branch == "devel" && x$repo == "workflows", st$streams)[[1]]
  expect_equal(wf$outcome, "fetch_failed")
  expect_true("3.23/views/software/VIEWS" %in% unlist(st$archive_files))
})

test_that("every stream read gives builds_ok true and exit 0", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  code <- suppressMessages(main(out, io = make_stub_io(build_files = all_build_files()),
                                live_floor = 1L))
  expect_identical(code, 0L)
  expect_true(read_status(out)$builds_ok)
})

test_that("a failed names gate writes catalog_ok false and exits 1", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  code <- suppressMessages(main(out, io = make_stub_io(build_files = all_build_files())))
  expect_identical(code, 1L)
  expect_false(read_status(out)$catalog_ok)
})

test_that("a failed VIEWS fetch leaves no status file, even a stale one", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  dir.create(out)
  writeLines('{"catalog_ok": true, "builds_ok": true}', file.path(out, "status.json"))
  io <- make_stub_io(build_files = all_build_files())
  io$fetch_views <- function(cat) stop("HTTP 504 for VIEWS")
  expect_error(main(out, io = io, live_floor = 1L), "HTTP 504")
  expect_false(file.exists(file.path(out, "status.json")))
})

test_that("an unreadable prior catalog leaves no status file", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")
  dir.create(out)
  writeLines('{"catalog_ok": true, "builds_ok": true}', file.path(out, "status.json"))
  io <- make_stub_io(build_files = all_build_files())
  io$prev_catalog <- function() stop("Prior bioconductor-metadata.db download failed")
  expect_error(main(out, io = io, live_floor = 1L), "download failed")
  expect_false(file.exists(file.path(out, "status.json")))
})
