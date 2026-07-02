#!/usr/bin/env Rscript
# scripts/update.R: Bioconductor metadata catalog builder.
#
# Fetches current VIEWS for each package category, crawls git branches to
# determine package lineage (first/last release, current, devel), and writes a
# SQLite catalog plus a JSON manifest to out_dir.
#
# run_update(io, out_dir, force_full) takes an injectable io for offline testing.
# default_io() supplies the real network fetchers.

options(timeout = 600)

suppressPackageStartupMessages({
  library(RSQLite)
  library(jsonlite)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("parse_views", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

with_retry <- function(expr, tries = 3L, wait = 3) {
  # a failed force() leaves the promise un-cached, so the loop re-evaluates expr.
  # Retry attempts are wrapped in suppressWarnings() to silence the
  # "restarting interrupted promise evaluation" diagnostic that R emits when a
  # previously-failed lazy promise is re-evaluated.
  for (i in seq_len(tries)) {
    val <- tryCatch(
      if (i == 1L) force(expr) else suppressWarnings(force(expr)),
      error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < tries) Sys.sleep(wait * i)
  }
  stop(val)
}

# ---------------------------------------------------------------------------
# run_update
# ---------------------------------------------------------------------------

run_update <- function(io, out_dir, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Release dates and current release (max by release_to_numeric)
  config_text     <- io$config_yaml()
  dates           <- parse_release_dates(config_text)
  r_vers          <- parse_r_ver_for_bioc(config_text)
  nums            <- vapply(names(dates), release_to_numeric, numeric(1))
  current_release <- names(dates)[which.max(nums)]

  releases_df          <- bioc_releases_from_dates(dates, r_vers)
  releases_fingerprint <- paste0(releases_df$version, ":", releases_df$r_version, collapse = ",")

  # 2. Fetch VIEWS metadata for every category
  views_parts <- lapply(names(VIEWS_URLS), function(cat) {
    parse_views(io$fetch_views(cat), cat)
  })
  views_df <- do.call(rbind, views_parts)
  rownames(views_df) <- NULL
  views_names <- views_df$name

  # Fingerprint of the current VIEWS state: sorted "name:version" pairs joined
  # by commas. Dependency-free and stable; used for change detection below.
  views_fingerprint <- paste(
    sort(paste0(views_df$name, ":", views_df$version)),
    collapse = ","
  )

  # 3. Prior catalog (empty list on cold start or force_full)
  prev      <- io$prev_catalog()
  prev_pkgs <- prev$packages
  has_prev  <- !is.null(prev_pkgs) && nrow(prev_pkgs) > 0

  # 4. Determine which packages to (re)crawl
  if (force_full || !has_prev) {
    crawl_set <- io$list_repos()
  } else {
    new_in_views       <- setdiff(views_names, prev_pkgs$name)
    prev_current       <- prev_pkgs$name[prev_pkgs$in_current == 1L]
    removed_from_views <- setdiff(prev_current, views_names)
    # Re-crawl current packages whose first_release was not established yet
    # (e.g., git ls-remote failed during a cold bootstrap run).
    # Restrict to software/workflows only: annotation and experiment data packages
    # have no RELEASE branches on github.com/bioc, so re-crawling them is fruitless.
    null_first <- if (all(c("first_release", "category") %in% names(prev_pkgs))) {
      prev_pkgs$name[
        (is.na(prev_pkgs$first_release) | prev_pkgs$first_release == "") &
        prev_pkgs$name %in% views_names &
        prev_pkgs$category %in% c("software", "workflows")
      ]
    } else {
      character(0L)
    }
    crawl_set <- union(union(new_in_views, removed_from_views), null_first)
  }

  # 5. Crawl each package in the set (per-package failures are caught and skipped)
  lineage_list <- list()
  authors_rows <- list()
  desc_meta    <- list()  # DESCRIPTION-derived metadata for packages absent from views

  for (pkg in crawl_set) {
    tryCatch({
      br <- io$ls_remote(pkg)
      L  <- package_lineage(br, current_release, dates)
      lineage_list[[pkg]] <- L

      # Pick the branch whose DESCRIPTION to fetch
      branch <- if (L$in_current) {
        paste0("RELEASE_", gsub(".", "_", current_release, fixed = TRUE))
      } else if (!is.na(L$last_release)) {
        paste0("RELEASE_", gsub(".", "_", L$last_release, fixed = TRUE))
      } else {
        "devel"
      }

      desc_text <- io$fetch_description(pkg, branch)
      m <- tryCatch(read.dcf(textConnection(desc_text)), error = function(e) NULL)

      # Extract Authors@R and parse to author rows
      ar_text <- if (!is.null(m) && "Authors@R" %in% colnames(m))
        m[1L, "Authors@R"] else NA_character_
      auth_df <- parse_authors_at_r(ar_text, pkg)
      if (nrow(auth_df) > 0L) authors_rows[[pkg]] <- auth_df

      # For packages absent from views, keep DESCRIPTION fields for the catalog row
      if (!(pkg %in% views_names) && !is.null(m)) {
        g <- function(f) if (f %in% colnames(m)) as.character(m[1L, f]) else NA_character_
        desc_meta[[pkg]] <- list(
          version = g("Version"), title       = g("Title"),
          description = g("Description"), license = g("License"),
          depends = g("Depends"), imports     = g("Imports"),
          suggests = g("Suggests"), biocviews = g("biocViews")
        )
      }

      Sys.sleep(0.05)  # throttle between per-package network calls
    }, error = function(e) {
      message("Skipping ", pkg, ": ", conditionMessage(e))
    })
  }

  # 6. Assemble packages_df ---------------------------------------------------

  packages_rows <- list()

  # Current packages: metadata from views + lineage from crawl or prev.
  # Skip any package whose crawl was attempted but failed with no prior fallback.
  for (i in seq_len(nrow(views_df))) {
    row <- views_df[i, , drop = FALSE]
    pkg <- row$name

    in_crawl_set <- pkg %in% crawl_set
    was_crawled  <- pkg %in% names(lineage_list)
    in_prev      <- has_prev && pkg %in% prev_pkgs$name

    if (in_crawl_set && !was_crawled && !in_prev) next  # crawl failed, no fallback

    if (was_crawled) {
      L <- lineage_list[[pkg]]
    } else if (in_prev) {
      idx <- which(prev_pkgs$name == pkg)[1L]
      pr  <- prev_pkgs[idx, ]
      L <- list(
        first_release      = pr$first_release,
        first_release_date = pr$first_release_date,
        last_release       = current_release,
        last_release_date  = unname(dates[current_release]) %||% NA_character_,
        in_current         = TRUE,
        in_devel           = as.logical(pr$in_devel)
      )
    } else {
      L <- list(
        first_release = NA_character_, first_release_date = NA_character_,
        last_release  = NA_character_, last_release_date  = NA_character_,
        in_current    = TRUE,           in_devel           = FALSE
      )
    }

    packages_rows[[pkg]] <- data.frame(
      name               = pkg,
      name_lower         = tolower(pkg),
      category           = row$category,
      version            = row$version,
      title              = row$title,
      description        = row$description,
      maintainer         = row$maintainer,
      maintainer_email   = row$maintainer_email,
      license            = row$license,
      depends            = row$depends,
      imports            = row$imports,
      suggests           = row$suggests,
      biocviews          = row$biocviews,
      git_url            = row$git_url,
      first_release      = L$first_release %||% NA_character_,
      first_release_date = L$first_release_date %||% NA_character_,
      last_release       = L$last_release %||% NA_character_,
      last_release_date  = L$last_release_date %||% NA_character_,
      in_current         = 1L,
      in_devel           = as.integer(isTRUE(L$in_devel)),
      updated_at         = iso(Sys.time()),
      stringsAsFactors   = FALSE
    )
  }

  # Removed packages: crawled but not in views
  for (pkg in names(lineage_list)) {
    if (pkg %in% views_names) next  # already handled as current
    L <- lineage_list[[pkg]]

    if (has_prev && pkg %in% prev_pkgs$name) {
      idx <- which(prev_pkgs$name == pkg)[1L]
      pr  <- prev_pkgs[idx, ]
      packages_rows[[pkg]] <- data.frame(
        name               = pkg,
        name_lower         = tolower(pkg),
        category           = pr$category,
        version            = pr$version,
        title              = pr$title,
        description        = pr$description,
        maintainer         = pr$maintainer,
        maintainer_email   = pr$maintainer_email,
        license            = pr$license,
        depends            = pr$depends,
        imports            = pr$imports,
        suggests           = pr$suggests,
        biocviews          = pr$biocviews,
        git_url            = pr$git_url,
        first_release      = L$first_release %||% pr$first_release,
        first_release_date = L$first_release_date %||% pr$first_release_date,
        last_release       = L$last_release %||% NA_character_,
        last_release_date  = L$last_release_date %||% NA_character_,
        in_current         = 0L,
        in_devel           = as.integer(isTRUE(L$in_devel)),
        updated_at         = iso(Sys.time()),
        stringsAsFactors   = FALSE
      )
    } else {
      # No prior data: assemble from DESCRIPTION fields (force_full with no prev)
      dm <- desc_meta[[pkg]]; if (is.null(dm)) dm <- list()
      packages_rows[[pkg]] <- data.frame(
        name               = pkg,
        name_lower         = tolower(pkg),
        category           = "",  # unknown: absent from views and no prior data
        version            = dm$version  %||% NA_character_,
        title              = dm$title    %||% NA_character_,
        description        = dm$description %||% NA_character_,
        maintainer         = NA_character_,
        maintainer_email   = NA_character_,
        license            = dm$license  %||% NA_character_,
        depends            = dm$depends  %||% NA_character_,
        imports            = dm$imports  %||% NA_character_,
        suggests           = dm$suggests %||% NA_character_,
        biocviews          = dm$biocviews %||% NA_character_,
        git_url            = NA_character_,
        first_release      = L$first_release %||% NA_character_,
        first_release_date = L$first_release_date %||% NA_character_,
        last_release       = L$last_release %||% NA_character_,
        last_release_date  = L$last_release_date %||% NA_character_,
        in_current         = 0L,
        in_devel           = as.integer(isTRUE(L$in_devel)),
        updated_at         = iso(Sys.time()),
        stringsAsFactors   = FALSE
      )
    }
  }

  # In incremental mode, prev current packages not in views and not crawled
  # (e.g., removed in a prior cycle and still not in views) keep their removed state
  if (has_prev) {
    prev_current_pkgs <- prev_pkgs$name[prev_pkgs$in_current == 1L]
    for (pkg in prev_current_pkgs) {
      if (pkg %in% views_names)          next  # in views, handled above
      if (pkg %in% names(packages_rows)) next  # crawled and handled
      idx <- which(prev_pkgs$name == pkg)[1L]
      pr  <- prev_pkgs[idx, ]
      packages_rows[[pkg]] <- data.frame(
        name               = pkg,
        name_lower         = tolower(pkg),
        category           = pr$category,
        version            = pr$version,
        title              = pr$title,
        description        = pr$description,
        maintainer         = pr$maintainer,
        maintainer_email   = pr$maintainer_email,
        license            = pr$license,
        depends            = pr$depends,
        imports            = pr$imports,
        suggests           = pr$suggests,
        biocviews          = pr$biocviews,
        git_url            = pr$git_url,
        first_release      = pr$first_release,
        first_release_date = pr$first_release_date,
        last_release       = pr$last_release,
        last_release_date  = pr$last_release_date,
        in_current         = 0L,
        in_devel           = as.integer(pr$in_devel),
        updated_at         = iso(Sys.time()),
        stringsAsFactors   = FALSE
      )
    }
    # Carry forward prev packages with in_current=0 that are not in views
    prev_removed_pkgs <- prev_pkgs$name[prev_pkgs$in_current == 0L]
    for (pkg in prev_removed_pkgs) {
      if (pkg %in% names(packages_rows)) next
      idx <- which(prev_pkgs$name == pkg)[1L]
      pr  <- prev_pkgs[idx, ]
      packages_rows[[pkg]] <- data.frame(
        name               = pkg,
        name_lower         = tolower(pkg),
        category           = pr$category,
        version            = pr$version,
        title              = pr$title,
        description        = pr$description,
        maintainer         = pr$maintainer,
        maintainer_email   = pr$maintainer_email,
        license            = pr$license,
        depends            = pr$depends,
        imports            = pr$imports,
        suggests           = pr$suggests,
        biocviews          = pr$biocviews,
        git_url            = pr$git_url,
        first_release      = pr$first_release,
        first_release_date = pr$first_release_date,
        last_release       = pr$last_release,
        last_release_date  = pr$last_release_date,
        in_current         = 0L,
        in_devel           = as.integer(pr$in_devel),
        updated_at         = pr$updated_at,
        stringsAsFactors   = FALSE
      )
    }
  }

  # Combine rows
  empty_pkgs <- data.frame(
    name = character(0), name_lower = character(0), category = character(0),
    version = character(0), title = character(0), description = character(0),
    maintainer = character(0), maintainer_email = character(0), license = character(0),
    depends = character(0), imports = character(0), suggests = character(0),
    biocviews = character(0), git_url = character(0),
    first_release = character(0), first_release_date = character(0),
    last_release = character(0), last_release_date = character(0),
    in_current = integer(0), in_devel = integer(0), updated_at = character(0),
    stringsAsFactors = FALSE
  )
  packages_df <- if (length(packages_rows) > 0L) {
    out_df <- do.call(rbind, packages_rows)
    rownames(out_df) <- NULL
    out_df
  } else {
    empty_pkgs
  }

  empty_auths <- data.frame(
    package = character(0), given = character(0), family = character(0),
    email = character(0), role = character(0), orcid = character(0),
    stringsAsFactors = FALSE
  )
  authors_df <- if (length(authors_rows) > 0L) {
    out_df <- do.call(rbind, authors_rows)
    rownames(out_df) <- NULL
    out_df
  } else {
    empty_auths
  }

  # Carry forward authors for packages that are in the final catalog but were
  # NOT re-crawled this run. Key off names(lineage_list) (built in the crawl
  # loop) so that a re-crawled package whose Authors@R yielded zero rows is
  # still treated as authoritative -- no stale rows come back from prev for it.
  if (has_prev && !is.null(prev$authors) && nrow(prev$authors) > 0L) {
    recrawled  <- names(lineage_list)
    keep       <- setdiff(packages_df$name, recrawled)
    carry      <- prev$authors[prev$authors$package %in% keep, , drop = FALSE]
    if (nrow(carry) > 0L) {
      carry    <- carry[, c("package", "given", "family", "email", "role", "orcid"),
                        drop = FALSE]
      authors_df <- rbind(authors_df, carry)
    }
  }

  # 7. Export catalog and manifest
  db_path <- file.path(out_dir, "bioconductor-metadata.db")
  export_catalog(db_path, packages_df, authors_df, releases_df)

  manifest_changed <- isTRUE(force_full) || length(crawl_set) > 0L ||
    (prev$manifest$source$views_fingerprint    %||% "") != views_fingerprint ||
    (prev$manifest$source$releases_fingerprint %||% "") != releases_fingerprint

  manifest <- list(
    release         = paste0("v", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at    = iso(Sys.time()),
    current_release = current_release,
    n_packages      = nrow(packages_df),
    n_current       = sum(packages_df$in_current == 1L),
    n_authors       = nrow(authors_df),
    changed              = manifest_changed,
    n_releases           = nrow(releases_df),
    source               = list(
      views_fingerprint    = views_fingerprint,
      releases_fingerprint = releases_fingerprint
    )
  )
  write_manifest(file.path(out_dir, "manifest.json"), manifest)

  list(changed = manifest_changed, manifest = manifest)
}

# ---------------------------------------------------------------------------
# default_io: real network fetchers
# ---------------------------------------------------------------------------

default_io <- function() {
  list(
    config_yaml = function() {
      with_retry(
        paste(readLines(url(CONFIG_YAML_URL), warn = FALSE), collapse = "\n")
      )
    },

    fetch_views = function(cat) {
      with_retry(
        paste(readLines(url(VIEWS_URLS[[cat]]), warn = FALSE), collapse = "\n")
      )
    },

    list_repos = function() {
      out <- suppressWarnings(system2(
        "gh",
        c("api", "--paginate", sprintf("orgs/%s/repos?per_page=100", BIOC_ORG),
          "--jq", ".[].name"),
        stdout = TRUE, stderr = FALSE))
      sort(out[nzchar(trimws(out))])
    },

    ls_remote = function(pkg) {
      raw <- suppressWarnings(system2(
        "git",
        c("ls-remote", "--heads", paste0(BIOC_GIT_BASE, "/", pkg)),
        stdout = TRUE, stderr = FALSE))
      refs <- grep("\trefs/heads/", raw, value = TRUE)
      sub(".*\trefs/heads/", "", refs)
    },

    fetch_description = function(pkg, branch) {
      url_str <- paste(BIOC_RAW_BASE, pkg, branch, "DESCRIPTION", sep = "/")
      with_retry(
        paste(readLines(url(url_str), warn = FALSE), collapse = "\n")
      )
    },

    prev_catalog = function() {
      tmp_dir <- tempfile()
      dir.create(tmp_dir, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      # Download prior manifest for fingerprint comparison; non-fatal on absence.
      suppressWarnings(system2(
        "gh",
        c("release", "download", "current",
          "--repo", PUBLISH_REPO,
          "--pattern", "manifest.json",
          "--dir", tmp_dir, "--clobber"),
        stdout = FALSE, stderr = FALSE))
      prev_manifest <- tryCatch({
        mf <- file.path(tmp_dir, "manifest.json")
        if (file.exists(mf)) jsonlite::read_json(mf) else list()
      }, error = function(e) list())

      st <- suppressWarnings(system2(
        "gh",
        c("release", "download", "current",
          "--repo", PUBLISH_REPO,
          "--pattern", "bioconductor-metadata.db",
          "--dir", tmp_dir, "--clobber"),
        stdout = FALSE, stderr = FALSE))
      db_path <- file.path(tmp_dir, "bioconductor-metadata.db")
      if (!identical(as.integer(st), 0L) || !file.exists(db_path)) {
        return(list(manifest = prev_manifest))
      }
      con  <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)
      pkgs  <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_packages")
      auths <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_authors")
      list(packages = pkgs, authors = auths, manifest = prev_manifest)
    }
  )
}

# ---------------------------------------------------------------------------
# Entry point when run as a standalone script
# ---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[1L] else "out"
  force_full <- "--bootstrap" %in% args
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  run_update(default_io(), out_dir, force_full)
}
