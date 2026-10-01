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
if (!exists("parse_build_status_db", mode = "function")) {
  source(file.path(.script_dir, "builds.R"))
}
if (!exists("views_state_rows", mode = "function")) {
  source(file.path(.script_dir, "views_history.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# GET a URL: status, body as UTF-8 text, and Last-Modified in UTC.
http_get <- function(url) {
  res <- curl::curl_fetch_memory(
    url, handle = curl::new_handle(followlocation = TRUE, timeout = 600))
  hdr <- curl::parse_headers_list(res$headers)
  body <- rawToChar(res$content)
  Encoding(body) <- "UTF-8"
  list(status = as.integer(res$status_code), body = body,
       last_modified = http_date_to_iso(hdr[["last-modified"]]))
}

with_retry <- function(expr, waits = RETRY_WAITS_S, sleep = Sys.sleep,
                       rand = function() stats::runif(1, 1, 1.25)) {
  # a failed force() leaves the promise un-cached, so the loop re-evaluates expr.
  # Retry attempts are wrapped in suppressWarnings() to silence the
  # "restarting interrupted promise evaluation" diagnostic that R emits when a
  # previously-failed lazy promise is re-evaluated. The first attempt is left
  # unsuppressed so the real diagnostic, HTTP status included, still reaches the
  # log. One more attempt is made than there are waits, and the original error is
  # the one re-raised. sleep and rand are injected so the suite can assert the
  # schedule without waiting.
  n <- length(waits) + 1L
  for (i in seq_len(n)) {
    val <- tryCatch(
      if (i == 1L) force(expr) else suppressWarnings(force(expr)),
      error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < n) sleep(waits[[i]] * rand())
  }
  stop(val)
}

# ---------------------------------------------------------------------------
# run_update
# ---------------------------------------------------------------------------

run_update <- function(io, out_dir, force_full = FALSE, live_floor = BIOC_LIVE_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # A run that stops early leaves no status file for the workflow to trust, and
  # upstream files are this run's only.
  unlink(file.path(out_dir, c("status.json", "upstream", "archive-message.txt")),
         recursive = TRUE)

  # 1. Release dates and current release (max by release_to_numeric)
  config_text     <- io$config_yaml()
  dates           <- parse_release_dates(config_text)
  r_vers          <- parse_r_ver_for_bioc(config_text)
  nums            <- vapply(names(dates), release_to_numeric, numeric(1))
  current_release <- names(dates)[which.max(nums)]

  releases_df          <- bioc_releases_from_dates(dates, r_vers)
  releases_fingerprint <- paste0(releases_df$version, ":", releases_df$r_version, collapse = ",")

  # 2. Fetch VIEWS metadata for every category, keeping the text for the vignette rows
  views_texts <- setNames(lapply(names(VIEWS_URLS), function(cat) io$fetch_views(cat)),
                          names(VIEWS_URLS))
  views_parts <- lapply(names(views_texts), function(cat) {
    parse_views(views_texts[[cat]], cat)
  })
  views_df <- do.call(rbind, views_parts)
  rownames(views_df) <- NULL
  views_names <- views_df$name
  vignettes_df <- build_bioc_vignettes(views_texts, current_release)

  # Fingerprint of the current VIEWS state: sorted "name:version" pairs joined
  # by commas. Dependency-free and stable; used for change detection below.
  views_fingerprint <- paste(
    sort(paste0(views_df$name, ":", views_df$version)),
    collapse = ","
  )

  # 3. Prior catalog. An unreadable prior stops the run so the catch-up tries
  # again; --bootstrap is the owner's way past it and starts the history over.
  prev <- tryCatch(io$prev_catalog(), error = function(e) {
    if (!isTRUE(force_full)) stop(e)
    message("Prior catalog unavailable (", conditionMessage(e),
            "); --bootstrap starts from scratch")
    list(manifest = list(), names_all = empty_bioc_names_all(), cold_start = TRUE)
  })
  prev_pkgs <- prev$packages
  has_prev  <- !is.null(prev_pkgs) && nrow(prev_pkgs) > 0

  # 3a. Build reports. A failed stream keeps its prior rows and never stops
  # the catalog.
  run_at <- iso(Sys.time())
  builds <- read_build_state(io, prev, parse_branch_versions(config_text), run_at)

  # 3b. biocViews vocabulary per release
  empty_edges <- data.frame(
    release = character(0), parent = character(0), child = character(0),
    stringsAsFactors = FALSE
  )
  prev_edges <- prev$view_edges %||% empty_edges
  captured   <- unique(prev_edges$release)

  edges_parts <- list()
  for (v in releases_df$version) {
    branch <- paste0("RELEASE_", gsub(".", "_", v, fixed = TRUE))
    if (v == current_release || !(v %in% captured)) {
      fetched <- tryCatch({
        dot <- io$fetch_biocviews_dot(branch)
        if (!is.null(dot) && nzchar(dot)) {
          e <- parse_biocviews_dot(dot)
          if (nrow(e) > 0L) {
            e$release <- v
            e[, c("release", "parent", "child"), drop = FALSE]
          } else {
            NULL
          }
        } else {
          NULL
        }
      }, error = function(e_err) NULL)

      if (!is.null(fetched)) {
        edges_parts[[v]] <- fetched
      } else if (v %in% captured) {
        edges_parts[[v]] <- prev_edges[prev_edges$release == v, , drop = FALSE]
      }
    } else {
      edges_parts[[v]] <- prev_edges[prev_edges$release == v, , drop = FALSE]
    }
  }

  view_edges_df <- if (length(edges_parts) > 0L) {
    out_df <- do.call(rbind, edges_parts)
    rownames(out_df) <- NULL
    out_df
  } else {
    empty_edges
  }

  cur_edges <- view_edges_df[view_edges_df$release == current_release, , drop = FALSE]
  biocviews_fingerprint <- if (nrow(cur_edges) > 0L) {
    paste0(sort(paste0(cur_edges$parent, "->", cur_edges$child)), collapse = ",")
  } else {
    ""
  }

  # 4. Determine which packages to (re)crawl
  migrate_authors <- has_prev && authors_need_migration(prev$authors)
  if (migrate_authors) {
    message("Prior bioc_authors lacks ror_id or comment; crawling every repository once")
  }
  if (force_full || !has_prev || migrate_authors) {
    crawl_set <- io$list_repos()
    # A short listing would publish the new columns with most packages unread,
    # and no later run would crawl them again.
    if (migrate_authors) {
      cover <- views_code_coverage(crawl_set, views_df)
      if (cover < BIOC_MIGRATION_LISTING_FLOOR) {
        stop(sprintf(paste("Repository listing holds %.1f%% of current software and",
                           "workflows packages; not starting the one-time author crawl"),
                     100 * cover))
      }
    }
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
    # A new version can carry a new Authors@R, so its DESCRIPTION is read again.
    version_bumped <- version_bumped_packages(prev_pkgs, views_df)
    if (length(version_bumped) > 0L) {
      message("Re-reading DESCRIPTION for ", length(version_bumped),
              " packages whose VIEWS version changed")
    }
    crawl_set <- union(union(union(new_in_views, removed_from_views), null_first),
                       version_bumped)
  }

  # 5. Crawl each package in the set (per-package failures are caught and skipped)
  lineage_list <- list()
  desc_read    <- character(0)  # packages whose DESCRIPTION was fetched this run
  authors_rows <- list()
  desc_meta    <- list()  # DESCRIPTION-derived metadata for packages absent from views
  crawl_start  <- Sys.time()

  for (pkg in crawl_set) {
    tryCatch({
      br <- io$ls_remote(pkg)
      # ls_remote returns no branches when git fails, so a known package keeps
      # its prior row instead of NA lineage and devel's authors.
      if (length(br) == 0L && has_prev && pkg %in% prev_pkgs$name) {
        stop("empty branch listing; keeping the prior catalog row")
      }
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
      desc_read <- c(desc_read, pkg)
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
  # The full crawl must stay inside the job's 300-minute timeout.
  message(sprintf("Crawled %d packages in %.1f min; DESCRIPTION read for %d",
                  length(crawl_set),
                  as.numeric(difftime(Sys.time(), crawl_start, units = "mins")),
                  length(desc_read)))

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
      has_news           = row$has_news,
      views_has_readme   = row$views_has_readme,
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
        has_news           = NA_integer_,
        views_has_readme   = NA_integer_,
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
        has_news           = NA_integer_,
        views_has_readme   = NA_integer_,
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
        has_news           = NA_integer_,
        views_has_readme   = NA_integer_,
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
        has_news           = NA_integer_,
        views_has_readme   = NA_integer_,
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
    has_news = integer(0), views_has_readme = integer(0),
    stringsAsFactors = FALSE
  )
  packages_df <- if (length(packages_rows) > 0L) {
    out_df <- do.call(rbind, packages_rows)
    rownames(out_df) <- NULL
    out_df
  } else {
    empty_pkgs
  }
  packages_df <- attach_views_extras(packages_df, views_df)

  empty_auths <- empty_bioc_authors()
  authors_df <- if (length(authors_rows) > 0L) {
    out_df <- do.call(rbind, authors_rows)
    rownames(out_df) <- NULL
    out_df
  } else {
    empty_auths
  }

  # Carry forward authors for packages in the final catalog whose DESCRIPTION
  # was not read this run. A package whose DESCRIPTION was read but yielded no
  # Authors@R rows is authoritative; one whose fetch failed keeps its prior rows.
  if (has_prev && !is.null(prev$authors) && nrow(prev$authors) > 0L) {
    carry      <- carry_forward_authors(prev$authors,
                                        setdiff(packages_df$name, desc_read))
    if (nrow(carry) > 0L) authors_df <- rbind(authors_df, carry)
  }

  # 7. Export catalog and manifest
  db_path <- file.path(out_dir, "bioconductor-metadata.db")

  n_live_bioc   <- sum(packages_df$in_current == 1L)
  names_gate_ok <- bioc_names_size_ok(n_live_bioc, floor = live_floor)
  names_all_df  <- if (names_gate_ok) {
    build_bioc_names_all(packages_df)
  } else if (!is.null(prev$names_all) && nrow(prev$names_all) > 0L) {
    message("bioc names size gate failed (live=", n_live_bioc,
            "); reusing the prior bioc_names_all")
    prev$names_all
  } else {
    message("bioc names size gate failed (live=", n_live_bioc,
            ") and no prior names table; building from the current catalog")
    build_bioc_names_all(packages_df)
  }
  n_names <- nrow(names_all_df)

  # 7a. VIEWS state episodes. A failed names gate skips them all; a category
  # that parses to nothing while the history holds it is a failed read.
  views_times <- vapply(names(views_texts), function(cat) {
    attr(views_texts[[cat]], "last_modified") %||% run_at
  }, character(1))
  views_prior <- conform_frame(prev$views_history, empty_views_history())
  views_now <- do.call(rbind, lapply(names(views_texts), function(cat) {
    views_state_rows(views_texts[[cat]], cat)
  }))
  views_apply <- if (isTRUE(names_gate_ok)) names(views_texts) else character(0)
  views_apply <- setdiff(views_apply, setdiff(unique(views_prior$category),
                                              unique(views_now$category)))
  views_hist <- apply_views_state(views_prior, views_now, views_times, views_apply,
                                  current_release)

  export_catalog(db_path, packages_df, authors_df, releases_df, view_edges_df,
                 names_all_df = names_all_df, vignettes_df = vignettes_df,
                 build_reports_df = builds$reports,
                 build_status_df = builds$history,
                 views_history_df = views_hist$history)

  # Integrity / completeness core for the primary published db. export_catalog
  # closes its own connection before returning, so the file on disk is
  # finalized here; db_integrity_core enumerates tables/counts on a fresh
  # connection, disconnects, and only then hashes the closed file's bytes.
  #
  # complete = the db holds the full, non-partial catalog (freshness is tracked
  # separately via generated_at and the source fingerprint). Two genuine
  # partial/bootstrap states make it FALSE:
  #   1. A truncated VIEWS fetch: names_gate_ok is FALSE when the live count
  #      falls below the floor, i.e. the live catalog itself came up short.
  #   2. Unresolved cold-bootstrap lineage: current software/workflows packages
  #      whose first_release could not be established yet (git ls-remote
  #      failures the pipeline keeps re-crawling until resolved). This mirrors
  #      the null_first re-crawl set; lineage_remaining == 0 means the lineage
  #      is fully built.
  lineage_remaining <- sum(
    packages_df$in_current == 1L &
      packages_df$category %in% c("software", "workflows") &
      (is.na(packages_df$first_release) | packages_df$first_release == "")
  )
  db_complete <- isTRUE(names_gate_ok) && lineage_remaining == 0L
  db_core <- db_integrity_core(db_path, complete = db_complete)

  # The VIEWS bytes and the applied reports catch a Deprecated flip, a new
  # binary or a new build result, none of which moves a version.
  views_sha <- views_sha256(views_texts)
  builds_fp <- builds_fingerprint(builds$reports)
  manifest_changed <- isTRUE(force_full) || length(crawl_set) > 0L ||
    (prev$manifest$source$views_sha256          %||% "") != views_sha ||
    (prev$manifest$source$builds_fingerprint    %||% "") != builds_fp ||
    (prev$manifest$source$views_fingerprint     %||% "") != views_fingerprint ||
    (prev$manifest$source$releases_fingerprint  %||% "") != releases_fingerprint ||
    (prev$manifest$source$biocviews_fingerprint %||% "") != biocviews_fingerprint ||
    !identical(as.integer(prev$manifest$source$schema %||% NA_integer_), BIOC_METADATA_SCHEMA)

  manifest <- list(
    release         = paste0("v", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at    = iso(Sys.time()),
    current_release = current_release,
    n_packages      = nrow(packages_df),
    n_current       = sum(packages_df$in_current == 1L),
    n_authors       = nrow(authors_df),
    n_vignettes     = nrow(vignettes_df),
    n_names         = n_names,
    names_gate_ok   = names_gate_ok,
    cold_start      = isTRUE(prev$cold_start),
    changed              = manifest_changed,
    n_releases           = nrow(releases_df),
    builds_ok            = builds$ok,
    builds               = builds$summary,
    builds_retired       = builds$retired,
    views_history        = list(new = views_hist$counts[["new"]],
                                extended = views_hist$counts[["extended"]],
                                closed = views_hist$counts[["closed"]],
                                applied = I(setdiff(views_apply, views_hist$skipped)),
                                skipped_stale = I(views_hist$skipped)),
    source               = list(
      views_fingerprint     = views_fingerprint,
      releases_fingerprint  = releases_fingerprint,
      biocviews_fingerprint = biocviews_fingerprint,
      views_sha256          = views_sha,
      builds_fingerprint    = builds_fp,
      n_view_edges          = nrow(view_edges_df),
      schema                = BIOC_METADATA_SCHEMA
    )
  )
  # Attach the integrity/completeness core as TOP-LEVEL manifest fields
  # (db_filename, db_bytes, db_sha256, tables, complete) alongside the existing
  # ones, so a downstream merge can content-verify the db it pulls.
  manifest <- c(manifest, db_core)
  write_manifest(file.path(out_dir, "manifest.json"), manifest)

  # 8. Upstream files for the archive branch.
  views_cats <- setdiff(views_apply, views_hist$skipped)
  views_cats <- views_cats[vapply(views_cats, function(cat) {
    nzchar(trimws(as.character(views_texts[[cat]])))
  }, logical(1))]
  archive <- upstream_files(views_texts, views_times, views_cats, current_release, builds)
  archive_files <- write_upstream_files(file.path(out_dir, "upstream"), archive)
  if (length(archive_files) > 0L) {
    writeLines(archive_message(archive, run_at), file.path(out_dir, "archive-message.txt"))
  }

  # 9. The status file, last, once the db and manifest are closed. The workflow
  # publishes and archives only when it says catalog_ok.
  status <- list(
    catalog_ok    = isTRUE(names_gate_ok),
    builds_ok     = isTRUE(builds$ok),
    changed       = manifest_changed,
    streams       = builds$summary,
    archive_files = I(archive_files))
  write_manifest(file.path(out_dir, "status.json"), status)

  list(changed = manifest_changed, manifest = manifest, archive_files = archive_files,
       status = status)
}

# Files for the archive branch. Only the newest applied report is listed, read
# again or not, so a stale copy never lands and a failed push heals next run.
upstream_files <- function(views_texts, views_times, views_cats, bioc_version, builds) {
  files <- lapply(views_cats, function(cat) {
    v <- views_texts[[cat]]
    list(path = file.path(bioc_version, "views", cat, "VIEWS"),
         text = attr(v, "raw") %||% paste0(as.character(v), "\n"),
         last_modified = views_times[[cat]])
  })
  applied <- builds$reports[builds$reports$outcome == "applied", , drop = FALSE]
  for (s in builds$streams) {
    if (!isTRUE(s$ok)) next
    mine <- applied$report_at[applied$bioc_version == s$bioc_version & applied$repo == s$repo]
    if (length(mine) == 0L || s$report_at != max(mine)) next
    dir <- file.path(s$bioc_version, "builds", s$repo)
    files[[length(files) + 1L]] <- list(path = file.path(dir, BUILD_FILES[["status"]]),
                                        text = s$status_body, last_modified = s$published_at)
    if (isTRUE(s$propagation_read)) {
      files[[length(files) + 1L]] <- list(path = file.path(dir, BUILD_FILES[["propagation"]]),
                                          text = s$propagation_body,
                                          last_modified = s$propagation_published_at)
    }
  }
  files
}

# ---------------------------------------------------------------------------
# Build reports: read after VIEWS, never fatal to the catalog
# ---------------------------------------------------------------------------

# One branch and repo's report, fetched and parsed, or ok = FALSE with a reason.
# A 404 for the index or propagation file means the report has none.
read_build_stream <- function(io, branch, repo, fallback_version, now) {
  base <- list(branch = branch, repo = repo, ok = FALSE, bioc_version = NA_character_)
  fail <- function(reason) c(base, reason = reason)
  get <- function(file) tryCatch(io$fetch_build_file(branch, repo, file), error = function(e) {
    message(sprintf("Build report %s/%s %s: %s", branch, repo, file, conditionMessage(e)))
    NULL
  })
  st <- get(BUILD_FILES[["status"]])
  if (is.null(st) || !identical(st$status, 200L)) {
    return(fail(sprintf("status file not read (HTTP %s)", st$status %||% "error")))
  }
  parsed <- parse_build_status_db(st$body)
  if (!parsed$valid) return(fail("status file failed validation"))

  ix <- get(BUILD_FILES[["index"]])
  if (is.null(ix) || !(ix$status %in% c(200L, 404L))) {
    return(fail(sprintf("index page not read (HTTP %s)", ix$status %||% "error")))
  }
  idx <- parse_report_index(if (identical(ix$status, 200L)) ix$body else NULL)
  if (is.na(idx$bioc_version)) {
    message(sprintf("Build report %s/%s: index.html gave no BioC version; using config.yaml",
                    branch, repo))
  }
  bioc_version <- idx$bioc_version %||% fallback_version
  if (is.na(bioc_version)) return(fail("BioC version unknown"))

  pr <- get(BUILD_FILES[["propagation"]])
  prop <- NULL
  if (!is.null(pr) && identical(pr$status, 404L)) {
    message(sprintf("Build report %s/%s: no propagation file", branch, repo))
  } else {
    if (!is.null(pr) && identical(pr$status, 200L)) prop <- parse_build_status_db(pr$body)
    if (is.null(prop) || !prop$valid) return(fail("propagation file not read"))
  }

  lines <- parsed$lines
  published_at <- st$last_modified %||% now
  list(branch = branch, repo = repo, ok = TRUE, bioc_version = bioc_version,
       report_at = idx$snapshot_at %||% published_at,
       snapshot_at = idx$snapshot_at, generated_at = idx$generated_at,
       published_at = published_at, status_sha256 = text_sha256(st$body),
       n_packages = length(unique(lines$package)), n_lines = nrow(lines),
       n_na = sum(lines$status == "NA"),
       nodes = paste(unique(lines$node), collapse = ","),
       versions = idx$versions,
       lines = if (is.null(prop)) lines else rbind(lines, prop$lines),
       propagation_read = !is.null(prop),
       propagation_published_at = if (is.null(prop)) NA_character_ else pr$last_modified %||% now,
       status_body = st$body,
       propagation_body = if (is.null(prop)) NULL else pr$body)
}

# One bioc_build_reports row for a stream.
build_report_row <- function(s, now, outcome) {
  data.frame(bioc_version = s$bioc_version, repo = s$repo, report_at = s$report_at,
             branch = s$branch, snapshot_at = s$snapshot_at,
             generated_at = s$generated_at, published_at = s$published_at,
             status_sha256 = s$status_sha256, n_packages = as.integer(s$n_packages),
             n_lines = as.integer(s$n_lines), n_na = as.integer(s$n_na),
             nodes = s$nodes, read_at = now, outcome = outcome,
             stringsAsFactors = FALSE)
}

# Versions each repo's aliases serve: this run's reads, or for a failed read the
# version that branch served last time. A repo with neither is left out.
served_versions <- function(streams, reports) {
  rows <- lapply(streams, function(s) {
    v <- s$bioc_version
    if (is.na(v)) {
      p <- reports[reports$branch == s$branch & reports$repo == s$repo, , drop = FALSE]
      if (nrow(p) == 0L) return(NULL)
      v <- p$bioc_version[order(p$read_at, decreasing = TRUE)[1L]]
    }
    data.frame(repo = s$repo, bioc_version = v, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(repo = character(0), bioc_version = character(0),
                                          stringsAsFactors = FALSE)), rows))
  unique(out)
}

# A stream's summary row as it stands until its report is read.
build_stream_row <- function(branch, repo, bioc_version = NA_character_) {
  list(branch = branch, repo = repo, bioc_version = bioc_version,
       report_at = NA_character_, lines = 0L, outcome = "fetch_failed",
       propagation = "not_read", new = 0L, extended = 0L, closed = 0L)
}

# One stream read and applied to the tables so far. Returns the stream, its
# summary row and the tables after it; a failed read returns them unchanged.
apply_build_stream <- function(io, branch, repo, fallback_version, now,
                               reports, history, prior_reports) {
  s <- read_build_stream(io, branch, repo, fallback_version, now)
  row <- build_stream_row(branch, repo, s$bioc_version)
  if (!isTRUE(s$ok)) {
    row$reason <- s$reason
    return(list(stream = s, row = row, reports = reports, history = history))
  }
  verdict <- build_report_verdict(reports, s$bioc_version, repo, s$report_at,
                                  s$status_sha256, s$published_at, s$n_packages)
  row$report_at <- s$report_at; row$lines <- s$n_lines; row$outcome <- verdict
  row$propagation <- if (s$propagation_read) "read" else "absent"
  if (s$propagation_read) {
    pf <- propagation_floor(history, s$lines, s$bioc_version, repo)
    if (pf$under) {
      # An empty or cut-short file would close every row it lacks as gone.
      s$propagation_read <- FALSE
      row$propagation <- "skipped_floor"
      row$reason <- sprintf("propagation file lists %d packages against %d with open rows",
                            pf$packages, pf$open)
    }
  }
  if (verdict == "applied") {
    # Censored unless an earlier report of this alias was applied.
    exact <- as.integer(any(prior_reports$branch == branch & prior_reports$repo == repo &
                              prior_reports$outcome == "applied"))
    r <- apply_build_report(history, s$lines,
                            list(bioc_version = s$bioc_version, repo = repo,
                                 report_at = s$report_at, versions = s$versions,
                                 propagation_read = s$propagation_read),
                            reports, exact)
    history <- r$history
    row$new <- r$counts[["new"]]; row$extended <- r$counts[["extended"]]
    row$closed <- r$counts[["closed"]]
  }
  if (verdict != "unchanged") {
    keep <- !(reports$bioc_version == s$bioc_version & reports$repo == repo &
                reports$report_at == s$report_at)
    reports <- rbind(reports[keep, , drop = FALSE], build_report_row(s, now, verdict))
  }
  list(stream = s, row = row, reports = reports, history = history)
}

# Reads and applies every branch and repo's report. ok is FALSE when a stream or
# its propagation file failed or was skipped, so the catch-up reads them again.
read_build_state <- function(io, prev, branch_versions, now) {
  reports <- conform_frame(prev$build_reports, empty_build_reports())
  history <- conform_frame(prev$build_status, empty_build_history())
  prior_reports <- reports
  streams <- list(); summary <- list()
  for (branch in BUILD_BRANCHES) for (repo in BUILD_REPOS) {
    # Any error fails this stream only and leaves the tables as they were.
    one <- tryCatch(
      apply_build_stream(io, branch, repo, branch_versions[[branch]], now,
                         reports, history, prior_reports),
      error = function(e) {
        reason <- conditionMessage(e)
        list(stream = list(branch = branch, repo = repo, ok = FALSE,
                           bioc_version = NA_character_, reason = reason),
             row = c(build_stream_row(branch, repo), reason = reason),
             reports = reports, history = history)
      })
    if (!is.null(one$row$reason)) {
      message(sprintf("Build report %s/%s: %s", branch, repo, one$row$reason))
    }
    reports <- one$reports; history <- one$history
    streams[[length(streams) + 1L]] <- one$stream
    summary[[length(summary) + 1L]] <- one$row
  }
  retired <- retire_build_versions(history, served_versions(streams, prior_reports), now)
  ok <- all(vapply(summary, function(x) {
    x$outcome %in% c("applied", "unchanged") && x$propagation != "skipped_floor"
  }, logical(1)))
  list(reports = reports, history = retired$history, streams = streams,
       summary = summary, retired = retired$closed, ok = ok)
}

# ---------------------------------------------------------------------------
# Prior catalog: anything short of "no release yet" stops the run
# ---------------------------------------------------------------------------

# HTTP status of the `current` release: 200, 404, or NA when gh gave no answer.
current_release_status <- function() {
  out <- suppressWarnings(system2(
    "gh", c("api", "-i", sprintf("repos/%s/releases/tags/current", PUBLISH_REPO)),
    stdout = TRUE, stderr = FALSE))
  first <- if (length(out) > 0L) out[[1L]] else ""
  suppressWarnings(as.integer(sub("^HTTP/[0-9.]+ ([0-9]{3}).*$", "\\1", first)))
}

# Reads a downloaded catalog db. Any read error stops the run. A table the db
# predates is NULL, and the package and state tables are checked against the
# manifest.
read_catalog_db <- function(db_path, manifest = list()) {
  # synchronous = NULL skips a PRAGMA that only warns on a damaged file; the
  # first query below is what reports it.
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path, synchronous = NULL)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  fail <- function(e) {
    stop("Prior catalog cannot be read: ", conditionMessage(e), call. = FALSE)
  }
  present <- tryCatch(RSQLite::dbGetQuery(con, paste(
    "SELECT name FROM sqlite_master",
    "WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"))$name, error = fail)
  read <- function(tbl) {
    if (!(tbl %in% present)) return(NULL)
    tryCatch(RSQLite::dbGetQuery(con, sprintf('SELECT * FROM "%s"', tbl)), error = fail)
  }
  pkgs  <- read("bioc_packages")
  auths <- read("bioc_authors")
  if (is.null(pkgs) || is.null(auths)) {
    stop("Prior catalog cannot be read: bioc_packages or bioc_authors is missing",
         call. = FALSE)
  }
  check_prior_packages(nrow(pkgs), manifest)
  state <- setNames(lapply(BIOC_STATE_TABLES, read), BIOC_STATE_TABLES)
  check_prior_state(lapply(state, function(d) if (is.null(d)) NULL else nrow(d)),
                    manifest)
  list(packages = pkgs, authors = auths,
       view_edges = read("bioc_view_edges") %||% data.frame(
         release = character(0), parent = character(0), child = character(0),
         stringsAsFactors = FALSE),
       manifest = manifest,
       names_all = read("bioc_names_all") %||% empty_bioc_names_all(),
       build_reports = state[["bioc_build_reports"]],
       build_status  = state[["bioc_build_status_history"]],
       views_history = state[["bioc_views_history"]])
}

# The prior catalog from the `current` release. No release is a bootstrap; a
# release whose manifest or db cannot be downloaded or read stops the run, so
# the catch-up retries instead of publishing a cold start over the history.
read_prev_catalog <- function(sleep = Sys.sleep) {
  status <- with_retry({
    s <- current_release_status()
    if (!(s %in% c(200L, 404L))) {
      stop(sprintf("Could not tell whether the current release exists (status %s)", s))
    }
    s
  }, sleep = sleep)
  if (identical(status, 404L)) {
    message("No current release yet; starting from scratch")
    return(list(manifest = list(), names_all = empty_bioc_names_all()))
  }

  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  fetch <- function(asset) {
    with_retry({
      st <- suppressWarnings(system2(
        "gh", c("release", "download", "current", "--repo", PUBLISH_REPO,
                "--pattern", asset, "--dir", tmp_dir, "--clobber"),
        stdout = FALSE, stderr = FALSE))
      path <- file.path(tmp_dir, asset)
      if (!identical(as.integer(st), 0L) || !file.exists(path)) {
        stop(sprintf("Prior %s download failed (gh release download current)", asset))
      }
      path
    }, sleep = sleep)
  }
  mf <- fetch("manifest.json")
  manifest <- tryCatch(jsonlite::read_json(mf), error = function(e) {
    stop("Prior manifest cannot be read: ", conditionMessage(e), call. = FALSE)
  })
  read_catalog_db(fetch("bioconductor-metadata.db"), manifest)
}

# ---------------------------------------------------------------------------
# default_io: real network fetchers
# ---------------------------------------------------------------------------

default_io <- function(sleep = Sys.sleep, http = http_get) {
  list(
    config_yaml = function() {
      with_retry(
        paste(readLines(url(CONFIG_YAML_URL), warn = FALSE), collapse = "\n")
      )
    },

    # The VIEWS text, with the file's Last-Modified as an attribute.
    fetch_views = function(cat) {
      with_retry({
        r <- http(VIEWS_URLS[[cat]])
        if (!identical(r$status, 200L)) {
          stop(sprintf("HTTP %s for %s", r$status, VIEWS_URLS[[cat]]))
        }
        structure(views_body_text(r$body), last_modified = r$last_modified, raw = r$body)
      }, sleep = sleep)
    },

    # One build report file. A 404 comes back as a result; 5xx and 429 retry.
    fetch_build_file = function(branch, repo, file) {
      u <- build_file_url(branch, repo, file)
      with_retry({
        r <- http(u)
        if (r$status >= 500L || r$status == 429L) stop(sprintf("HTTP %s for %s", r$status, u))
        r
      }, waits = ITEM_RETRY_WAITS_S, sleep = sleep)
    },

    list_repos = function() {
      out <- suppressWarnings(system2(
        "gh",
        c("api", "--paginate", sprintf("orgs/%s/repos?per_page=100", BIOC_ORG),
          "--jq", ".[].name"),
        stdout = TRUE, stderr = FALSE))
      # gh prints a failed page's error body to stdout, so a failed exit means
      # a partial listing that can also hold that body as a name.
      status <- attr(out, "status")
      if (!is.null(status) && status != 0L) {
        stop(sprintf(
          "Repository listing failed (gh exit %s); not crawling a partial listing",
          status))
      }
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
        paste(readLines(url(url_str), warn = FALSE), collapse = "\n"),
        waits = ITEM_RETRY_WAITS_S
      )
    },

    fetch_biocviews_dot = function(branch) {
      url_str <- paste(BIOC_RAW_BASE, "biocViews", branch,
                       "inst/dot/biocViewsVocab.dot", sep = "/")
      tryCatch(
        with_retry(
          paste(readLines(url(url_str), warn = FALSE), collapse = "\n"),
          waits = ITEM_RETRY_WAITS_S
        ),
        error = function(e) NULL
      )
    },

    prev_catalog = function() read_prev_catalog(sleep = sleep)
  )
}

# ---------------------------------------------------------------------------
# Entry point when run as a standalone script
# ---------------------------------------------------------------------------

# Exit status for the workflow: 0 when the catalog was built, even if a build
# report stream failed (the status file says so), 1 when it was not.
main <- function(args = commandArgs(trailingOnly = TRUE), io = default_io(),
                 live_floor = BIOC_LIVE_FLOOR) {
  out_dir <- if (length(args) >= 1L) args[1L] else "out"
  force_full <- "--bootstrap" %in% args
  res <- run_update(io, out_dir, force_full, live_floor = live_floor)
  if (isTRUE(res$status$catalog_ok)) 0L else 1L
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = main())
}
