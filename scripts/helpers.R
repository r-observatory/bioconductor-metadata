# scripts/helpers.R: pure helper functions for the bioconductor-metadata pipeline.

#' Null/NA/empty coalescing operator.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

#' Map a Bioconductor release "X.Y" to a sortable number (major*1000 + minor).
release_to_numeric <- function(rel) {
  p <- as.integer(strsplit(rel, ".", fixed = TRUE)[[1]])
  if (length(p) < 2 || any(is.na(p))) return(NA_real_)
  p[1] * 1000 + p[2]
}

#' Convert a named dates vector (version -> ISO date) to an ordered
#' bioc_releases data.frame with columns version, released, seq, r_version.
#' Rows are ordered by release_to_numeric ascending; seq is 1-based.
#' Empty-safe: returns the 4-column zero-row frame when dates is empty.
#' @param r_versions  Named character vector (bioc version -> R version) as
#'   returned by parse_r_ver_for_bioc(). NULL or zero-length gives NA for all.
bioc_releases_from_dates <- function(dates, r_versions = NULL) {
  cols  <- c("version", "released", "seq", "r_version")
  empty <- setNames(
    data.frame(
      character(0), character(0), integer(0), character(0),
      stringsAsFactors = FALSE
    ),
    cols
  )
  if (length(dates) == 0L) return(empty)
  nums <- vapply(names(dates), release_to_numeric, numeric(1))
  ord  <- order(nums)
  vers <- names(dates)[ord]
  r_ver <- if (!is.null(r_versions) && length(r_versions) > 0L) {
    unname(r_versions[vers])
  } else {
    rep(NA_character_, length(vers))
  }
  data.frame(
    version   = vers,
    released  = unname(dates[ord]),
    seq       = seq_len(length(dates)),
    r_version = r_ver,
    stringsAsFactors = FALSE
  )
}

#' Parse a Bioconductor VIEWS file (DCF text) into a catalog data.frame.
#' Returns a stable 14-column data.frame (zero rows when input is empty or invalid).
parse_views <- function(views_text, category) {
  cols <- c("name","name_lower","category","version","title","description",
            "maintainer","maintainer_email","license","depends","imports",
            "suggests","biocviews","git_url")
  empty <- setNames(data.frame(matrix(character(0), ncol = length(cols)),
                               stringsAsFactors = FALSE), cols)
  if (!nzchar(trimws(views_text))) return(empty)
  m <- tryCatch(read.dcf(textConnection(views_text)), error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0) return(empty)
  g <- function(field) if (field %in% colnames(m)) as.character(m[, field]) else rep(NA_character_, nrow(m))
  maint_raw <- g("Maintainer")
  email <- sub(".*<([^>]+)>.*", "\\1", maint_raw); email[email == maint_raw] <- NA_character_
  name  <- trimws(sub("<.*>", "", maint_raw)); name[!nzchar(name)] <- NA_character_
  pkg <- g("Package")
  data.frame(
    name = pkg, name_lower = tolower(pkg), category = category,
    version = g("Version"), title = g("Title"), description = g("Description"),
    maintainer = name, maintainer_email = email, license = g("License"),
    depends = g("Depends"), imports = g("Imports"), suggests = g("Suggests"),
    biocviews = g("biocViews"), git_url = g("git_url"),
    stringsAsFactors = FALSE)
}

#' Parse an Authors@R field (R code) into a data.frame of author rows.
#' Evaluates the expression in a restricted environment that exposes only
#' `person` and `c`, limiting arbitrary-code risk from untrusted DESCRIPTION
#' content. Returns an empty 6-column frame on any parse/eval failure.
parse_authors_at_r <- function(authors_r_text, package) {
  cols <- c("package","given","family","email","role","orcid")
  empty <- setNames(data.frame(matrix(character(0), ncol = 6), stringsAsFactors = FALSE), cols)
  if (is.na(authors_r_text) || !nzchar(trimws(authors_r_text))) return(empty)
  env <- new.env(parent = emptyenv())
  env$person <- utils::person
  env$c      <- base::c
  pp <- tryCatch(eval(parse(text = authors_r_text), envir = env), error = function(e) NULL)
  if (is.null(pp) || length(pp) == 0) return(empty)
  rows <- lapply(seq_along(pp), function(i) {
    p <- pp[i]
    orc <- tryCatch(unname(p$comment[["ORCID"]]), error = function(e) NULL)
    data.frame(
      package = package,
      given  = paste(p$given,  collapse = " "),
      family = paste(p$family, collapse = " "),
      email  = if (length(p$email)) p$email[1] else NA_character_,
      role   = if (length(p$role))  paste(p$role, collapse = ", ") else NA_character_,
      orcid  = if (!is.null(orc) && nzchar(orc)) orc else NA_character_,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[nzchar(out$given) | nzchar(out$family), , drop = FALSE]
}

#' Derive a package's Bioconductor release lineage from its git branch names.
#' Returns a named list with first_release, first_release_date, last_release,
#' last_release_date, in_current, and in_devel.
#' @param branches  Character vector of git branch names (e.g. from git ls-remote).
#' @param current_release  The current Bioconductor release string, e.g. "3.23".
#' @param dates  Named character vector mapping release string -> ISO date, as
#'   returned by parse_release_dates().
package_lineage <- function(branches, current_release, dates) {
  rels <- sub("^RELEASE_", "", grep("^RELEASE_[0-9]+_[0-9]+$", branches, value = TRUE))
  rels <- gsub("_", ".", rels)
  res <- list(first_release = NA_character_, first_release_date = NA_character_,
              last_release = NA_character_, last_release_date = NA_character_,
              in_current = FALSE,
              in_devel = any(branches %in% c("devel", "master")))
  if (length(rels) == 0) return(res)
  ord <- order(vapply(rels, release_to_numeric, numeric(1)))
  rels <- rels[ord]
  first <- rels[1]; last <- rels[length(rels)]
  res$first_release <- first; res$last_release <- last
  res$first_release_date <- unname(dates[first]) %||% NA_character_
  res$last_release_date  <- unname(dates[last])  %||% NA_character_
  res$in_current <- current_release %in% rels
  res
}

#' Export the assembled catalog to a fresh SQLite database.
#'
#' Creates (or replaces) the file at `path` with three tables:
#'   bioc_packages  -- one row per package (21 columns)
#'   bioc_authors   -- one row per author credit (6 columns)
#'   bioc_releases  -- ordered release list (version, released, seq)
#' and four indexes for common lookup patterns.
#'
#' @param path        File path for the output .db file.
#' @param packages_df data.frame with exactly the 21 bioc_packages columns in
#'   schema order (name, name_lower, category, version, title, description,
#'   maintainer, maintainer_email, license, depends, imports, suggests,
#'   biocviews, git_url, first_release, first_release_date, last_release,
#'   last_release_date, in_current, in_devel, updated_at).
#' @param authors_df  data.frame with 6 bioc_authors columns in schema order
#'   (package, given, family, email, role, orcid).
#' @param releases_df data.frame with 4 bioc_releases columns (version, released,
#'   seq, r_version) as returned by bioc_releases_from_dates(). NULL or 0-row
#'   creates the empty table only.
export_catalog <- function(path, packages_df, authors_df, releases_df = NULL) {
  if (file.exists(path)) unlink(path)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_packages (
      name TEXT PRIMARY KEY,
      name_lower TEXT NOT NULL,
      category TEXT NOT NULL,
      version TEXT,
      title TEXT,
      description TEXT,
      maintainer TEXT,
      maintainer_email TEXT,
      license TEXT,
      depends TEXT,
      imports TEXT,
      suggests TEXT,
      biocviews TEXT,
      git_url TEXT,
      first_release TEXT,
      first_release_date TEXT,
      last_release TEXT,
      last_release_date TEXT,
      in_current INTEGER NOT NULL DEFAULT 0,
      in_devel INTEGER NOT NULL DEFAULT 0,
      updated_at TEXT
    )
  ")

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_authors (
      package TEXT NOT NULL,
      given TEXT,
      family TEXT,
      email TEXT,
      role TEXT,
      orcid TEXT
    )
  ")

  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_meta_lower ON bioc_packages(name_lower)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_authors_package ON bioc_authors(package)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_authors_name ON bioc_authors(family, given)")

  RSQLite::dbWriteTable(con, "bioc_packages", packages_df, append = TRUE)
  RSQLite::dbWriteTable(con, "bioc_authors",  authors_df,  append = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_releases (
      version   TEXT PRIMARY KEY,
      released  TEXT,
      seq       INTEGER,
      r_version TEXT
    )
  ")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_releases_seq ON bioc_releases(seq)")

  if (!is.null(releases_df) && nrow(releases_df) > 0L) {
    RSQLite::dbWriteTable(con, "bioc_releases", releases_df, append = TRUE)
  }

  RSQLite::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write an R list as pretty-printed JSON.
#'
#' @param path File path for the output .json file.
#' @param obj  R list to serialise.
write_manifest <- function(path, obj) {
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}

#' Parse the `r_ver_for_bioc_ver:` block of config.yaml into a named character
#' vector mapping Bioconductor version -> R version string. Returns an empty
#' named character vector when the key is absent.
parse_r_ver_for_bioc <- function(yaml_text) {
  y  <- yaml::yaml.load(yaml_text)
  rv <- y$r_ver_for_bioc_ver
  if (is.null(rv)) return(setNames(character(0), character(0)))
  setNames(as.character(unlist(rv)), names(rv))
}

#' Parse the `release_dates:` block of config.yaml into a named vector of ISO
#' dates (release -> YYYY-MM-DD). Uses the yaml parser; the source dates are
#' M/D/YYYY or MM/DD/YYYY. Non-date entries are dropped.
parse_release_dates <- function(yaml_text) {
  y <- yaml::yaml.load(yaml_text)
  rd <- y$release_dates
  if (is.null(rd)) return(setNames(character(0), character(0)))
  # NOTE: Release keys such as "3.20" MUST be quoted in config.yaml.
  # An unquoted 3.20 is parsed by YAML as the number 3.2, silently dropping
  # the trailing zero and producing an unrecoverable wrong key. The names()
  # call below reads keys as character strings, but only if the YAML source
  # keeps them quoted (e.g. "3.20": ...). Keep that quoting in place.
  out <- vapply(rd, function(v) {
    d <- as.Date(as.character(v), tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
    if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
  }, character(1))
  out[!is.na(out)]
}
