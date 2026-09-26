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

#' Map a VIEWS TRUE/FALSE flag to 1L/0L. A missing field or any other value is NA.
views_flag <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- rep(NA_integer_, length(x))
  out[x %in% "TRUE"]  <- 1L
  out[x %in% "FALSE"] <- 0L
  out
}

#' Parse a Bioconductor VIEWS file (DCF text) into a catalog data.frame.
#' Returns a stable 16-column data.frame (zero rows when input is empty or invalid).
parse_views <- function(views_text, category) {
  cols <- c("name","name_lower","category","version","title","description",
            "maintainer","maintainer_email","license","depends","imports",
            "suggests","biocviews","git_url","has_news","views_has_readme")
  empty <- setNames(data.frame(matrix(character(0), ncol = length(cols)),
                               stringsAsFactors = FALSE), cols)
  empty$has_news <- integer(0)
  empty$views_has_readme <- integer(0)
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
    has_news = views_flag(g("hasNEWS")), views_has_readme = views_flag(g("hasREADME")),
    stringsAsFactors = FALSE)
}

#' Zero-row bioc_vignettes frame with the published column types.
empty_bioc_vignettes <- function() {
  data.frame(package = character(0), release = character(0),
             category = character(0), version = character(0),
             seq = integer(0), file = character(0), title = character(0),
             output = character(0), url = character(0),
             stringsAsFactors = FALSE)
}

#' Split a VIEWS vignetteTitles value into titles. VIEWS writes a comma inside
#' a title doubled, so only a single comma followed by whitespace separates.
split_vignette_titles <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(x))) return(character(0))
  p <- strsplit(x, "(?<!,),(?!,)[[:space:]]+", perl = TRUE)[[1]]
  p <- gsub(",,", ",", p, fixed = TRUE)
  p <- gsub("[[:space:]]+", " ", trimws(p))
  sub('^"(.*)"$', "\\1", p)
}

#' One row per vignette file listed in a category's VIEWS text, for the given
#' release. Titles pair with files by position; when the counts differ every
#' title of that package is NA.
parse_views_vignettes <- function(views_text, category, release) {
  empty <- empty_bioc_vignettes()
  if (length(views_text) == 0L || is.na(views_text) || !nzchar(trimws(views_text))) return(empty)
  m <- tryCatch(read.dcf(textConnection(views_text)), error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0L || !("vignettes" %in% colnames(m))) return(empty)
  g <- function(field) if (field %in% colnames(m)) as.character(m[, field]) else rep(NA_character_, nrow(m))
  pkgs <- g("Package"); vers <- g("Version"); vigs <- g("vignettes"); tits <- g("vignetteTitles")
  base <- paste0("https://bioconductor.org/packages/", release, "/",
                 BIOC_REPO_PATHS[[category]], "/")
  # A repeated record would break the (package, seq) key and stop the whole run.
  again <- duplicated(pkgs) & !is.na(pkgs)
  if (any(again)) {
    message("VIEWS for ", category, " lists ", paste(unique(pkgs[again]), collapse = ", "),
            " more than once; keeping the first record")
  }
  rows <- lapply(seq_len(nrow(m)), function(i) {
    if (again[i] || is.na(pkgs[i]) || is.na(vigs[i]) || !nzchar(trimws(vigs[i]))) return(NULL)
    files <- trimws(strsplit(vigs[i], ",[[:space:]]+")[[1]])
    files <- files[nzchar(files)]
    if (length(files) == 0L) return(NULL)
    titles <- split_vignette_titles(tits[i])
    if (length(titles) != length(files)) titles <- rep(NA_character_, length(files))
    ext <- tolower(tools::file_ext(files))
    data.frame(package = pkgs[i], release = release, category = category,
               version = vers[i], seq = seq_along(files), file = files,
               title = titles, output = ifelse(nzchar(ext), ext, NA_character_),
               url = paste0(base, files), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' bioc_vignettes rows over every category of views_texts (a list named by
#' category, in VIEWS_URLS order). A package listed under two categories keeps
#' the rows of the first, and the repeat is logged.
build_bioc_vignettes <- function(views_texts, release) {
  out  <- empty_bioc_vignettes()
  seen <- character(0)
  for (cat in names(views_texts)) {
    v   <- parse_views_vignettes(views_texts[[cat]], cat, release)
    dup <- unique(v$package[v$package %in% seen])
    if (length(dup) > 0L) {
      message("Vignettes of ", paste(dup, collapse = ", "), " also listed under ",
              cat, "; keeping the earlier category")
      v <- v[!(v$package %in% dup), , drop = FALSE]
    }
    seen <- c(seen, unique(v$package))
    out  <- rbind(out, v)
  }
  rownames(out) <- NULL
  out
}

#' Zero-row bioc_authors frame in the published column order.
empty_bioc_authors <- function() {
  setNames(data.frame(matrix(character(0), ncol = length(BIOC_AUTHOR_COLS)),
                      stringsAsFactors = FALSE), BIOC_AUTHOR_COLS)
}

# An ORCID iD not glued to further digits, and a ROR id as it follows ror.org/.
ORCID_ID_PATTERN <- "(?<![0-9])[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X](?![0-9X])"
ROR_ID_PATTERN   <- "0[a-hj-km-np-tv-z0-9]{6}[0-9]{2}"

# ISO 7064 MOD 11-2, the check ORCID defines, so a mistyped iD never moves.
orcid_checksum_ok <- function(id) {
  vapply(id, function(one) {
    if (is.na(one)) return(FALSE)
    digits <- gsub("-", "", one, fixed = TRUE)
    if (!grepl("^[0-9]{15}[0-9X]$", digits)) return(FALSE)
    total <- 0
    for (d in as.integer(strsplit(substr(digits, 1, 15), "")[[1]])) total <- (total + d) * 2
    r <- (12 - total %% 11) %% 11
    identical(if (r == 10) "X" else as.character(r), substr(digits, 16, 16))
  }, logical(1), USE.NAMES = FALSE)
}

# One line per comment; a blank comment is stored as NULL, never as "".
collapse_comment_whitespace <- function(x) {
  x <- trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# Never truncates: the viewer reads review links from the full text. A comment
# empties only when a moved identifier and its label were all it held.
normalize_author_comments <- function(comment, orcid, ror_id) {
  comment <- collapse_comment_whitespace(as.character(comment))
  orcid   <- as.character(orcid)
  ror_id  <- as.character(ror_id)
  absent  <- function(v) is.na(v) || !nzchar(v)
  orcid_hits <- regmatches(comment, gregexpr(ORCID_ID_PATTERN, comment, perl = TRUE))
  ror_hits   <- regmatches(comment, gregexpr(
    paste0("(?<![a-z0-9-])ror\\.org/", ROR_ID_PATTERN, "(?![a-z0-9])"), comment, perl = TRUE))
  n_orcid <- 0L
  n_ror   <- 0L
  for (i in which(!is.na(comment))) {
    rest  <- comment[i]
    moved <- FALSE
    # The same iD written twice is still one iD.
    id <- unique(orcid_hits[[i]])
    if (absent(orcid[i]) && length(id) == 1L && orcid_checksum_ok(id)) {
      orcid[i] <- id
      n_orcid  <- n_orcid + 1L
      moved    <- TRUE
      rest <- gsub(paste0("(?i)(orcid(\\s*id)?\\s*[:=]?\\s*)?[\"'<]?((https?://)?(www\\.)?orcid\\.org/)?",
                          id, "[\"'>]?"), "", rest, perl = TRUE)
    }
    id <- unique(sub("^ror\\.org/", "", ror_hits[[i]]))
    if (absent(ror_id[i]) && length(id) == 1L) {
      ror_id[i] <- id
      n_ror     <- n_ror + 1L
      moved     <- TRUE
      rest <- gsub(paste0("(?i)(ror(\\s*id)?\\s*[:=]?\\s*)?[\"'<]?(https?://)?(www\\.)?ror\\.org/",
                          id, "[\"'>]?"), "", rest, perl = TRUE)
    }
    if (moved && grepl("^[[:punct:][:space:]]*$", rest)) comment[i] <- NA_character_
  }
  list(comment = comment, orcid = orcid, ror_id = ror_id,
       n_orcid = n_orcid, n_ror = n_ror)
}

# The cleaning cran-metadata's sanitize_df gives every author field: control
# characters other than tab, LF and CR removed, then UTF-8 forced.
sanitize_comment_text <- function(x) {
  x <- gsub("[\\x{00}-\\x{08}\\x{0b}\\x{0c}\\x{0e}-\\x{1f}]", "", x, perl = TRUE)
  iconv(x, to = "UTF-8", sub = "")
}

#' Parse an Authors@R field (R code) into a data.frame of author rows.
#' Evaluates the expression in a restricted environment that exposes only
#' `person` and `c`, limiting arbitrary-code risk from untrusted DESCRIPTION
#' content. Returns an empty 8-column frame on any parse/eval failure.
parse_authors_at_r <- function(authors_r_text, package) {
  empty <- empty_bioc_authors()
  if (is.na(authors_r_text) || !nzchar(trimws(authors_r_text))) return(empty)
  env <- new.env(parent = emptyenv())
  env$person <- utils::person
  env$c      <- base::c
  pp <- tryCatch(eval(parse(text = authors_r_text), envir = env), error = function(e) NULL)
  if (is.null(pp) || length(pp) == 0) return(empty)
  rows <- lapply(seq_along(pp), function(i) {
    p <- pp[i]
    orc <- tryCatch(unname(p$comment[["ORCID"]]), error = function(e) NULL)
    orc <- if (length(orc) && !is.na(orc[1]))
      trimws(sub("^https?://orcid\\.org/", "", trimws(orc[1]))) else NA_character_
    # Strip before the empty check, so a bare orcid.org/ prefix is NA, not "".
    if (!is.na(orc) && !nzchar(orc)) orc <- NA_character_
    ror <- tryCatch(unname(p$comment[["ROR"]]), error = function(e) NULL)
    ror <- if (length(ror) && !is.na(ror[1])) sub("^https://ror\\.org/", "", trimws(ror[1])) else NA_character_
    if (!is.na(ror) && !grepl(paste0("^", ROR_ID_PATTERN, "$"), ror)) ror <- NA_character_
    # A bad comment must not cost the person row or its identifiers.
    ids <- tryCatch({
      cm   <- p$comment
      nm   <- names(cm)
      # Every part but ORCID and ROR, as tools::CRAN_authors_db() stores it.
      free <- if (is.null(nm)) cm else cm[!(nm %in% c("ORCID", "ROR"))]
      free <- free[!is.na(free) & nzchar(trimws(free))]
      text <- if (length(free)) sanitize_comment_text(paste(free, collapse = ", ")) else NA_character_
      normalize_author_comments(text, orc, ror)
    }, error = function(e) {
      message("Could not read an author comment in ", package, ": ", conditionMessage(e))
      list(orcid = orc, ror_id = ror, comment = NA_character_)
    })
    data.frame(
      package = package,
      given   = paste(p$given,  collapse = " "),
      family  = paste(p$family, collapse = " "),
      email   = if (length(p$email)) p$email[1] else NA_character_,
      role    = if (length(p$role))  paste(p$role, collapse = ", ") else NA_character_,
      orcid   = ids$orcid,
      ror_id  = ids$ror_id,
      comment = ids$comment,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[nzchar(out$given) | nzchar(out$family), , drop = FALSE]
}

#' Prior bioc_authors rows of the packages in keep, in the published column
#' order. A column the prior catalog predates is filled with NA.
carry_forward_authors <- function(prev_authors, keep) {
  if (is.null(prev_authors) || nrow(prev_authors) == 0L) return(empty_bioc_authors())
  carry <- prev_authors[prev_authors$package %in% keep, , drop = FALSE]
  for (col in setdiff(BIOC_AUTHOR_COLS, names(carry))) {
    carry[[col]] <- rep(NA_character_, nrow(carry))
  }
  carry <- carry[, BIOC_AUTHOR_COLS, drop = FALSE]
  rownames(carry) <- NULL
  carry
}

#' Share of current software and workflows packages present in a repository
#' listing. 1 when VIEWS lists none, so an empty VIEWS never blocks here.
views_code_coverage <- function(repos, views_df) {
  code <- unique(views_df$name[views_df$category %in% c("software", "workflows")])
  if (length(code) == 0L) return(1)
  mean(code %in% repos)
}

#' TRUE when a prior bioc_authors frame predates ror_id or comment, so every
#' repository is crawled once to fill them.
authors_need_migration <- function(prev_authors) {
  !all(c("ror_id", "comment") %in% names(prev_authors))
}

#' Current software and workflows packages whose VIEWS version differs from the
#' prior catalog's, so their DESCRIPTION is read again. Data packages have no
#' RELEASE branches on github.com/bioc and are left out, as in the lineage
#' backfill.
version_bumped_packages <- function(prev_pkgs, views_df) {
  if (is.null(prev_pkgs) || nrow(prev_pkgs) == 0L || nrow(views_df) == 0L) {
    return(character(0))
  }
  code     <- views_df[views_df$category %in% c("software", "workflows"), , drop = FALSE]
  prev_ver <- prev_pkgs$version[match(code$name, prev_pkgs$name)]
  known    <- code$name %in% prev_pkgs$name
  differs  <- (is.na(prev_ver) != is.na(code$version)) |
    (!is.na(prev_ver) & !is.na(code$version) & prev_ver != code$version)
  unique(code$name[known & differs])
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

#' Parse a Bioconductor biocViewsVocab.dot file into a parent/child edge table.
#'
#' Strips block comments (/* ... */), line comments (//), the digraph header
#' and braces, then extracts every `NAME -> NAME ;` edge as written (typos
#' included, no deduplication beyond exact-duplicate rows).
#'
#' @param dot_text  Character string containing the full .dot file text.
#' @return data.frame(parent, child) -- zero rows on empty or unrecognised input.
parse_biocviews_dot <- function(dot_text) {
  empty <- data.frame(parent = character(0), child = character(0),
                      stringsAsFactors = FALSE)
  if (length(dot_text) == 0L || !nzchar(trimws(paste(dot_text, collapse = "\n")))) {
    return(empty)
  }
  # Collapse vector input to a single string, then strip block comments.
  text <- paste(dot_text, collapse = "\n")
  text <- gsub("(?s)/\\*.*?\\*/", "", text, perl = TRUE)
  # Split into lines, strip // line comments, trim whitespace.
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  lines <- sub("//.*$", "", lines)
  lines <- trimws(lines)
  # Match edge pattern: NAME -> NAME ;
  pat <- "^([A-Za-z0-9_.]+)\\s*->\\s*([A-Za-z0-9_.]+)\\s*;\\s*$"
  m <- regmatches(lines, regexec(pat, lines))
  matched <- Filter(function(x) length(x) == 3L, m)
  if (length(matched) == 0L) return(empty)
  data.frame(
    parent = vapply(matched, `[[`, character(1L), 2L),
    child  = vapply(matched, `[[`, character(1L), 3L),
    stringsAsFactors = FALSE
  )
}

#' Export the assembled catalog to a fresh SQLite database.
#'
#' Creates (or replaces) the file at `path` with these tables:
#'   bioc_packages    -- one row per package (23 columns)
#'   bioc_authors     -- one row per author credit (8 columns)
#'   bioc_releases    -- ordered release list (version, released, seq)
#'   bioc_view_edges  -- biocViews DAG edges (release, parent, child)
#'   bioc_vignettes   -- one row per current-release vignette file
#' and six indexes for common lookup patterns.
#'
#' @param path          File path for the output .db file.
#' @param packages_df   data.frame with the 23 bioc_packages columns in
#'   schema order (name, name_lower, category, version, title, description,
#'   maintainer, maintainer_email, license, depends, imports, suggests,
#'   biocviews, git_url, first_release, first_release_date, last_release,
#'   last_release_date, in_current, in_devel, updated_at, has_news,
#'   views_has_readme).
#' @param authors_df    data.frame with the 8 bioc_authors columns in schema
#'   order (BIOC_AUTHOR_COLS: package, given, family, email, role, orcid,
#'   ror_id, comment).
#' @param releases_df   data.frame with 4 bioc_releases columns (version, released,
#'   seq, r_version) as returned by bioc_releases_from_dates(). NULL or 0-row
#'   creates the empty table only.
#' @param view_edges_df data.frame with 3 bioc_view_edges columns (release,
#'   parent, child) as produced by parse_biocviews_dot(). NULL or 0-row creates
#'   the empty table only.
#' @param vignettes_df  data.frame with the 9 bioc_vignettes columns as returned
#'   by build_bioc_vignettes(). NULL or 0-row creates the empty table only.
export_catalog <- function(path, packages_df, authors_df, releases_df = NULL,
                           view_edges_df = NULL, names_all_df = NULL,
                           vignettes_df = NULL) {
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
      updated_at TEXT,
      has_news INTEGER,
      views_has_readme INTEGER
    )
  ")

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_authors (
      package TEXT NOT NULL,
      given TEXT,
      family TEXT,
      email TEXT,
      role TEXT,
      orcid TEXT,
      ror_id TEXT,
      comment TEXT
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

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_view_edges (
      release TEXT,
      parent  TEXT,
      child   TEXT
    )
  ")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_view_edges_rel   ON bioc_view_edges(release)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_bioc_view_edges_child ON bioc_view_edges(release, child)")

  if (!is.null(view_edges_df) && nrow(view_edges_df) > 0L) {
    RSQLite::dbWriteTable(con, "bioc_view_edges", view_edges_df, append = TRUE)
  }

  RSQLite::dbExecute(con, "
    CREATE TABLE bioc_vignettes (
      package  TEXT NOT NULL,
      release  TEXT NOT NULL,
      category TEXT NOT NULL,
      version  TEXT,
      seq      INTEGER NOT NULL,
      file     TEXT NOT NULL,
      title    TEXT,
      output   TEXT,
      url      TEXT NOT NULL,
      PRIMARY KEY (package, seq)
    )
  ")
  if (!is.null(vignettes_df) && nrow(vignettes_df) > 0L) {
    RSQLite::dbWriteTable(con, "bioc_vignettes", vignettes_df, append = TRUE)
  }

  if (!is.null(names_all_df)) {
    RSQLite::dbExecute(con, "
      CREATE TABLE bioc_names_all (
        name_lower     TEXT PRIMARY KEY,
        canonical_name TEXT NOT NULL,
        identity_state TEXT NOT NULL,
        first_seen     TEXT NOT NULL,
        last_seen      TEXT NOT NULL
      )")
    if (nrow(names_all_df) > 0L) {
      RSQLite::dbWriteTable(con, "bioc_names_all",
        names_all_df[, c("name_lower", "canonical_name", "identity_state",
                         "first_seen", "last_seen"), drop = FALSE], append = TRUE)
    }
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

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  - present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    - macOS/local fallback
#' No extra dependency is required: the CI job installs RSQLite, DBI, jsonlite,
#' yaml, testthat, withr and curl, none of which pull in digest, so the
#' coreutils sha256sum path is taken on CI. If a sibling pipeline already
#' installs digest, that path wins automatically.
#'
#' @param path File whose bytes are hashed.
#' @return lowercase 64-character hex string.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path`. Call this only after the catalog is fully
#' written and its connection is closed (export_catalog closes its own handle
#' before returning), so no open journal skews the size or hash:
#'   * db_filename - basename of the file
#'   * db_bytes    - byte size of the file as a double. Deliberately NOT cast
#'                   to integer: R's integer range is 32-bit and overflows to
#'                   NA (serialized as the string "NA") for files >= ~2 GiB.
#'   * db_sha256   - lowercase hex sha256 of the file's exact bytes, hashed
#'                   only after the enumeration connection below is closed.
#'   * tables      - named list mapping each user table (sqlite_master
#'                   type='table', excluding sqlite_% internals) to its row count
#'   * complete    - passed through by the caller. complete = the DB holds the
#'                   full, non-partial dataset, NOT how fresh it is; freshness is
#'                   tracked separately via generated_at and the source
#'                   fingerprint. The caller derives it from the pipeline's
#'                   genuine partial/bootstrap state rather than hardcoding TRUE.
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present before consuming it.
#'
#' @param db_path  Path to the finalized SQLite database.
#' @param complete Honest boolean derived by the caller (see above).
#' @return named list of top-level integrity/completeness fields.
db_integrity_core <- function(db_path, complete) {
  stopifnot(file.exists(db_path))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  tables <- tryCatch({
    tbl_names <- RSQLite::dbGetQuery(con, "
      SELECT name FROM sqlite_master
       WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
       ORDER BY name")$name

    stats::setNames(
      lapply(tbl_names, function(t) {
        RSQLite::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
      }),
      tbl_names
    )
  }, finally = RSQLite::dbDisconnect(con))

  # db_bytes / db_sha256 read the raw on-disk file only after the enumeration
  # connection above is disconnected, so no open handle or journal file skews
  # the byte count or the hash.
  list(
    db_filename = basename(db_path),
    db_bytes    = file.size(db_path),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
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

#' Project bioc_packages into the shared name-authority shape. canonical_name is
#' the Bioc-cased name; identity_state is live when in_current else archived;
#' first_seen is the first_release_date (empty string when unknown, so it never
#' churns); last_seen is updated_at. One row per name_lower, keeping live on a
#' case collision.
build_bioc_names_all <- function(packages_df) {
  cols <- c("name_lower", "canonical_name", "identity_state", "first_seen", "last_seen")
  if (is.null(packages_df) || nrow(packages_df) == 0L) {
    empty <- as.data.frame(setNames(rep(list(character(0)), length(cols)), cols),
                           stringsAsFactors = FALSE)
    return(empty)
  }
  frd <- packages_df$first_release_date
  first_seen <- ifelse(is.na(frd) | frd == "", "", frd)
  df <- data.frame(
    name_lower     = packages_df$name_lower,
    canonical_name = packages_df$name,
    identity_state = ifelse(packages_df$in_current == 1L, "live", "archived"),
    first_seen     = first_seen,
    last_seen      = packages_df$updated_at,
    stringsAsFactors = FALSE
  )
  df <- df[order(df$name_lower, df$identity_state != "live"), , drop = FALSE]
  df <- df[!duplicated(df$name_lower), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Reject a truncated VIEWS fetch: FALSE when the live package count is below the
#' floor, signalling the caller to reuse the prior bioc_names_all.
bioc_names_size_ok <- function(n_live, floor = BIOC_LIVE_FLOOR) {
  is.finite(n_live) && n_live >= floor
}
