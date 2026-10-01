# scripts/views_history.R: VIEWS field values kept as episodes, timed by each
# VIEWS file's Last-Modified.

# Fields kept as episodes; every binary field is matched by VIEWS_BINARY_FIELD_RE
# so a renamed macOS platform arrives as a new field.
VIEWS_HISTORY_FIELDS  <- c("Version", "Date/Publication", "PackageStatus")
VIEWS_BINARY_FIELD_RE <- "^(source|win\\.binary|mac\\.binary.*)\\.ver$"

#' Zero-row bioc_views_history frame, in schema order.
empty_views_history <- function() {
  data.frame(package = character(0), field = character(0), episode_seq = integer(0),
             value = character(0), bioc_version = character(0), category = character(0),
             first_seen = character(0), last_seen = character(0),
             first_seen_exact = integer(0), ended_on = character(0),
             stringsAsFactors = FALSE)
}

#' One row per package and kept field in a category's VIEWS text. A package
#' listed twice keeps its first record.
views_state_rows <- function(views_text, category) {
  empty <- data.frame(package = character(0), field = character(0), value = character(0),
                      category = character(0), stringsAsFactors = FALSE)
  if (length(views_text) == 0L || is.na(views_text) || !nzchar(trimws(views_text))) return(empty)
  m <- tryCatch(read.dcf(textConnection(views_text)), error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0L || !("Package" %in% colnames(m))) return(empty)
  fields <- c(intersect(VIEWS_HISTORY_FIELDS, colnames(m)),
              grep(VIEWS_BINARY_FIELD_RE, colnames(m), value = TRUE))
  pkgs <- m[, "Package"]
  first <- !is.na(pkgs) & !duplicated(pkgs)
  rows <- lapply(fields, function(f) {
    v <- trimws(m[first, f])
    ok <- !is.na(v) & nzchar(v)
    data.frame(package = pkgs[first][ok], field = rep(f, sum(ok)), value = v[ok],
               category = rep(category, sum(ok)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  out
}

#' Apply the current VIEWS state to the episode history, per category.
#'   current  views_state_rows() over every category, first category first
#'   times    named Last-Modified per category (UTC ISO)
#'   apply    categories to diff; the rest carry forward untouched
#' An open row whose value and BioC version still hold is extended; a change
#' closes it and opens the next episode; a field gone from its package closes
#' it. A category whose file is older than its newest stored row is skipped as
#' a stale copy. Episodes opened in a category the history has never held are
#' marked first_seen_exact = 0.
apply_views_state <- function(history, current, times, apply, bioc_version) {
  key <- function(d) paste(d$package, d$field, sep = "\r")
  counts <- c(new = 0L, extended = 0L, closed = 0L)
  current <- current[!duplicated(key(current)), , drop = FALSE]
  open0 <- is.na(history$ended_on)
  skipped <- character(0)
  for (cat in apply) {
    seen <- history$last_seen[open0 & history$category == cat]
    if (length(seen) > 0L && times[[cat]] < max(seen)) skipped <- c(skipped, cat)
  }
  apply <- setdiff(apply, skipped)
  held <- unique(history$category)

  idx <- which(is.na(history$ended_on) & history$category %in% apply)
  cur <- current[current$category %in% apply, , drop = FALSE]
  m <- match(key(history[idx, , drop = FALSE]), key(cur))
  same <- !is.na(m) & history$value[idx] == cur$value[m] &
    history$bioc_version[idx] == bioc_version
  ext <- idx[same]
  history$last_seen[ext] <- unname(times[cur$category[m[same]]])
  diff <- idx[!is.na(m) & !same]
  history$ended_on[diff] <- unname(times[cur$category[m[!is.na(m) & !same]]])
  gone <- idx[is.na(m)]
  history$ended_on[gone] <- unname(times[history$category[gone]])

  open_keys <- key(history[is.na(history$ended_on), , drop = FALSE])
  to_open <- cur[!(key(cur) %in% open_keys), , drop = FALSE]
  if (nrow(to_open) > 0L) {
    top <- if (nrow(history) > 0L) tapply(history$episode_seq, key(history), max) else integer(0)
    prev_seq <- unname(top[key(to_open)])
    if (length(prev_seq) != nrow(to_open)) prev_seq <- rep(NA_integer_, nrow(to_open))
    prev_seq[is.na(prev_seq)] <- 0L
    at <- unname(times[to_open$category])
    history <- rbind(history, data.frame(
      package = to_open$package, field = to_open$field,
      episode_seq = as.integer(prev_seq) + 1L, value = to_open$value,
      bioc_version = bioc_version, category = to_open$category,
      first_seen = at, last_seen = at,
      first_seen_exact = as.integer(to_open$category %in% held),
      ended_on = NA_character_, stringsAsFactors = FALSE))
  }
  counts[["new"]] <- nrow(to_open)
  counts[["extended"]] <- length(ext)
  counts[["closed"]] <- length(diff) + length(gone)
  list(history = history, counts = counts, skipped = skipped)
}
