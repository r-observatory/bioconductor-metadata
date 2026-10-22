# scripts/checksums.R: each release's package checksums, from the PACKAGES file
# of each of its repositories.

#' URL of a release repository's PACKAGES file.
packages_url <- function(release, repository) {
  sprintf("%s/%s/%s/src/contrib/PACKAGES", PACKAGES_BASE, release, repository)
}

#' Zero-row bioc_package_checksums frame, in schema order.
empty_bioc_checksums <- function() {
  data.frame(release = character(0), repository = character(0), package = character(0),
             version = character(0), md5sum = character(0), stringsAsFactors = FALSE)
}

#' Package, Version and MD5sum of each entry in a PACKAGES file. An entry
#' lacking any of them is left out and a package listed twice keeps its first
#' entry. valid is FALSE for text that is not a PACKAGES file, such as an error
#' page served with a 200.
parse_packages_checksums <- function(text) {
  out <- list(valid = FALSE,
              rows = data.frame(package = character(0), version = character(0),
                                md5sum = character(0), stringsAsFactors = FALSE),
              entries = 0L, incomplete = 0L, repeated = character(0))
  if (!is.character(text) || length(text) != 1L || is.na(text)) return(out)
  if (!nzchar(trimws(text))) {
    out$valid <- TRUE
    return(out)
  }
  con <- textConnection(gsub("\r\n", "\n", text, fixed = TRUE))
  on.exit(close(con), add = TRUE)
  m <- tryCatch(read.dcf(con, fields = c("Package", "Version", "MD5sum")),
                error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0L || all(is.na(m[, "Package"]))) return(out)
  has <- function(x) !is.na(x) & nzchar(x)
  full <- has(m[, "Package"]) & has(m[, "Version"]) & has(m[, "MD5sum"])
  m <- m[full, , drop = FALSE]
  again <- duplicated(m[, "Package"])
  out$valid <- TRUE
  out$entries <- length(full)
  out$incomplete <- sum(!full)
  out$repeated <- unique(unname(m[again, "Package"]))
  m <- m[!again, , drop = FALSE]
  out$rows <- data.frame(package = unname(m[, "Package"]), version = unname(m[, "Version"]),
                         md5sum = unname(m[, "MD5sum"]), stringsAsFactors = FALSE)
  out
}

#' Release repositories to read: the live releases first, then the frozen ones
#' newest first, each with its repositories in CHECKSUM_REPOS order.
checksum_pairs <- function(releases, live, repos = CHECKSUM_REPOS) {
  live <- live[!is.na(live) & nzchar(live)]
  rel <- unique(c(live, releases))
  rel <- rel[!is.na(rel) & nzchar(rel)]
  num <- vapply(rel, release_to_numeric, numeric(1))
  rel <- rel[order(!(rel %in% live), -num)]
  d <- expand.grid(repository = repos, release = rel, stringsAsFactors = FALSE)
  data.frame(release = d$release, repository = d$repository,
             key = paste(d$release, d$repository, sep = "/"),
             live = d$release %in% live, stringsAsFactors = FALSE)
}

#' The prior catalog's checksum rows and the frozen and missing repositories its
#' manifest lists. A table holding fewer rows than the manifest counts has lost
#' some, so every release is read again.
prior_checksum_state <- function(prev) {
  rows <- conform_frame(prev$checksums, empty_bioc_checksums())
  listed <- prev$manifest$checksums
  frozen <- as.character(unlist(listed$frozen))
  missing <- as.character(unlist(listed$missing))
  want <- suppressWarnings(as.numeric(prev$manifest$tables$bioc_package_checksums %||% NA))
  if (!is.na(want) && nrow(rows) < want) {
    message(sprintf(paste("Prior catalog holds %d checksum rows against %s in its manifest;",
                          "reading every release again"), nrow(rows), format(want)))
    frozen <- character(0)
    missing <- character(0)
  }
  list(rows = rows, frozen = frozen, missing = missing)
}

#' One release repository's PACKAGES file: outcome "read" with its rows, or
#' "missing" for a 404. Anything else is an error.
read_checksum_repo <- function(io, release, repository) {
  key <- paste(release, repository, sep = "/")
  r <- io$fetch_packages(release, repository)
  status <- suppressWarnings(as.integer(r$status))
  if (identical(status, 404L)) {
    message(sprintf("Package checksums %s: no PACKAGES file (HTTP 404)", key))
    return(list(outcome = "missing"))
  }
  if (!identical(status, 200L)) stop(sprintf("HTTP %s", r$status))
  p <- parse_packages_checksums(r$body)
  if (!p$valid) stop("not a PACKAGES file")
  if (p$incomplete > 0L) {
    message(sprintf("Package checksums %s: %d entries lack Package, Version or MD5sum and are left out",
                    key, p$incomplete))
  }
  if (length(p$repeated) > 0L) {
    message(sprintf("Package checksums %s: %s listed more than once; keeping the first entry",
                    key, paste(p$repeated, collapse = ", ")))
  }
  n <- nrow(p$rows)
  list(outcome = "read",
       rows = data.frame(release = rep(release, n), repository = rep(repository, n), p$rows,
                         stringsAsFactors = FALSE))
}

#' Every release's checksums. Live releases are read again and a frozen release
#' repository once; whatever is not read keeps its prior rows. Reading stops for
#' the run after fail_streak failures in a row or once budget_s has passed.
read_checksum_state <- function(io, prev, releases, live, clock = Sys.time,
                                budget_s = CHECKSUM_TIME_BUDGET_S,
                                fail_streak = CHECKSUM_FAIL_STREAK) {
  prior <- prior_checksum_state(prev)
  pairs <- checksum_pairs(releases, live)
  todo <- pairs[pairs$live | !(pairs$key %in% prior$frozen), , drop = FALSE]
  read <- not_found <- failed <- deferred <- character(0)
  parts <- list()
  streak <- 0L
  started <- clock()
  for (i in seq_len(nrow(todo))) {
    spent <- as.numeric(difftime(clock(), started, units = "secs"))
    if (streak >= fail_streak || spent >= budget_s) {
      deferred <- todo$key[i:nrow(todo)]
      why <- if (streak >= fail_streak) sprintf("%d failures in a row", streak) else
        sprintf("%s seconds of reading", format(budget_s))
      message(sprintf("Package checksums: %d repositories left for the next run after %s",
                      length(deferred), why))
      break
    }
    key <- todo$key[i]
    one <- tryCatch(read_checksum_repo(io, todo$release[i], todo$repository[i]),
                    error = function(e) {
                      message(sprintf("Package checksums %s: %s; keeping its prior rows",
                                      key, conditionMessage(e)))
                      list(outcome = "failed")
                    })
    streak <- if (one$outcome == "failed") streak + 1L else 0L
    if (one$outcome == "read") {
      read <- c(read, key)
      parts[[key]] <- one$rows
    } else if (one$outcome == "missing") {
      not_found <- c(not_found, key)
    } else {
      failed <- c(failed, key)
    }
  }

  keys <- function(d) paste(d$release, d$repository, sep = "/")
  rows <- prior$rows[!(keys(prior$rows) %in% read), , drop = FALSE]
  rows <- do.call(rbind, c(list(rows), unname(parts)))
  rows <- rows[!duplicated(paste(keys(rows), rows$package, sep = "\r")), , drop = FALSE]
  u <- unique(rows$release)
  num <- setNames(vapply(u, release_to_numeric, numeric(1)), u)[rows$release]
  rows <- rows[order(num, rows$repository, rows$package, method = "radix"), , drop = FALSE]
  rownames(rows) <- NULL

  answered <- c(read, not_found)
  frozen <- union(setdiff(prior$frozen, pairs$key[pairs$live]),
                  intersect(answered, pairs$key[!pairs$live]))
  list(rows = rows,
       frozen = sort(frozen, method = "radix"),
       missing = sort(union(setdiff(prior$missing, answered), not_found), method = "radix"),
       failed = failed, deferred = deferred,
       releases_read = length(unique(todo$release[todo$key %in% read])),
       repositories_read = length(read))
}

#' The prior checksum state carried as it stands, for a run whose checksum step
#' stopped.
keep_checksum_state <- function(prev) {
  prior <- tryCatch(prior_checksum_state(prev), error = function(e) {
    list(rows = empty_bioc_checksums(), frozen = character(0), missing = character(0))
  })
  c(prior, list(failed = character(0), deferred = character(0),
                releases_read = 0L, repositories_read = 0L))
}

#' Publish-gate fingerprint of the checksum rows and the frozen repositories
#' read, "" when there are neither.
checksums_fingerprint <- function(rows, frozen) {
  if (nrow(rows) == 0L && length(frozen) == 0L) return("")
  lines <- paste(rows$release, rows$repository, rows$package, rows$version, rows$md5sum,
                 sep = "\t")
  text_sha256(c(sort(lines, method = "radix"), "", sort(frozen, method = "radix")))
}
