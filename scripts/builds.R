# scripts/builds.R: Bioconductor build reports, parsed and kept as status episodes.

#' URL of one build report file.
build_file_url <- function(branch, repo, file) {
  sprintf("%s/%s/%s-LATEST/%s", BUILD_REPORT_BASE, branch, repo, file)
}

#' Zero-row frame of parsed status lines.
empty_build_lines <- function() {
  data.frame(package = character(0), node = character(0), stage = character(0),
             status = character(0), detail = character(0), stringsAsFactors = FALSE)
}

#' Parse BUILD_STATUS_DB.txt or PROPAGATION_STATUS_DB.txt. Every non-blank line
#' must read `pkg#node#stage: STATUS`, or the file is invalid (a 200 carrying an
#' error page). NA is kept as the string "NA"; detail keeps the reason given
#' with a propagation NO.
parse_build_status_db <- function(text) {
  empty <- empty_build_lines()
  if (is.null(text) || length(text) != 1L || is.na(text)) {
    return(list(valid = FALSE, lines = empty))
  }
  raw <- strsplit(text, "\r?\n")[[1L]]
  raw <- raw[nzchar(trimws(raw))]
  if (length(raw) == 0L) return(list(valid = TRUE, lines = empty))
  if (!all(grepl("^[^#\\s]+#[^#\\s]+#[a-z]+: \\S", raw, perl = TRUE))) {
    return(list(valid = FALSE, lines = empty))
  }
  m <- do.call(rbind, regmatches(raw, regexec("^([^#]+)#([^#]+)#([a-z]+): (.*)$", raw)))
  value  <- trimws(m[, 5])
  status <- sub(",.*$", "", value)
  detail <- ifelse(grepl(",", value, fixed = TRUE), trimws(sub("^[^,]*,", "", value)),
                   NA_character_)
  detail[status != "NO"] <- NA_character_
  list(valid = TRUE,
       lines = data.frame(package = m[, 2], node = m[, 3], stage = m[, 4],
                          status = status, detail = detail, stringsAsFactors = FALSE))
}

#' "2026-09-28 13:40" at offset "-0400" as UTC ISO-8601.
local_time_to_utc <- function(stamp, offset) {
  t <- as.POSIXct(stamp, format = "%Y-%m-%d %H:%M", tz = "UTC")
  sgn <- if (substr(offset, 1L, 1L) == "-") -1 else 1
  secs <- sgn * (as.integer(substr(offset, 2L, 3L)) * 3600 +
                   as.integer(substr(offset, 4L, 5L)) * 60)
  format(t - secs, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' The report's index.html: BioC version, generated and snapshot times in UTC,
#' and the version built for each package. Anything that does not parse is NA
#' (or no versions), never an error.
parse_report_index <- function(html) {
  out <- list(bioc_version = NA_character_, generated_at = NA_character_,
              snapshot_at = NA_character_,
              versions = setNames(character(0), character(0)))
  if (is.null(html) || length(html) != 1L || is.na(html) || !nzchar(html)) return(out)
  txt <- gsub("&nbsp;", " ", html, fixed = TRUE)
  first <- function(pattern, x) regmatches(x, regexec(pattern, x, ignore.case = TRUE))[[1L]]
  v <- first("<TITLE>[^<]*BioC ([0-9]+\\.[0-9]+)[^<]*</TITLE>", txt)
  if (length(v) == 2L) out$bioc_version <- v[2]
  stamp <- "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}) ([+-][0-9]{4})"
  g <- first(paste0("generated on ", stamp), txt)
  if (length(g) == 3L) out$generated_at <- local_time_to_utc(g[2], g[3])
  plain <- gsub("<[^>]+>", "", txt)
  s <- first(paste0("Snapshot Date/Time[^0-9]*", stamp), plain)
  if (length(s) == 3L) out$snapshot_at <- local_time_to_utc(s[2], s[3])
  hits <- regmatches(txt, gregexpr('<A href="[^"/]+/">[^<]+</A> [0-9][0-9A-Za-z.-]*', txt))[[1L]]
  if (length(hits) > 0L) {
    href <- sub('^<A href="([^"/]+)/">.*$', "\\1", hits)
    text <- sub('^<A href="[^"/]+/">([^<]+)</A>.*$', "\\1", hits)
    ver  <- sub("^.*</A> ", "", hits)
    keep <- href == text & !duplicated(href)
    out$versions <- setNames(ver[keep], href[keep])
  }
  out
}

#' release_version and devel_version from config.yaml, NA when absent.
parse_branch_versions <- function(yaml_text) {
  y <- yaml::yaml.load(yaml_text)
  one <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  c(release = one(y$release_version), devel = one(y$devel_version))
}

#' Zero-row bioc_build_reports frame, in schema order.
empty_build_reports <- function() {
  data.frame(bioc_version = character(0), repo = character(0), report_at = character(0),
             branch = character(0), snapshot_at = character(0),
             generated_at = character(0), published_at = character(0),
             status_sha256 = character(0), n_packages = integer(0),
             n_lines = integer(0), n_na = integer(0), nodes = character(0),
             read_at = character(0), outcome = character(0), stringsAsFactors = FALSE)
}

#' What to do with a report: 'unchanged' when it is not newer than the last
#' applied report of its BioC version and repo, or is that same file read again
#' (same bytes, same Last-Modified); 'skipped_floor' when it lists no packages or
#' under the floor's share of that report's; else 'applied'. A newer report with
#' the same bytes is applied, so its rows extend to the newer time.
build_report_verdict <- function(reports, bioc_version, repo, report_at,
                                 status_sha256, published_at, n_packages,
                                 floor = BUILD_HEALTH_FLOOR) {
  prior <- reports[reports$bioc_version == bioc_version & reports$repo == repo &
                     reports$outcome == "applied", , drop = FALSE]
  if (nrow(prior) > 0L) {
    last <- prior[order(prior$report_at, decreasing = TRUE)[1L], , drop = FALSE]
    same_file <- identical(status_sha256, last$status_sha256) &&
      identical(published_at, last$published_at)
    if (report_at <= last$report_at || same_file) return("unchanged")
    if (n_packages < floor * last$n_packages) return("skipped_floor")
  }
  if (n_packages == 0L) return("skipped_floor")
  "applied"
}

#' Zero-row bioc_build_status_history frame, in schema order.
empty_build_history <- function() {
  data.frame(package = character(0), bioc_version = character(0), repo = character(0),
             node = character(0), stage = character(0), episode_seq = integer(0),
             status = character(0), detail = character(0),
             first_version = character(0), last_version = character(0),
             first_seen = character(0), last_seen = character(0),
             first_seen_exact = integer(0), ended_on = character(0),
             end_reason = character(0), stringsAsFactors = FALSE)
}

#' NA-safe elementwise equality: two NAs are equal.
na_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

build_key <- function(d) paste(d$package, d$node, d$stage, sep = "\r")

#' For each node, how many reports in a row lack it, counting this one and the
#' applied reports of the same BioC version and repo before it, and the
#' report_at of the earliest of them.
node_absence <- function(reports, bioc_version, repo, report_at, nodes_now, nodes) {
  prior <- reports[reports$bioc_version == bioc_version & reports$repo == repo &
                     reports$outcome == "applied" & reports$report_at < report_at, ,
                   drop = FALSE]
  prior <- prior[order(prior$report_at, decreasing = TRUE), , drop = FALSE]
  listed <- strsplit(prior$nodes, ",", fixed = TRUE)
  absent <- integer(length(nodes)); since <- character(length(nodes))
  for (i in seq_along(nodes)) {
    if (nodes[i] %in% nodes_now) { absent[i] <- 0L; since[i] <- NA_character_; next }
    n <- 1L; s <- report_at
    for (j in seq_along(listed)) {
      if (nodes[i] %in% listed[[j]]) break
      n <- n + 1L; s <- prior$report_at[j]
    }
    absent[i] <- n; since[i] <- s
  }
  data.frame(node = nodes, absent = absent, since = since, stringsAsFactors = FALSE)
}

#' Apply one report to the episode history. An open row whose status and reason
#' still hold is extended; a different result closes it 'changed' and opens the
#' next episode; a line gone from a report whose node is present closes it
#' 'gone'. NA is no result and leaves the open row alone, as does a missing
#' node until `gone_after` reports in a row lack it. Propagation rows are only
#' touched when the propagation file was read.
#'   report: list(bioc_version, repo, report_at, versions, propagation_read)
#'   reports: bioc_build_reports rows before this report
#'   exact: first_seen_exact for the episodes opened here
apply_build_report <- function(history, lines, report, reports, exact,
                               gone_after = BUILD_NODE_GONE_AFTER) {
  bv <- report$bioc_version; rp <- report$repo; at <- report$report_at
  versions <- report$versions
  ver_of <- function(pkg) {
    v <- unname(versions[pkg])
    if (length(v) != length(pkg)) v <- rep(NA_character_, length(pkg))
    v
  }
  lines <- lines[!duplicated(build_key(lines)), , drop = FALSE]
  if (!isTRUE(report$propagation_read)) {
    lines <- lines[lines$stage != "propagate", , drop = FALSE]
  }
  counts <- c(new = 0L, extended = 0L, closed = 0L)

  scope <- is.na(history$ended_on) & history$bioc_version == bv & history$repo == rp
  if (!isTRUE(report$propagation_read)) scope <- scope & history$stage != "propagate"
  idx <- which(scope)
  m <- match(build_key(history[idx, , drop = FALSE]), build_key(lines))
  st <- lines$status[m]; dt <- lines$detail[m]
  has <- !is.na(m) & st != "NA"
  same <- has & na_eq(history$status[idx], st) & na_eq(history$detail[idx], dt)

  ext <- idx[same]
  history$last_seen[ext] <- at
  v <- ver_of(history$package[ext])
  history$last_version[ext] <- ifelse(is.na(v), history$last_version[ext], v)

  ch <- idx[has & !same]
  history$ended_on[ch] <- at
  history$end_reason[ch] <- "changed"

  status_nodes <- unique(lines$node[lines$stage != "propagate"])
  unmatched <- idx[is.na(m)]
  node_here <- history$stage[unmatched] == "propagate" |
    history$node[unmatched] %in% status_nodes
  gone <- unmatched[node_here]
  history$ended_on[gone] <- at
  history$end_reason[gone] <- "gone"

  away <- unmatched[!node_here]
  closed_away <- integer(0)
  if (length(away) > 0L) {
    ab <- node_absence(reports, bv, rp, at, status_nodes, unique(history$node[away]))
    k <- match(history$node[away], ab$node)
    shut <- ab$absent[k] >= gone_after
    closed_away <- away[shut]
    history$ended_on[closed_away] <- ab$since[k][shut]
    history$end_reason[closed_away] <- "gone"
  }

  open_now <- history[is.na(history$ended_on) & history$bioc_version == bv &
                        history$repo == rp, , drop = FALSE]
  results <- lines[lines$status != "NA", , drop = FALSE]
  to_open <- results[!(build_key(results) %in% build_key(open_now)), , drop = FALSE]
  if (nrow(to_open) > 0L) {
    scoped <- history[history$bioc_version == bv & history$repo == rp, , drop = FALSE]
    top <- if (nrow(scoped) > 0L) tapply(scoped$episode_seq, build_key(scoped), max) else integer(0)
    prev_seq <- unname(top[build_key(to_open)])
    if (length(prev_seq) != nrow(to_open)) prev_seq <- rep(NA_integer_, nrow(to_open))
    prev_seq[is.na(prev_seq)] <- 0L
    v <- ver_of(to_open$package)
    history <- rbind(history, data.frame(
      package = to_open$package, bioc_version = bv, repo = rp,
      node = to_open$node, stage = to_open$stage,
      episode_seq = as.integer(prev_seq) + 1L,
      status = to_open$status, detail = to_open$detail,
      first_version = v, last_version = v, first_seen = at, last_seen = at,
      first_seen_exact = as.integer(exact), ended_on = NA_character_,
      end_reason = NA_character_, stringsAsFactors = FALSE))
  }
  counts[["new"]] <- nrow(to_open)
  counts[["extended"]] <- length(ext)
  counts[["closed"]] <- length(ch) + length(gone) + length(closed_away)
  list(history = history, counts = counts)
}

#' Close with 'retired' the open rows of a BioC version older than every
#' version its repo's aliases serve now. served is data.frame(repo,
#' bioc_version); a repo absent from it is left alone. Comparing against the
#' oldest served version keeps a version in the middle open while the release
#' and devel aliases move one at a time at the rollover.
retire_build_versions <- function(history, served, now) {
  if (nrow(served) == 0L || nrow(history) == 0L) {
    return(list(history = history, closed = 0L))
  }
  num <- function(v) {
    u <- unique(v)
    setNames(vapply(u, release_to_numeric, numeric(1)), u)[v]
  }
  oldest <- tapply(num(served$bioc_version), served$repo, min)
  floor_of <- unname(oldest[history$repo])
  idx <- which(is.na(history$ended_on) & !is.na(floor_of) &
                 num(history$bioc_version) < floor_of)
  history$ended_on[idx] <- now
  history$end_reason[idx] <- "retired"
  list(history = history, closed = length(idx))
}

#' Publish-gate fingerprint: the newest applied report of each BioC version and
#' repo, sorted.
builds_fingerprint <- function(reports) {
  a <- reports[reports$outcome == "applied", , drop = FALSE]
  if (nrow(a) == 0L) return("")
  a <- a[order(a$report_at, decreasing = TRUE), , drop = FALSE]
  a <- a[!duplicated(paste(a$bioc_version, a$repo)), , drop = FALSE]
  paste(sort(paste(a$bioc_version, a$repo, a$report_at, sep = ":")), collapse = ",")
}
