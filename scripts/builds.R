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
