# scripts/helpers.R: pure helper functions for the bioconductor-metadata pipeline.

#' Map a Bioconductor release "X.Y" to a sortable number (major*1000 + minor).
release_to_numeric <- function(rel) {
  p <- as.integer(strsplit(rel, ".", fixed = TRUE)[[1]])
  if (length(p) < 2 || any(is.na(p))) return(NA_real_)
  p[1] * 1000 + p[2]
}

#' Parse the `release_dates:` block of config.yaml into a named vector of ISO
#' dates (release -> YYYY-MM-DD). Uses the yaml parser; the source dates are
#' M/D/YYYY or MM/DD/YYYY. Non-date entries are dropped.
parse_release_dates <- function(yaml_text) {
  y <- yaml::yaml.load(yaml_text)
  rd <- y$release_dates
  if (is.null(rd)) return(setNames(character(0), character(0)))
  out <- vapply(rd, function(v) {
    d <- as.Date(as.character(v), tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
    if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
  }, character(1))
  out[!is.na(out)]
}
