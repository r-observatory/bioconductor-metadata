# scripts/helpers.R: pure helper functions for the bioconductor-metadata pipeline.

#' Map a Bioconductor release "X.Y" to a sortable number (major*1000 + minor).
release_to_numeric <- function(rel) {
  p <- as.integer(strsplit(rel, ".", fixed = TRUE)[[1]])
  if (length(p) < 2 || any(is.na(p))) return(NA_real_)
  p[1] * 1000 + p[2]
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
