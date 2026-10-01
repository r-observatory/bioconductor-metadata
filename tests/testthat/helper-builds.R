# Shared builders for the build report tests.

# One bioc_build_reports row. The sha and Last-Modified default to values
# derived from report_at, so two rows differ unless a test says otherwise.
report_row <- function(at, nodes, n_packages = 2L, outcome = "applied",
                       bioc_version = "3.23", branch = "release", repo = "bioc",
                       published_at = at, status_sha256 = paste0("sha-", at)) {
  data.frame(bioc_version = bioc_version, repo = repo, report_at = at, branch = branch,
             snapshot_at = at, generated_at = NA_character_, published_at = published_at,
             status_sha256 = status_sha256, n_packages = as.integer(n_packages),
             n_lines = 0L, n_na = 0L, nodes = nodes, read_at = at, outcome = outcome,
             stringsAsFactors = FALSE)
}

T1 <- "2026-09-26T17:40:00Z"
T2 <- "2026-09-27T17:40:00Z"
T3 <- "2026-09-28T17:40:00Z"
