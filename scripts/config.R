# scripts/config.R: constants for the bioconductor-metadata pipeline.
VIEWS_URLS <- c(
  software   = "https://bioconductor.org/packages/release/bioc/VIEWS",
  annotation = "https://bioconductor.org/packages/release/data/annotation/VIEWS",
  experiment = "https://bioconductor.org/packages/release/data/experiment/VIEWS",
  workflows  = "https://bioconductor.org/packages/release/workflows/VIEWS")
CONFIG_YAML_URL <- "https://bioconductor.org/config.yaml"

# Backoff between attempts at a bioconductor.org fetch, in seconds; one more
# attempt is made than there are waits. The previous 3 tries at 3s and 6s covered
# nine seconds, which a real outage walks straight through: bioconductor.org
# served 504 for at least eight minutes on 2026-07-26 and took the run with it.
# Holding the runner idle for a quarter hour is far cheaper than forfeiting the
# day's run. The first wait stays small because most failures are a one-off blip
# and DESCRIPTION fetches run once per package, so a costly first retry would
# multiply across the catalog.
RETRY_WAITS_S   <- c(5, 15, 30, 60, 120, 300, 600)

# Backoff for the fetches that run once per ITEM rather than once per run, in
# seconds. The budget above is worth waiting out for config.yaml and the four
# VIEWS files, where a failure kills the run and there is exactly one of each.
# It is the wrong trade for the per-item fetches, because it multiplies:
#
#   fetch_biocviews_dot  once per release branch; the oldest branches have no
#                        inst/dot/biocViewsVocab.dot at all (RELEASE_1_0 and
#                        RELEASE_1_5 return 404, RELEASE_1_8 onward do not), and
#                        the caller already treats failure as NULL
#   fetch_description    once per package across a ~2800 package catalog, inside
#                        a loop that logs a skip and moves on
#
# At the full budget, roughly 16 packages failing every attempt would consume
# the job's entire 300 minute timeout by themselves, so a partial upstream
# degradation would wedge the run instead of merely thinning that day's results.
# Giving up early on one item costs a day of that item's metadata, which is much
# the cheaper mistake.
ITEM_RETRY_WAITS_S <- c(5, 15, 30)
BIOC_ORG        <- "bioc"
BIOC_GIT_BASE   <- "https://github.com/bioc"          # git ls-remote <base>/<pkg>
BIOC_RAW_BASE   <- "https://raw.githubusercontent.com/bioc" # <base>/<pkg>/<branch>/DESCRIPTION
PUBLISH_REPO    <- "r-observatory/bioconductor-metadata"

# Floor for the names size gate: a live count below this is treated as a partial
# VIEWS fetch and the run reuses the prior bioc_names_all.
BIOC_LIVE_FLOOR <- 1500L
