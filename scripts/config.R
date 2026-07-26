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

# The oldest biocViews release branches have no inst/dot/biocViewsVocab.dot at
# all (RELEASE_1_0 and RELEASE_1_5 return 404), and fetch_biocviews_dot already
# treats failure as NULL. Sitting out the full outage budget on a 404 that can
# never succeed would add tens of minutes to a cold run for nothing, so that one
# fetch retries briefly and moves on.
DOT_RETRY_WAITS_S <- c(5, 15)
BIOC_ORG        <- "bioc"
BIOC_GIT_BASE   <- "https://github.com/bioc"          # git ls-remote <base>/<pkg>
BIOC_RAW_BASE   <- "https://raw.githubusercontent.com/bioc" # <base>/<pkg>/<branch>/DESCRIPTION
PUBLISH_REPO    <- "r-observatory/bioconductor-metadata"

# Floor for the names size gate: a live count below this is treated as a partial
# VIEWS fetch and the run reuses the prior bioc_names_all.
BIOC_LIVE_FLOOR <- 1500L
