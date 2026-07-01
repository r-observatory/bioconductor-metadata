# scripts/config.R: constants for the bioconductor-metadata pipeline.
VIEWS_URLS <- c(
  software   = "https://bioconductor.org/packages/release/bioc/VIEWS",
  annotation = "https://bioconductor.org/packages/release/data/annotation/VIEWS",
  experiment = "https://bioconductor.org/packages/release/data/experiment/VIEWS",
  workflows  = "https://bioconductor.org/packages/release/workflows/VIEWS")
CONFIG_YAML_URL <- "https://bioconductor.org/config.yaml"
BIOC_ORG        <- "bioc"
BIOC_GIT_BASE   <- "https://github.com/bioc"          # git ls-remote <base>/<pkg>
BIOC_RAW_BASE   <- "https://raw.githubusercontent.com/bioc" # <base>/<pkg>/<branch>/DESCRIPTION
PUBLISH_REPO    <- "r-observatory/bioconductor-metadata"
