# bioconductor-metadata

This pipeline collects and publishes metadata for Bioconductor packages across
the software, annotation, experiment, and workflows repositories. It fetches
package-level fields from the VIEWS files and release-date information from the
Bioconductor config.yaml, then writes the aggregated data to the
`r-observatory/bioconductor-metadata` GitHub repository for downstream
consumers.

## Build reports and VIEWS history

Bioconductor keeps only the latest build report of each build and overwrites VIEWS in place, so each daily run reads the release and devel reports (software, data-experiment and workflows) and keeps what they and VIEWS say as episodes.

- `bioc_build_reports`: one row per report read, keyed by BioC version, repository and report time (the report's snapshot time, else the status file's Last-Modified), with `outcome` `applied` or `skipped_floor`.
- `bioc_build_status_history`: one episode per package, node and stage while its status holds, with the propagation status (stage `propagate`) and the reason given with a NO. A status of NA means the node had no result that day and is never stored; a node missing from seven reports in a row closes its rows as `gone`, and a BioC version no longer served closes as `retired`.
- `bioc_views_history`: one episode per package and VIEWS field (Version, Date/Publication, PackageStatus, `source.ver`, `win.binary.ver` and every `mac.binary*.ver` field), timed by each VIEWS file's Last-Modified.

`first_seen_exact = 0` marks an episode that was already open in the first report or VIEWS file read, so its real start is earlier. `bioc_packages` also carries `package_status`, `date_publication`, `linking_to`, `enhances`, `dependency_count` and `author_text` from VIEWS.

The raw release VIEWS files and the build status and propagation files are committed to the `upstream-archive` branch of this repository whenever their bytes change. They are the files bioconductor.org publishes, maintainer email addresses included.

The run stops when the `current` release exists but its manifest or database cannot be downloaded or read, or the database holds fewer packages than its manifest counts, so the history is never restarted by accident. The `bootstrap` input of the update workflow is the way past an unreadable prior: it crawls every repository and starts the catalog and its history over.

A run that publishes replaces the database and the manifest on the `current` release one at a time: the file is uploaded under a temporary name, the upload's size and SHA-256 are checked against the local file, the old asset is deleted and the upload is renamed. A run that stops between the delete and the rename leaves the upload on the release and prints the `gh api` command that renames it.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
