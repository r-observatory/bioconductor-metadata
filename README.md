# bioconductor-metadata

This pipeline collects and publishes metadata for Bioconductor packages across
the software, annotation, experiment, and workflows repositories. It fetches
package-level fields from the VIEWS files and release-date information from the
Bioconductor config.yaml, then writes the aggregated data to the
`r-observatory/bioconductor-metadata` GitHub repository for downstream
consumers.
