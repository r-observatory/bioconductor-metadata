library(RSQLite)
library(jsonlite)

# ---------------------------------------------------------------------------
# integrity / completeness core (file_sha256 + db_integrity_core)
# ---------------------------------------------------------------------------

# Build a tiny, real catalog db on disk via the canonical export_catalog path
# so the fixture has the exact published schema (bioc_packages, bioc_authors,
# bioc_releases, bioc_view_edges, bioc_names_all).
build_catalog_db <- function(n_pkgs = 2L) {
  pkgs <- data.frame(
    name               = paste0("Pkg", seq_len(n_pkgs)),
    name_lower         = tolower(paste0("pkg", seq_len(n_pkgs))),
    category           = rep("software", n_pkgs),
    version            = rep("1.0.0", n_pkgs),
    title              = paste("Package", seq_len(n_pkgs)),
    description        = rep("Does things.", n_pkgs),
    maintainer         = rep("Alice Smith", n_pkgs),
    maintainer_email   = rep("alice@example.com", n_pkgs),
    license            = rep("MIT", n_pkgs),
    depends            = rep(NA_character_, n_pkgs),
    imports            = rep(NA_character_, n_pkgs),
    suggests           = rep(NA_character_, n_pkgs),
    biocviews          = rep("Infrastructure", n_pkgs),
    git_url            = paste0("https://git.bioconductor.org/packages/Pkg", seq_len(n_pkgs)),
    first_release      = rep("3.10", n_pkgs),
    first_release_date = rep("2018-10-31", n_pkgs),
    last_release       = rep("3.21", n_pkgs),
    last_release_date  = rep("2024-10-30", n_pkgs),
    in_current         = rep(1L, n_pkgs),
    in_devel           = rep(0L, n_pkgs),
    updated_at         = rep("2025-01-01", n_pkgs),
    stringsAsFactors   = FALSE
  )
  auths <- data.frame(
    package = pkgs$name,
    given   = rep("Alice", n_pkgs),
    family  = rep("Smith", n_pkgs),
    email   = rep("alice@example.com", n_pkgs),
    role    = rep("aut, cre", n_pkgs),
    orcid   = rep(NA_character_, n_pkgs),
    stringsAsFactors = FALSE
  )
  names_all <- build_bioc_names_all(pkgs)

  tmp <- tempfile(fileext = ".db")
  export_catalog(tmp, pkgs, auths, releases_df = NULL, view_edges_df = NULL,
                 names_all_df = names_all)
  tmp
}

test_that("file_sha256 returns lowercase 64-char hex of a file's bytes", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(as.raw(0:255), tmp)

  h <- file_sha256(tmp)
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that("db_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_catalog_db(2L)
  on.exit(unlink(db), add = TRUE)

  core <- db_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count (sqlite_% internals excluded)
  expect_equal(core$tables, list(
    bioc_authors    = 2L,
    bioc_names_all  = 2L,
    bioc_packages   = 2L,
    bioc_releases   = 0L,
    bioc_view_edges = 0L
  ))
  expect_true(core$complete)
})

test_that("db_integrity_core passes through a derived (FALSE) completeness flag", {
  db <- build_catalog_db(1L)
  on.exit(unlink(db), add = TRUE)

  core <- db_integrity_core(db, complete = FALSE)
  expect_false(core$complete)
})

test_that("db_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_catalog_db(2L)
  on.exit(unlink(db), add = TRUE)

  core <- db_integrity_core(db, complete = TRUE)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_catalog_db(3L)
  on.exit(unlink(db), add = TRUE)
  core <- db_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  manifest <- list(
    release      = "v20260714-000000",
    generated_at = "2026-07-14T00:00:00Z",
    n_packages   = 3L,
    changed      = TRUE
  )
  write_manifest(tmp, c(manifest, core))

  parsed <- jsonlite::read_json(tmp)
  # existing fields preserved
  expect_equal(parsed$release, "v20260714-000000")
  expect_equal(parsed$n_packages, 3L)
  expect_true(parsed$changed)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$bioc_packages, 3L)
  expect_true(parsed$complete)
})
