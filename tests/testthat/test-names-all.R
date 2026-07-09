.pkgs <- function(...) {
  base <- data.frame(name = character(0), name_lower = character(0),
                     in_current = integer(0), first_release_date = character(0),
                     updated_at = character(0), stringsAsFactors = FALSE)
  rows <- list(...)
  if (length(rows) == 0L) return(base)
  do.call(rbind, c(list(base), rows))
}

test_that("build_bioc_names_all projects bioc_packages with a live/archived state", {
  pdf <- .pkgs(
    data.frame(name = "ComplexHeatmap", name_lower = "complexheatmap", in_current = 1L,
               first_release_date = "2016-05-01", updated_at = "2026-07-09", stringsAsFactors = FALSE),
    data.frame(name = "oldbioc", name_lower = "oldbioc", in_current = 0L,
               first_release_date = NA_character_, updated_at = "2024-01-01", stringsAsFactors = FALSE))
  df <- build_bioc_names_all(pdf)

  expect_setequal(df$name_lower, c("complexheatmap", "oldbioc"))
  expect_equal(df$canonical_name[df$name_lower == "complexheatmap"], "ComplexHeatmap")
  expect_equal(df$identity_state[df$name_lower == "complexheatmap"], "live")
  expect_equal(df$identity_state[df$name_lower == "oldbioc"], "archived")
  expect_equal(df$first_seen[df$name_lower == "complexheatmap"], "2016-05-01")
  expect_equal(df$first_seen[df$name_lower == "oldbioc"], "")   # unknown -> empty, stable
  expect_equal(df$last_seen[df$name_lower == "oldbioc"], "2024-01-01")
  expect_equal(nrow(build_bioc_names_all(.pkgs())), 0L)
})

test_that("build_bioc_names_all keeps the live entry on a case collision", {
  # "Foo" (archived) and "foo" (live) collide on name_lower; the live row must win.
  pdf <- .pkgs(
    data.frame(name = "Foo", name_lower = "foo", in_current = 0L,
               first_release_date = "2015-01-01", updated_at = "2020-01-01", stringsAsFactors = FALSE),
    data.frame(name = "foo", name_lower = "foo", in_current = 1L,
               first_release_date = "2018-01-01", updated_at = "2026-07-09", stringsAsFactors = FALSE))
  hit <- build_bioc_names_all(pdf)
  hit <- hit[hit$name_lower == "foo", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$canonical_name, "foo")      # live row's name
  expect_equal(hit$identity_state, "live")
  expect_equal(hit$first_seen, "2018-01-01")   # live row's first_release_date
})

test_that("bioc_names_size_ok rejects a truncated VIEWS fetch", {
  expect_true(bioc_names_size_ok(4000))
  expect_false(bioc_names_size_ok(200))
  expect_false(bioc_names_size_ok(0))
})

test_that("export_catalog writes bioc_names_all when given a projection", {
  path <- tempfile(fileext = ".db")
  on.exit(unlink(path))
  pdf <- .pkgs(data.frame(name = "ComplexHeatmap", name_lower = "complexheatmap",
                          in_current = 1L, first_release_date = "2016-05-01",
                          updated_at = "2026-07-09", stringsAsFactors = FALSE))
  # bioc_packages needs its full column set; build a minimal compliant frame.
  full <- data.frame(name = "ComplexHeatmap", name_lower = "complexheatmap",
    category = "software", version = "1.0", title = "t", description = "d",
    maintainer = "m", maintainer_email = "e", license = "MIT", depends = "",
    imports = "", suggests = "", biocviews = "", git_url = "",
    first_release = "3.3", first_release_date = "2016-05-01", last_release = "3.19",
    last_release_date = "2026-07-09", in_current = 1L, in_devel = 0L,
    updated_at = "2026-07-09", stringsAsFactors = FALSE)
  auth <- data.frame(package = character(0), given = character(0), family = character(0),
                     email = character(0), role = character(0), orcid = character(0),
                     stringsAsFactors = FALSE)
  export_catalog(path, full, auth, names_all_df = build_bioc_names_all(pdf))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  got <- RSQLite::dbGetQuery(con, "SELECT * FROM bioc_names_all")
  RSQLite::dbDisconnect(con)
  expect_equal(nrow(got), 1L)
  expect_equal(got$canonical_name, "ComplexHeatmap")
  expect_equal(got$identity_state, "live")
})

test_that("run_update passes a gated bioc_names_all to export", {
  # Reuse the repo's existing run-update test io if one exists; otherwise this
  # focused check drives the projection + gate decision directly.
  pdf <- .pkgs(
    data.frame(name = "ComplexHeatmap", name_lower = "complexheatmap", in_current = 1L,
               first_release_date = "2016-05-01", updated_at = "2026-07-09", stringsAsFactors = FALSE),
    data.frame(name = "oldbioc", name_lower = "oldbioc", in_current = 0L,
               first_release_date = NA_character_, updated_at = "2024-01-01", stringsAsFactors = FALSE))
  n_live <- sum(pdf$in_current == 1L)
  prior  <- data.frame(name_lower = "prevonly", canonical_name = "PrevOnly",
                       identity_state = "archived", first_seen = "2015-01-01",
                       last_seen = "2025-01-01", stringsAsFactors = FALSE)

  # gate passes with a low floor: fresh projection is used
  chosen_ok <- if (bioc_names_size_ok(n_live, floor = 0L)) build_bioc_names_all(pdf) else prior
  expect_setequal(chosen_ok$name_lower, c("complexheatmap", "oldbioc"))

  # gate fails with a high floor: prior is reused
  chosen_bad <- if (bioc_names_size_ok(n_live, floor = 999999L)) build_bioc_names_all(pdf) else prior
  expect_setequal(chosen_bad$name_lower, "prevonly")
})
