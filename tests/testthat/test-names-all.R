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

test_that("bioc_names_size_ok rejects a truncated VIEWS fetch", {
  expect_true(bioc_names_size_ok(4000))
  expect_false(bioc_names_size_ok(200))
  expect_false(bioc_names_size_ok(0))
})
