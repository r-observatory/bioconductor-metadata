# The update workflow as written in .github/workflows/update.yml.

wf_steps <- function() {
  wf <- yaml::read_yaml(test_path("..", "..", ".github", "workflows", "update.yml"))
  wf$jobs$update$steps
}
step_names <- function(steps) vapply(steps, function(s) s$name %||% "", character(1))
step_named <- function(steps, name) {
  hit <- Filter(function(s) identical(s$name, name), steps)
  expect_length(hit, 1L)
  hit[[1L]]
}

PUBLISH_STEP <- "Publish to the \"current\" release"
ARCHIVE_STEP <- "Archive upstream files"

test_that("the archive step follows Publish and runs the archive script", {
  names <- step_names(wf_steps())
  expect_gt(match(ARCHIVE_STEP, names), match(PUBLISH_STEP, names))
  run <- step_named(wf_steps(), ARCHIVE_STEP)$run
  expect_match(run, "bash scripts/archive-upstream.sh out", fixed = TRUE)
  expect_match(run, "${GITHUB_REPOSITORY}", fixed = TRUE)
})
