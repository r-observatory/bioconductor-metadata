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

FINAL_STEP <- "Fail the run when any part failed"

test_that("Build, Publish, Archive and the final check run in that order, last", {
  names <- step_names(wf_steps())
  order <- match(c("Build catalog", PUBLISH_STEP, ARCHIVE_STEP, FINAL_STEP), names)
  expect_false(anyNA(order))
  expect_equal(order, sort(order))
  expect_equal(order[4], length(names))
})

test_that("the last three steps run unless the run was cancelled", {
  steps <- wf_steps()
  for (n in c(PUBLISH_STEP, ARCHIVE_STEP, FINAL_STEP)) {
    expect_equal(step_named(steps, n)$`if`, "${{ !cancelled() }}", label = n)
  }
  expect_null(step_named(steps, "Build catalog")$`if`)
})

test_that("steps carry the ids the final check reads", {
  steps <- wf_steps()
  expect_equal(step_named(steps, "Build catalog")$id, "build")
  expect_equal(step_named(steps, PUBLISH_STEP)$id, "publish")
  expect_equal(step_named(steps, ARCHIVE_STEP)$id, "archive")
  env <- step_named(steps, FINAL_STEP)$env
  expect_equal(env$BUILD, "${{ steps.build.outcome }}")
  expect_equal(env$PUBLISH, "${{ steps.publish.outcome }}")
  expect_equal(env$ARCHIVE, "${{ steps.archive.outcome }}")
})

test_that("Publish and Archive act only on a status file saying catalog_ok", {
  steps <- wf_steps()
  for (n in c(PUBLISH_STEP, ARCHIVE_STEP)) {
    run <- step_named(steps, n)$run
    expect_match(run, "jq -r '.catalog_ok' out/status.json", fixed = TRUE, label = n)
    # The check comes before anything is uploaded or pushed.
    expect_lt(regexpr(".catalog_ok", run, fixed = TRUE),
              regexpr(if (n == PUBLISH_STEP) "gh release" else "archive-upstream.sh", run,
                      fixed = TRUE))
  }
})

test_that("the final check reads status.json and fails on builds_ok false", {
  run <- step_named(wf_steps(), FINAL_STEP)$run
  expect_match(run, "out/status.json", fixed = TRUE)
  expect_match(run, "jq -r '.builds_ok'", fixed = TRUE)
  expect_match(run, "exit 1", fixed = TRUE)
})
