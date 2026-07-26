# with_retry: surviving a transient bioconductor.org outage.
# sleep and rand are injected, so the suite asserts the backoff schedule
# without ever waiting.
#
# with_retry takes its work as a lazy promise, which R evaluates in the caller's
# environment. That is this test block, so the attempt counters below assign with
# `<-`: a `<<-` would skip this frame and silently count in a parent environment,
# leaving the local counter at zero. The injected sleep closures are ordinary
# functions, so those correctly use `<<-`.

# Source update.R if not already loaded.
if (!exists("with_retry", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

no_sleep <- function(s) invisible(NULL)

test_that("with_retry returns the first attempt's value without sleeping", {
  slept <- numeric(0)
  calls <- 0L

  val <- with_retry({ calls <- calls + 1L; "ok" },
                    sleep = function(s) slept <<- c(slept, s))

  expect_equal(val, "ok")
  expect_equal(calls, 1L)
  expect_equal(slept, numeric(0))
})

test_that("with_retry re-evaluates the expression and returns the first success", {
  calls <- 0L

  val <- with_retry({
    calls <- calls + 1L
    if (calls < 3L) stop("HTTP status was '504 Gateway Timeout'")
    "ok"
  }, sleep = no_sleep)

  expect_equal(val, "ok")
  expect_equal(calls, 3L)
})

test_that("with_retry waits the configured backoff between attempts", {
  slept <- numeric(0)

  expect_error(
    with_retry(stop("boom"), waits = c(1, 2, 4),
               sleep = function(s) slept <<- c(slept, s), rand = function() 1),
    "boom")

  expect_equal(slept, c(1, 2, 4))
})

test_that("with_retry spreads each wait with jitter", {
  slept <- numeric(0)

  expect_error(
    with_retry(stop("boom"), waits = c(10, 20),
               sleep = function(s) slept <<- c(slept, s), rand = function() 1.2),
    "boom")

  expect_equal(slept, c(12, 24))
})

test_that("with_retry makes one more attempt than it has waits, then re-raises", {
  calls <- 0L

  expect_error(
    with_retry({
      calls <- calls + 1L
      stop("HTTP status was '504 Gateway Timeout'")
    }, waits = c(1, 2), sleep = no_sleep),
    "504 Gateway Timeout")

  expect_equal(calls, 3L)
})

test_that("the default backoff covers more than fifteen minutes", {
  expect_gt(sum(RETRY_WAITS_S), 15 * 60)
})

test_that("the first retry is cheap so a one-off blip costs seconds", {
  expect_lte(RETRY_WAITS_S[[1]], 5)
})

# The oldest biocViews release branches (RELEASE_1_0, RELEASE_1_5) genuinely have
# no inst/dot/biocViewsVocab.dot, and fetch_biocviews_dot already treats failure
# as NULL. Spending the full outage budget on a 404 that cannot succeed would add
# tens of minutes to a cold run for nothing.
test_that("the biocViews vocabulary backoff stays far below the outage budget", {
  expect_lt(sum(DOT_RETRY_WAITS_S), 60)
})
