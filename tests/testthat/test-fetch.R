# VIEWS and build report fetches through an injected http function, so the
# retry and status handling is tested without the network.

# Source update.R if not already loaded.
if (!exists("http_get", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

# An http function answering from a queue of responses, recording each URL.
fake_http <- function(...) {
  queue <- list(...)
  calls <- character(0)
  f <- function(url) {
    calls <<- c(calls, url)
    queue[[min(length(calls), length(queue))]]
  }
  f
}
calls_of <- function(f) environment(f)$calls
ok_response <- function(body, lm = "Tue, 29 Sep 2026 18:14:30 GMT") {
  list(status = 200L, body = body, last_modified = http_date_to_iso(lm))
}

test_that("http_date_to_iso reads Last-Modified and refuses anything else", {
  expect_equal(http_date_to_iso("Tue, 29 Sep 2026 16:35:46 GMT"), "2026-09-29T16:35:46Z")
  expect_identical(http_date_to_iso(NULL), NA_character_)
  expect_identical(http_date_to_iso("2026-09-29"), NA_character_)
  expect_identical(http_date_to_iso("Tue, 29 Foo 2026 16:35:46 GMT"), NA_character_)
})

test_that("views_body_text matches readLines and a newline join", {
  expect_equal(views_body_text("Package: a\r\nVersion: 1\r\n"), "Package: a\nVersion: 1")
  expect_equal(views_body_text("Package: a\n\n"), "Package: a\n")
})

test_that("fetch_views returns the text with its Last-Modified, retrying a 503", {
  http <- fake_http(list(status = 503L, body = "busy", last_modified = NA_character_),
                    ok_response("Package: a\nVersion: 1\n"))
  slept <- numeric(0)
  io <- default_io(sleep = function(s) slept <<- c(slept, s), http = http)
  v <- io$fetch_views("software")
  expect_equal(as.character(v), "Package: a\nVersion: 1")
  expect_equal(attr(v, "last_modified"), "2026-09-29T18:14:30Z")
  expect_length(slept, 1L)
  expect_equal(calls_of(http), rep(VIEWS_URLS[["software"]], 2))
})

test_that("fetch_views gives up after the full outage budget", {
  slept <- numeric(0)
  io <- default_io(sleep = function(s) slept <<- c(slept, s),
                   http = fake_http(list(status = 404L, body = "", last_modified = NA_character_)))
  expect_error(io$fetch_views("workflows"), "HTTP 404", fixed = TRUE)
  expect_length(slept, length(RETRY_WAITS_S))
})

test_that("fetch_build_file returns a 404 at once and retries a 502 on the item budget", {
  slept <- numeric(0)
  gone <- fake_http(list(status = 404L, body = "Not Found", last_modified = NA_character_))
  io <- default_io(sleep = function(s) slept <<- c(slept, s), http = gone)
  r <- io$fetch_build_file("release", "workflows", "PROPAGATION_STATUS_DB.txt")
  expect_equal(r$status, 404L)
  expect_length(slept, 0L)
  expect_equal(calls_of(gone),
               "https://bioconductor.org/checkResults/release/workflows-LATEST/PROPAGATION_STATUS_DB.txt")

  busy <- fake_http(list(status = 502L, body = "", last_modified = NA_character_))
  io <- default_io(sleep = function(s) slept <<- c(slept, s), http = busy)
  expect_error(io$fetch_build_file("devel", "bioc", "BUILD_STATUS_DB.txt"), "HTTP 502")
  expect_length(slept, length(ITEM_RETRY_WAITS_S))
})

test_that("the build report fetch takes the per-item budget", {
  io <- default_io()
  expect_match(paste(deparse(io$fetch_build_file), collapse = " "),
               "ITEM_RETRY_WAITS_S", fixed = TRUE)
})
