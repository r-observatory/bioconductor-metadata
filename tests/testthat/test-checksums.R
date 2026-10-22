# Package checksums from each release's PACKAGES files, read through a fake io
# or an injected http function, so no test touches the network.

# Source update.R if not already loaded.
if (!exists("default_io", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1]))
}

packages_fixture <- function() {
  paste(readLines(test_path("fixtures", "packages-bioc.dcf"), warn = FALSE), collapse = "\n")
}

# A PACKAGES file listing the named versions.
packages_text <- function(versions, md5 = sprintf("%032d", seq_along(versions))) {
  paste(sprintf("Package: %s\nVersion: %s\nMD5sum: %s\n", names(versions), versions, md5),
        collapse = "\n")
}

checksum_rows <- function(release, repository, versions,
                          md5 = sprintf("%032d", seq_along(versions))) {
  data.frame(release = release, repository = repository, package = names(versions),
             version = unname(versions), md5sum = md5, stringsAsFactors = FALSE)
}

# An io whose fetch_packages answers from files keyed "release/repository": a
# string is a 200 with that body, an error is thrown, a list is the response,
# and anything else is a 404. Each request is recorded in calls$keys.
fake_packages_io <- function(files, calls = new.env()) {
  calls$keys <- character(0)
  list(fetch_packages = function(release, repository) {
    key <- paste(release, repository, sep = "/")
    calls$keys <- c(calls$keys, key)
    f <- files[[key]]
    if (is.null(f)) return(list(status = 404L, body = "Not Found", last_modified = NA_character_))
    if (inherits(f, "error")) stop(f)
    if (is.character(f)) return(list(status = 200L, body = f, last_modified = NA_character_))
    f
  })
}

prior_of <- function(cs, tables = NULL) {
  manifest <- list(checksums = list(frozen = as.list(cs$frozen), missing = as.list(cs$missing)))
  if (!is.null(tables)) manifest$tables <- tables
  list(checksums = cs$rows, manifest = manifest)
}

# ---------------------------------------------------------------------------
# PACKAGES parsing
# ---------------------------------------------------------------------------

test_that("a PACKAGES file gives each package's version and MD5sum", {
  p <- parse_packages_checksums(packages_fixture())
  expect_true(p$valid)
  expect_equal(p$rows$package, c("a4", "a4Base", "ABSSeq"))
  expect_equal(p$rows$version, c("1.58.0", "1.58.0", "1.64.0"))
  expect_equal(p$rows$md5sum, c("3c2a0b1f6d3e4a5b6c7d8e9f0a1b2c3d",
                                "0f1e2d3c4b5a69788796a5b4c3d2e1f0",
                                "9a8b7c6d5e4f30211203f4e5d6c7b8a9"))
  expect_equal(p$entries, 6L)
  # abseqR has no MD5sum and aCGH no Version; a4 is listed twice.
  expect_equal(p$incomplete, 2L)
  expect_equal(p$repeated, "a4")
})

test_that("an empty PACKAGES file is read as no packages", {
  for (text in c("", "\n\n")) {
    p <- parse_packages_checksums(text)
    expect_true(p$valid)
    expect_equal(nrow(p$rows), 0L)
  }
})

test_that("a body that is not a PACKAGES file is refused", {
  bodies <- list("<html><head><title>504 Gateway Time-out</title></head></html>",
                 '{"message": "Service Unavailable"}',
                 "Title: no package here\nVersion: 1.0\n",
                 NA_character_, NULL, 1L, c("Package: a", "Package: b"))
  for (b in bodies) expect_false(parse_packages_checksums(b)$valid)
})

# ---------------------------------------------------------------------------
# Which releases and repositories are read
# ---------------------------------------------------------------------------

test_that("the live releases come first, then the frozen ones newest first", {
  p <- checksum_pairs(c("3.9", "3.10", "3.22", "3.23"), live = c("3.23", "3.24"))
  expect_equal(unique(p$release), c("3.24", "3.23", "3.22", "3.10", "3.9"))
  expect_equal(p$repository[1:4], c("bioc", "data/annotation", "data/experiment", "workflows"))
  expect_equal(p$key[2], "3.24/data/annotation")
  expect_equal(p$live, rep(c(TRUE, FALSE), c(8L, 12L)))
  # A devel version config.yaml did not give is left out.
  expect_equal(unique(checksum_pairs(c("3.22", "3.23"), live = c("3.23", NA))$release),
               c("3.23", "3.22"))
})

test_that("a repository a release does not have is skipped and noted, not an error", {
  io <- fake_packages_io(list("3.23/bioc" = packages_text(c(PkgSoft = "1.2.0")),
                              "1.0/bioc" = packages_text(c(Biobase = "1.0.0"))))
  msgs <- capture_messages(cs <- read_checksum_state(io, list(), c("1.0", "3.23"), live = "3.23"))

  expect_equal(cs$failed, character(0))
  expect_setequal(cs$missing, c(paste0("3.23/", CHECKSUM_REPOS[-1]), paste0("1.0/", CHECKSUM_REPOS[-1])))
  expect_equal(cs$rows$package, c("Biobase", "PkgSoft"))
  expect_equal(cs$rows$repository, c("bioc", "bioc"))
  expect_equal(cs$releases_read, 2L)
  expect_equal(cs$repositories_read, 2L)
  # A frozen repository that is not there counts as read, so it is not asked for again.
  expect_setequal(cs$frozen, paste0("1.0/", CHECKSUM_REPOS))
  expect_true(any(grepl("Package checksums 1.0/workflows: no PACKAGES file (HTTP 404)", msgs,
                        fixed = TRUE)))
})

test_that("a frozen release is read once and then kept without another request", {
  calls <- new.env()
  files <- list("3.22/bioc" = packages_text(c(PkgOld = "0.9.0")),
                "3.23/bioc" = packages_text(c(PkgSoft = "1.2.0")))
  first <- suppressMessages(
    read_checksum_state(fake_packages_io(files, calls), list(), c("3.22", "3.23"), live = "3.23"))
  expect_true("3.22/bioc" %in% calls$keys)
  expect_setequal(first$frozen, paste0("3.22/", CHECKSUM_REPOS))

  second <- suppressMessages(
    read_checksum_state(fake_packages_io(files, calls), prior_of(first), c("3.22", "3.23"),
                        live = "3.23"))
  expect_equal(calls$keys, paste0("3.23/", CHECKSUM_REPOS))
  expect_equal(second$rows, first$rows)
  expect_equal(second$frozen, first$frozen)
  expect_equal(second$missing, first$missing)
  expect_equal(second$repositories_read, 1L)
})

test_that("the current release and devel are read again and their rows replaced", {
  prior <- list(rows = rbind(checksum_rows("3.23", "bioc", c(PkgSoft = "1.2.0", PkgGone = "2.0.0")),
                             checksum_rows("3.24", "bioc", c(PkgSoft = "1.3.0"))),
                frozen = character(0), missing = character(0))
  files <- list("3.23/bioc" = packages_text(c(PkgSoft = "1.2.1"), md5 = strrep("b", 32)),
                "3.24/bioc" = packages_text(c(PkgSoft = "1.3.1", PkgNew = "0.1.0")))
  calls <- new.env()
  cs <- suppressMessages(read_checksum_state(fake_packages_io(files, calls), prior_of(prior),
                                             "3.23", live = c("3.23", "3.24")))

  expect_equal(calls$keys, c(paste0("3.24/", CHECKSUM_REPOS), paste0("3.23/", CHECKSUM_REPOS)))
  current <- cs$rows[cs$rows$release == "3.23", ]
  expect_equal(current$package, "PkgSoft")
  expect_equal(current$version, "1.2.1")
  expect_equal(current$md5sum, strrep("b", 32))
  expect_equal(cs$rows$version[cs$rows$release == "3.24"], c("0.1.0", "1.3.1"))
  # Live releases are never marked as read for good.
  expect_equal(cs$frozen, character(0))
  expect_equal(cs$releases_read, 2L)
})

test_that("a failed read keeps the repository's prior rows and is tried again next run", {
  prior <- list(rows = checksum_rows("3.23", "bioc", c(PkgSoft = "1.2.0")),
                frozen = character(0), missing = character(0))
  files <- list(
    "3.23/bioc" = simpleError("HTTP 503 for https://bioconductor.org/packages/3.23/bioc/src/contrib/PACKAGES"),
    "3.23/workflows" = "<html>502 Bad Gateway</html>",
    "3.22/bioc" = list(status = 403L, body = "Forbidden", last_modified = NA_character_),
    "3.22/workflows" = simpleError("Timeout was reached"))
  msgs <- capture_messages(cs <- read_checksum_state(fake_packages_io(files), prior_of(prior),
                                                     c("3.22", "3.23"), live = "3.23"))

  expect_equal(cs$rows, prior$rows)
  expect_setequal(cs$failed, c("3.23/bioc", "3.23/workflows", "3.22/bioc", "3.22/workflows"))
  expect_setequal(cs$frozen, c("3.22/data/annotation", "3.22/data/experiment"))
  expect_true(any(grepl("Package checksums 3.22/workflows: Timeout was reached; keeping its prior rows",
                        msgs, fixed = TRUE)))
  expect_true(any(grepl("Package checksums 3.23/workflows: not a PACKAGES file", msgs, fixed = TRUE)))
  expect_true(any(grepl("Package checksums 3.22/bioc: HTTP 403", msgs, fixed = TRUE)))
})

test_that("reading stops after three failures in a row and leaves the rest for the next run", {
  calls <- new.env()
  io <- fake_packages_io(list(), calls)
  io$fetch_packages <- function(release, repository) {
    calls$keys <- c(calls$keys, paste(release, repository, sep = "/"))
    stop("HTTP 504 for PACKAGES")
  }
  msgs <- capture_messages(cs <- read_checksum_state(io, list(), c("3.21", "3.22", "3.23"),
                                                     live = "3.23"))

  expect_length(calls$keys, CHECKSUM_FAIL_STREAK)
  expect_equal(cs$failed, paste0("3.23/", CHECKSUM_REPOS[1:3]))
  expect_length(cs$deferred, 12L - CHECKSUM_FAIL_STREAK)
  expect_equal(cs$frozen, character(0))
  expect_true(any(grepl("Package checksums: 9 repositories left for the next run after 3 failures in a row",
                        msgs, fixed = TRUE)))
})

test_that("a read between failures starts the count again", {
  files <- list("3.23/bioc" = simpleError("HTTP 504"), "3.23/data/annotation" = simpleError("HTTP 504"),
                "3.23/workflows" = simpleError("HTTP 504"), "3.22/bioc" = simpleError("HTTP 504"))
  cs <- suppressMessages(read_checksum_state(fake_packages_io(files), list(), c("3.22", "3.23"),
                                             live = "3.23"))
  expect_length(cs$failed, 4L)
  expect_length(cs$deferred, 0L)
})

test_that("reading stops once the time budget is spent", {
  clock <- new.env()
  clock$t <- as.POSIXct("2026-10-22 06:00:00", tz = "UTC")
  tick <- function() {
    clock$t <- clock$t + 250
    clock$t
  }
  calls <- new.env()
  msgs <- capture_messages(cs <- read_checksum_state(
    fake_packages_io(list(), calls), list(), c("3.22", "3.23"), live = "3.23",
    clock = tick, budget_s = 600))

  expect_length(calls$keys, 2L)
  expect_length(cs$deferred, 6L)
  expect_true(any(grepl("Package checksums: 6 repositories left for the next run after 600 seconds of reading",
                        msgs, fixed = TRUE)))
})

test_that("a prior table holding fewer rows than its manifest lists is read again in full", {
  prior <- list(rows = checksum_rows("3.22", "bioc", c(PkgOld = "0.9.0")),
                frozen = paste0("3.22/", CHECKSUM_REPOS), missing = "3.22/workflows")
  calls <- new.env()
  msgs <- capture_messages(read_checksum_state(
    fake_packages_io(list(), calls), prior_of(prior, tables = list(bioc_package_checksums = 5L)),
    c("3.22", "3.23"), live = "3.23"))
  expect_true(all(paste0("3.22/", CHECKSUM_REPOS) %in% calls$keys))
  expect_true(any(grepl("Prior catalog holds 1 checksum rows against 5 in its manifest; reading every release again",
                        msgs, fixed = TRUE)))

  suppressMessages(read_checksum_state(
    fake_packages_io(list(), calls), prior_of(prior, tables = list(bioc_package_checksums = 1L)),
    c("3.22", "3.23"), live = "3.23"))
  expect_equal(calls$keys, paste0("3.23/", CHECKSUM_REPOS))
})

test_that("the checksum fingerprint follows the rows and the frozen list, not their order", {
  rows <- rbind(checksum_rows("3.22", "bioc", c(PkgOld = "0.9.0")),
                checksum_rows("3.23", "bioc", c(PkgSoft = "1.2.0")))
  expect_equal(checksums_fingerprint(empty_bioc_checksums(), character(0)), "")
  fp <- checksums_fingerprint(rows, "3.22/bioc")
  expect_match(fp, "^[0-9a-f]{64}$")
  expect_equal(checksums_fingerprint(rows[2:1, ], "3.22/bioc"), fp)
  changed <- rows
  changed$md5sum[2] <- strrep("f", 32)
  expect_false(checksums_fingerprint(changed, "3.22/bioc") == fp)
  expect_false(checksums_fingerprint(rows, c("3.22/bioc", "3.22/workflows")) == fp)
})

# ---------------------------------------------------------------------------
# default_io fetch_packages through an injected http function
# ---------------------------------------------------------------------------

# An http function answering from a queue of responses, recording each URL and
# timeout in calls.
queued_http <- function(responses, calls = new.env()) {
  calls$urls <- character(0)
  calls$timeouts <- numeric(0)
  function(url, timeout = NA_real_) {
    calls$urls <- c(calls$urls, url)
    calls$timeouts <- c(calls$timeouts, timeout)
    responses[[min(length(calls$urls), length(responses))]]
  }
}
response <- function(status, body = "") {
  list(status = status, body = body, last_modified = NA_character_)
}
recorded_sleep <- function(env) {
  env$slept <- numeric(0)
  function(s) env$slept <- c(env$slept, s)
}

test_that("fetch_packages reads a release repository's PACKAGES on the short timeout", {
  calls <- new.env()
  slept <- new.env()
  io <- default_io(sleep = recorded_sleep(slept),
                   http = queued_http(list(response(200L, packages_fixture())), calls))
  r <- io$fetch_packages("3.23", "data/annotation")
  expect_equal(r$status, 200L)
  expect_equal(calls$urls, "https://bioconductor.org/packages/3.23/data/annotation/src/contrib/PACKAGES")
  expect_equal(calls$timeouts, CHECKSUM_HTTP_TIMEOUT_S)
  expect_length(slept$slept, 0L)
})

test_that("fetch_packages returns a 404 at once", {
  slept <- new.env()
  io <- default_io(sleep = recorded_sleep(slept), http = queued_http(list(response(404L, "Not Found"))))
  expect_equal(io$fetch_packages("1.0", "workflows")$status, 404L)
  expect_length(slept$slept, 0L)
})

test_that("fetch_packages retries a 502 and an error page on the item budget", {
  slept <- new.env()
  io <- default_io(sleep = recorded_sleep(slept),
                   http = queued_http(list(response(502L), response(200L, "<html>Bad Gateway</html>"),
                                           response(200L, packages_fixture()))))
  r <- io$fetch_packages("3.23", "bioc")
  expect_equal(r$status, 200L)
  expect_length(slept$slept, 2L)

  io <- default_io(sleep = recorded_sleep(slept), http = queued_http(list(response(503L))))
  expect_error(io$fetch_packages("3.23", "bioc"),
               "HTTP 503 for https://bioconductor.org/packages/3.23/bioc/src/contrib/PACKAGES",
               fixed = TRUE)
  expect_length(slept$slept, length(ITEM_RETRY_WAITS_S))
})
