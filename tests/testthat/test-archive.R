# scripts/archive-upstream.sh against a local bare repository standing in for
# the GitHub remote.

skip_if(!nzchar(Sys.which("git")), "git is not installed")

archive_script <- function() {
  normalizePath(test_path("..", "..", "scripts", "archive-upstream.sh"))
}

# A bare remote, and an out dir whose upstream/ holds the given files.
local_archive <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  bare <- file.path(tmp, "remote.git")
  system2("git", c("init", "-q", "--bare", bare))
  list(out = file.path(tmp, "out"), remote = paste0("file://", bare), bare = bare)
}
stage <- function(a, files, message = "Bioconductor files as read at now") {
  unlink(file.path(a$out, "upstream"), recursive = TRUE)
  for (p in names(files)) {
    dest <- file.path(a$out, "upstream", p)
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[p]], dest)
  }
  writeLines(message, file.path(a$out, "archive-message.txt"))
}
# from: the directory the script is called in, with a$out relative to it.
archive <- function(a, from = NULL) {
  script <- archive_script()
  run <- function() suppressWarnings(system2("bash", c(script, a$out, a$remote),
                                             stdout = TRUE, stderr = TRUE))
  out <- if (is.null(from)) run() else withr::with_dir(from, run())
  list(status = attr(out, "status") %||% 0L, output = out)
}
branch_log <- function(a) {
  suppressWarnings(system2("git", c("--git-dir", a$bare, "log", shQuote("--format=%H %P|%s"),
                                    "upstream-archive"), stdout = TRUE, stderr = TRUE))
}
branch_files <- function(a) {
  system2("git", c("--git-dir", a$bare, "ls-tree", "-r", "--name-only", "upstream-archive"),
          stdout = TRUE)
}

test_that("the first archive creates the branch as an orphan with the files at its root", {
  a <- local_archive()
  stage(a, list("3.23/views/software/VIEWS" = "Package: a",
                "3.23/builds/bioc/BUILD_STATUS_DB.txt" = "a#n1#install: OK"),
        message = "Bioconductor files as read at 2026-10-01T12:20:00Z")
  r <- archive(a)
  expect_equal(r$status, 0L)
  log <- branch_log(a)
  expect_length(log, 1L)
  # One hash and no parent: the branch shares no history with main.
  expect_match(log, "^[0-9a-f]{40} \\|Bioconductor files as read at 2026-10-01T12:20:00Z$")
  expect_setequal(branch_files(a), c("3.23/views/software/VIEWS",
                                     "3.23/builds/bioc/BUILD_STATUS_DB.txt"))
})

test_that("an out dir given relative to the calling directory is archived", {
  a <- local_archive()
  stage(a, list("3.23/views/software/VIEWS" = "Package: a"),
        message = "Bioconductor files as read at 2026-10-01T12:20:00Z")
  from <- dirname(a$out)
  a$out <- "out"
  r <- archive(a, from = from)
  expect_equal(r$status, 0L)
  expect_match(branch_log(a), "\\|Bioconductor files as read at 2026-10-01T12:20:00Z$")
})

test_that("the same bytes again make no commit", {
  a <- local_archive()
  stage(a, list("3.23/views/software/VIEWS" = "Package: a"))
  archive(a)
  r <- archive(a)
  expect_equal(r$status, 0L)
  expect_match(paste(r$output, collapse = "\n"), "unchanged since the last archive commit")
  expect_length(branch_log(a), 1L)
})

test_that("changed bytes add one commit on top, and files absent this run are kept", {
  a <- local_archive()
  stage(a, list("3.23/views/software/VIEWS" = "Package: a",
                "3.23/builds/bioc/BUILD_STATUS_DB.txt" = "a#n1#install: OK"))
  archive(a)
  first <- sub(" .*$", "", branch_log(a))
  stage(a, list("3.23/views/software/VIEWS" = "Package: a\nPackageStatus: Deprecated"))
  expect_equal(archive(a)$status, 0L)
  log <- branch_log(a)
  expect_length(log, 2L)
  expect_match(log[1], paste0("^[0-9a-f]{40} ", first, "\\|"))
  expect_true("3.23/builds/bioc/BUILD_STATUS_DB.txt" %in% branch_files(a))
  shown <- system2("git", c("--git-dir", a$bare, "show",
                            "upstream-archive:3.23/views/software/VIEWS"), stdout = TRUE)
  expect_equal(shown, c("Package: a", "PackageStatus: Deprecated"))
})

test_that("a run with no upstream files touches nothing", {
  a <- local_archive()
  dir.create(a$out)
  r <- archive(a)
  expect_equal(r$status, 0L)
  expect_match(paste(r$output, collapse = "\n"), "No upstream files this run.", fixed = TRUE)
  expect_false(is.null(attr(branch_log(a), "status")))
})

test_that("an unreachable remote fails the step", {
  a <- local_archive()
  stage(a, list("3.23/views/software/VIEWS" = "Package: a"))
  a$remote <- paste0("file://", file.path(dirname(a$bare), "missing.git"))
  expect_false(identical(archive(a)$status, 0L))
})
