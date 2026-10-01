# scripts/publish.R against a fake gh on PATH that keeps a release in a
# directory and logs every call.

if (!exists("publish_release", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "publish.R"),
    file.path(getwd(), "..", "..", "scripts", "publish.R")
  )
  .pub <- .candidates[file.exists(.candidates)]
  if (length(.pub)) source(normalizePath(.pub[1]))
}

PUB_REPO <- "o/r"
PUB_DB   <- "bioconductor-metadata.db"
PUB_MAN  <- "manifest.json"
PUB_DB_TEMP  <- "incoming-bioconductor-metadata.db.part"
PUB_MAN_TEMP <- "incoming-manifest.json.part"

# The fake release: assets/<id> holds an asset's bytes and names/<id> its name.
# An operation listed in `fail` exits 1: "release", "list", "upload", "DELETE",
# "PATCH". "upload-short" stores other bytes and exits 0, "PATCH-noop" exits 0
# without renaming, "PATCH-lost" renames and exits 1.
FAKE_GH <- c(
  "#!/bin/sh",
  "d=\"$FAKE_GH_DIR\"",
  "printf '%s\\n' \"$*\" >> \"$d/calls.log\"",
  "fails() { [ -f \"$d/fail\" ] && grep -qx \"$1\" \"$d/fail\"; }",
  "taken() { cat \"$d\"/names/* 2>/dev/null | grep -qx \"$1\"; }",
  "sha() {",
  "  if command -v sha256sum >/dev/null 2>&1; then sha256sum \"$1\" | cut -d' ' -f1",
  "  else shasum -a 256 \"$1\" | cut -d' ' -f1; fi",
  "}",
  "case \"$1 $2\" in",
  "  'api repos/'*'/releases/tags/current')",
  "    if fails release; then exit 1; fi",
  "    echo 77; exit 0 ;;",
  "  'api --paginate')",
  "    if fails list; then exit 1; fi",
  "    for f in \"$d\"/assets/*; do",
  "      [ -f \"$f\" ] || continue",
  "      id=$(basename \"$f\")",
  "      printf '%s\\t%s\\t%s\\t%s\\tsha256:%s\\n' \"$id\" \"$(cat \"$d/names/$id\")\" \\",
  "        \"$(wc -c < \"$f\" | tr -d ' ')\" \"$(cat \"$d/states/$id\")\" \"$(sha \"$f\")\"",
  "    done",
  "    exit 0 ;;",
  "  'release upload')",
  "    [ \"$5 $6\" = \"--repo $FAKE_GH_REPO\" ] || exit 1",
  "    if fails upload; then exit 1; fi",
  "    name=$(basename \"$4\")",
  "    if taken \"$name\"; then exit 1; fi",
  "    id=$(cat \"$d/next_id\"); echo $((id + 1)) > \"$d/next_id\"",
  "    if fails upload-short; then echo cut > \"$d/assets/$id\"; else cp \"$4\" \"$d/assets/$id\"; fi",
  "    printf '%s\\n' \"$name\" > \"$d/names/$id\"; echo uploaded > \"$d/states/$id\"",
  "    exit 0 ;;",
  "  'api -X')",
  "    id=$(basename \"$4\")",
  "    [ -f \"$d/assets/$id\" ] || exit 1",
  "    if [ \"$3\" = DELETE ]; then",
  "      if fails DELETE; then exit 1; fi",
  "      rm \"$d/assets/$id\" \"$d/names/$id\" \"$d/states/$id\"; exit 0",
  "    fi",
  "    if [ \"$3\" = PATCH ]; then",
  "      if fails PATCH; then exit 1; fi",
  "      if fails PATCH-noop; then exit 0; fi",
  "      new=${6#name=}",
  "      if taken \"$new\"; then exit 1; fi",
  "      printf '%s\\n' \"$new\" > \"$d/names/$id\"",
  "      if fails PATCH-lost; then exit 1; fi",
  "      exit 0",
  "    fi ;;",
  "esac",
  "echo \"fake gh: unexpected call: $*\" >&2",
  "exit 2")

# Puts the fake gh on PATH over a release holding `assets` (name = text), ids
# from 501 in the order given. `states` overrides an asset's "uploaded".
local_fake_gh <- function(assets = list(), fail = character(0), states = list(),
                          env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  bin <- file.path(dir, "bin")
  for (sub in c("bin", "assets", "names", "states")) dir.create(file.path(dir, sub))
  writeLines(FAKE_GH, file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "0755")
  id <- 500L
  for (n in names(assets)) {
    id <- id + 1L
    writeBin(charToRaw(assets[[n]]), file.path(dir, "assets", id))
    writeLines(n, file.path(dir, "names", id))
    writeLines(states[[n]] %||% "uploaded", file.path(dir, "states", id))
  }
  writeLines(as.character(id + 1L), file.path(dir, "next_id"))
  if (length(fail) > 0L) writeLines(fail, file.path(dir, "fail"))
  file.create(file.path(dir, "calls.log"))
  withr::local_envvar(FAKE_GH_DIR = dir, FAKE_GH_REPO = PUB_REPO, .local_envir = env)
  withr::local_path(bin, action = "prefix", .local_envir = env)
  ids <- function() list.files(file.path(dir, "assets"))
  name_of <- function(i) vapply(i, function(x) readLines(file.path(dir, "names", x)), "")
  list(
    calls = function() readLines(file.path(dir, "calls.log")),
    names = function() unname(name_of(ids())),
    id = function(name) { i <- ids(); i[name_of(i) == name] },
    text = function(name) {
      i <- ids()
      f <- file.path(dir, "assets", i[name_of(i) == name])
      rawToChar(readBin(f, "raw", file.size(f)))
    })
}

# An out dir holding this run's database and manifest.
local_out <- function(env = parent.frame()) {
  out <- file.path(withr::local_tempdir(.local_envir = env), "out")
  dir.create(out)
  writeBin(charToRaw("new database"), file.path(out, PUB_DB))
  writeBin(charToRaw("{\"new\": true}"), file.path(out, PUB_MAN))
  out
}

OLD_RELEASE <- list("bioconductor-metadata.db" = "old database", "manifest.json" = "{\"old\": true}")

no_pause <- function(n) invisible(NULL)
publish <- function(out, ...) {
  msgs <- character(0)
  status <- withCallingHandlers(
    publish_release(out, repo = PUB_REPO, pause = no_pause, ...),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(status = status, log = paste(msgs, collapse = ""))
}

list_call   <- sprintf("api --paginate repos/%s/releases/77/assets?per_page=100 --jq %s",
                       PUB_REPO, PUBLISH_ASSET_JQ)
asset_api   <- function(id) sprintf("repos/%s/releases/assets/%s", PUB_REPO, id)
upload_call <- function(out, temp) {
  sprintf("release upload current %s --repo %s", file.path(out, "publish", temp), PUB_REPO)
}
delete_call <- function(id) paste("api -X DELETE", asset_api(id))
rename_call <- function(id, name) sprintf("api -X PATCH %s -f name=%s", asset_api(id), name)
writes <- function(calls) grep("^release upload | -X (DELETE|PATCH) ", calls, value = TRUE)

test_that("no download pattern in use matches a temporary name", {
  expect_equal(publish_temp_name(PUB_DB), PUB_DB_TEMP)
  expect_equal(publish_temp_name(PUB_MAN), PUB_MAN_TEMP)
  patterns <- c(PUB_DB, PUB_MAN, "*.db", "*.json", "bioconductor-metadata*", "manifest*")
  for (p in patterns) {
    expect_false(any(grepl(utils::glob2rx(p), c(PUB_DB_TEMP, PUB_MAN_TEMP))), label = p)
  }
})

test_that("each file is uploaded and verified before the old asset is deleted and the upload renamed", {
  gh <- local_fake_gh(OLD_RELEASE)
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 0L)
  expect_identical(gh$calls(), c(
    sprintf("api repos/%s/releases/tags/current --jq .id", PUB_REPO),
    list_call,
    upload_call(out, PUB_DB_TEMP),
    list_call,
    delete_call(501),
    rename_call(503, PUB_DB),
    list_call,
    upload_call(out, PUB_MAN_TEMP),
    list_call,
    delete_call(502),
    rename_call(504, PUB_MAN),
    list_call))
  expect_setequal(gh$names(), c(PUB_DB, PUB_MAN))
  expect_equal(gh$id(PUB_DB), "503")
  expect_equal(gh$text(PUB_DB), "new database")
  expect_equal(gh$text(PUB_MAN), "{\"new\": true}")
  expect_false(dir.exists(file.path(out, "publish")))
})

test_that("a release with no assets yet gets both files with nothing deleted", {
  gh <- local_fake_gh()
  out <- local_out()
  expect_identical(publish(out)$status, 0L)
  expect_identical(writes(gh$calls()), c(
    upload_call(out, PUB_DB_TEMP), rename_call(501, PUB_DB),
    upload_call(out, PUB_MAN_TEMP), rename_call(502, PUB_MAN)))
  expect_equal(gh$text(PUB_DB), "new database")
})

test_that("a failed upload deletes nothing", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "upload")
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_identical(writes(gh$calls()), upload_call(out, PUB_DB_TEMP))
  expect_equal(gh$id(PUB_DB), "501")
  expect_equal(gh$text(PUB_DB), "old database")
  expect_match(r$log, "bioconductor-metadata.db was not touched", fixed = TRUE)
})

test_that("an upload that does not match the local file deletes nothing", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "upload-short")
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_identical(writes(gh$calls()), upload_call(out, PUB_DB_TEMP))
  expect_equal(gh$text(PUB_DB), "old database")
  expect_match(r$log, "nothing was deleted", fixed = TRUE)
})

test_that("a failed rename after the delete leaves the upload on the release and gives the recovery command", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "PATCH")
  out <- local_out()
  summary <- file.path(dirname(out), "summary.md")
  r <- publish(out, summary_path = summary)
  expect_identical(r$status, 1L)
  w <- writes(gh$calls())
  # Upload, delete, then the rename tried three times; the manifest is not begun.
  expect_identical(w, c(upload_call(out, PUB_DB_TEMP), delete_call(501),
                        rep(rename_call(503, PUB_DB), 3L)))
  expect_setequal(gh$names(), c(PUB_DB_TEMP, PUB_MAN))
  expect_equal(gh$text(PUB_DB_TEMP), "new database")
  expect_equal(gh$text(PUB_MAN), "{\"old\": true}")
  fix <- sprintf("gh api -X PATCH %s -f name=%s", asset_api(503), PUB_DB)
  expect_match(r$log, fix, fixed = TRUE)
  expect_match(r$log, "holds the full file", fixed = TRUE)
  expect_true(any(grepl(fix, readLines(summary), fixed = TRUE)))
})

test_that("a rename the release does not show gives the recovery command", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "PATCH-noop")
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_setequal(gh$names(), c(PUB_DB_TEMP, PUB_MAN))
  expect_match(r$log, sprintf("gh api -X PATCH %s -f name=%s", asset_api(503), PUB_DB),
               fixed = TRUE)
})

test_that("a rename reported as failed that the release shows as done is a finished swap", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "PATCH-lost")
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 0L)
  expect_setequal(gh$names(), c(PUB_DB, PUB_MAN))
  expect_equal(gh$text(PUB_DB), "new database")
  expect_equal(gh$text(PUB_MAN), "{\"new\": true}")
})

test_that("a failed delete leaves the upload on the release and names both commands", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "DELETE")
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_identical(writes(gh$calls()), c(upload_call(out, PUB_DB_TEMP), delete_call(501)))
  expect_setequal(gh$names(), c(PUB_DB, PUB_MAN, PUB_DB_TEMP))
  expect_equal(gh$text(PUB_DB), "old database")
  expect_match(r$log, paste("gh api -X DELETE", asset_api(501)), fixed = TRUE)
  expect_match(r$log, sprintf("gh api -X PATCH %s -f name=%s", asset_api(503), PUB_DB),
               fixed = TRUE)
})

test_that("an upload left beside its final asset is removed before the new upload", {
  gh <- local_fake_gh(c(OLD_RELEASE, list("incoming-bioconductor-metadata.db.part" = "stale")))
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 0L)
  expect_identical(writes(gh$calls())[1:4], c(
    delete_call(503), upload_call(out, PUB_DB_TEMP), delete_call(501), rename_call(504, PUB_DB)))
  expect_setequal(gh$names(), c(PUB_DB, PUB_MAN))
  expect_equal(gh$text(PUB_DB), "new database")
})

test_that("an upload left without its final asset stops the run before any write", {
  gh <- local_fake_gh(list("manifest.json" = "{\"old\": true}",
                           "incoming-bioconductor-metadata.db.part" = "only copy"))
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_length(writes(gh$calls()), 0L)
  expect_equal(gh$text(PUB_DB_TEMP), "only copy")
  expect_match(r$log, "Nothing was changed", fixed = TRUE)
  expect_match(r$log, sprintf("gh api -X PATCH %s -f name=%s", asset_api(502), PUB_DB),
               fixed = TRUE)
})

test_that("a half made upload left without its final asset is never offered as the file", {
  gh <- local_fake_gh(list("incoming-manifest.json.part" = "{"),
                      states = list("incoming-manifest.json.part" = "starter"))
  out <- local_out()
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_length(writes(gh$calls()), 0L)
  expect_false(grepl("-X PATCH", r$log, fixed = TRUE))
  expect_match(r$log, paste("gh api -X DELETE", asset_api(501)), fixed = TRUE)
})

test_that("a release whose assets cannot be listed is left alone", {
  gh <- local_fake_gh(OLD_RELEASE, fail = "list")
  r <- publish(local_out())
  expect_identical(r$status, 1L)
  expect_length(writes(gh$calls()), 0L)
  expect_match(r$log, "could not list the release assets", fixed = TRUE)
})

test_that("a missing output file or repository stops the run before gh is called", {
  gh <- local_fake_gh(OLD_RELEASE)
  out <- local_out()
  expect_identical(publish_release(out, repo = "", pause = no_pause) |> suppressMessages(), 1L)
  unlink(file.path(out, PUB_MAN))
  r <- publish(out)
  expect_identical(r$status, 1L)
  expect_match(r$log, "manifest.json", fixed = TRUE)
  expect_length(gh$calls(), 0L)
})

test_that("gh_cli passes each argument through intact and reports the exit status", {
  bin <- withr::local_tempdir()
  writeLines(c("#!/bin/sh", "for a in \"$@\"; do printf '%s\\n' \"$a\"; done",
               "exit ${FAKE_GH_STATUS:-0}"), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "0755")
  withr::local_path(bin, action = "prefix")
  args <- c("api", "--paginate", "repos/x/y/releases/1/assets?per_page=100",
            "--jq", PUBLISH_ASSET_JQ)
  r <- gh_cli(args)
  expect_identical(r$status, 0L)
  expect_identical(r$out, args)
  withr::local_envvar(FAKE_GH_STATUS = "3")
  expect_identical(gh_cli("x")$status, 3L)
})

# The script as the workflow runs it, from the repository root.
run_publish_script <- function(out, repo = PUB_REPO) {
  root <- normalizePath(test_path("..", ".."))
  withr::local_dir(root)
  withr::local_envvar(GH_REPO = repo, GITHUB_STEP_SUMMARY = "")
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("scripts/publish.R", shQuote(out)),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(res, "status") %||% 0L, output = paste(res, collapse = "\n"))
}

test_that("the script exits 0 once both files are in place", {
  gh <- local_fake_gh(OLD_RELEASE)
  r <- run_publish_script(local_out())
  expect_identical(r$status, 0L)
  expect_equal(gh$text(PUB_DB), "new database")
  expect_equal(gh$text(PUB_MAN), "{\"new\": true}")
})

test_that("the script exits 1 and prints the recovery command when it stops", {
  gh <- local_fake_gh(list("incoming-bioconductor-metadata.db.part" = "only copy"))
  r <- run_publish_script(local_out())
  expect_identical(r$status, 1L)
  expect_match(r$output, sprintf("gh api -X PATCH %s -f name=%s", asset_api(501), PUB_DB),
               fixed = TRUE)
  expect_identical(run_publish_script(local_out(), repo = "")$status, 1L)
})
