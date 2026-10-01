#!/usr/bin/env Rscript
# scripts/publish.R: puts this run's database and manifest on the `current`
# release. Needs helpers.R. Run: GH_REPO=owner/repo Rscript scripts/publish.R out

PUBLISH_TAG      <- "current"
# The database goes first, so the new manifest is never on the release before its database.
PUBLISH_FILES    <- c("bioconductor-metadata.db", "manifest.json")
PUBLISH_ASSET_JQ <- ".[] | [.id, .name, .size, .state, .digest] | @tsv"

# No download of this release (by exact name, or *.db) matches this name.
publish_temp_name <- function(name) paste0("incoming-", name, ".part")

#' Run gh with each argument passed intact.
#' @return list(status = exit status, out = stdout lines).
gh_cli <- function(args) {
  out <- suppressWarnings(system2("gh", shQuote(args), stdout = TRUE))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else as.integer(status),
       out = as.character(out))
}

asset_path <- function(repo, id) sprintf("repos/%s/releases/assets/%s", repo, id)
delete_cmd <- function(repo, id) sprintf("gh api -X DELETE %s", asset_path(repo, id))
rename_cmd <- function(repo, id, name) {
  sprintf("gh api -X PATCH %s -f name=%s", asset_path(repo, id), name)
}

release_id <- function(gh, repo, tag) {
  r <- gh(c("api", sprintf("repos/%s/releases/tags/%s", repo, tag), "--jq", ".id"))
  id <- trimws(r$out)
  if (!identical(r$status, 0L) || length(id) != 1L || !grepl("^[0-9]+$", id)) {
    stop(sprintf("could not read the id of release '%s'", tag))
  }
  id
}

#' Every asset on the release, half made ones included (the assets API lists
#' those; gh release view does not).
#' @return data.frame(id, name, size, state, digest).
list_release_assets <- function(gh, repo, rid) {
  r <- gh(c("api", "--paginate",
            sprintf("repos/%s/releases/%s/assets?per_page=100", repo, rid),
            "--jq", PUBLISH_ASSET_JQ))
  if (!identical(r$status, 0L)) stop("could not list the release assets")
  f <- strsplit(r$out[nzchar(r$out)], "\t", fixed = TRUE)
  if (any(lengths(f) < 4L)) stop("unexpected line in the release asset listing")
  # strsplit drops a trailing empty field (an asset with no digest).
  field <- function(i) vapply(f, function(x) if (length(x) >= i) x[[i]] else "", "")
  data.frame(id = field(1), name = field(2), size = as.numeric(field(3)),
             state = field(4), digest = field(5), stringsAsFactors = FALSE)
}

# Lists the release until ok(assets) holds, or gives up.
await_assets <- function(gh, repo, rid, ok, attempts, pause) {
  a <- NULL
  for (i in seq_len(attempts)) {
    if (i > 1L) pause(i - 1L)
    a <- tryCatch(list_release_assets(gh, repo, rid), error = function(e) NULL)
    if (!is.null(a) && isTRUE(ok(a))) return(list(ok = TRUE, assets = a))
  }
  list(ok = FALSE, assets = a)
}

#' Before any write: removes an upload an earlier run left beside its final
#' asset, and stops when one is on the release without its final asset.
clear_leftover_uploads <- function(gh, repo, assets, finals) {
  for (final in finals) {
    temp <- publish_temp_name(final)
    left <- assets[assets$name == temp, , drop = FALSE]
    if (nrow(left) == 0L) next
    if (!any(assets$name == final)) {
      if (identical(left$state, "uploaded")) {
        stop(sprintf(paste0("%s (asset %s) is on the release and %s is not: an earlier run stopped ",
                            "before renaming its upload. Nothing was changed. Put it in place with:\n  %s"),
                     temp, left$id, final, rename_cmd(repo, left$id, final)))
      }
      stop(sprintf(paste0("%s (asset %s) is on the release as an unfinished upload and %s is not. ",
                          "Nothing was changed. Delete it with:\n  %s"),
                   temp, left$id, final, delete_cmd(repo, left$id)))
    }
    for (id in left$id) {
      message(sprintf("removing %s (asset %s) left by an earlier run; %s is in place", temp, id, final))
      if (!identical(gh(c("api", "-X", "DELETE", asset_path(repo, id)))$status, 0L)) {
        stop(sprintf("could not remove %s (asset %s) left by an earlier run. Delete it with:\n  %s",
                     temp, id, delete_cmd(repo, id)))
      }
    }
  }
  invisible(TRUE)
}

#' Replace one asset: upload file (already named with the temporary name), check
#' its size and sha256, delete the old asset by id, rename the upload, check again.
swap_asset <- function(gh, repo, rid, tag, file, final, attempts = 3L,
                       pause = function(n) Sys.sleep(20 * n)) {
  temp <- basename(file)
  size <- file.size(file)
  digest <- paste0("sha256:", file_sha256(file))
  same <- function(a, name, id, sz, dg) {
    r <- a[a$name == name, , drop = FALSE]
    nrow(r) == 1L && (is.null(id) || identical(r$id, id)) && identical(r$state, "uploaded") &&
      isTRUE(r$size == sz) && identical(r$digest, dg)
  }

  message(sprintf("uploading %s as %s (%s bytes, %s)", final, temp,
                  format(size, scientific = FALSE), digest))
  if (!identical(gh(c("release", "upload", tag, file, "--repo", repo))$status, 0L)) {
    stop(sprintf("uploading %s failed; %s was not touched.", temp, final))
  }
  chk <- await_assets(gh, repo, rid, function(a) same(a, temp, NULL, size, digest),
                      attempts, pause)
  if (!chk$ok) {
    stop(sprintf("the release does not show %s matching the local file; nothing was deleted.", temp))
  }
  up  <- chk$assets[chk$assets$name == temp, , drop = FALSE]
  old <- chk$assets[chk$assets$name == final, , drop = FALSE]

  if (nrow(old) == 1L) {
    message(sprintf("replacing %s (asset %s) with asset %s; should this stop before the rename, recover with: %s",
                    final, old$id, up$id, rename_cmd(repo, up$id, final)))
    if (!identical(gh(c("api", "-X", "DELETE", asset_path(repo, old$id)))$status, 0L)) {
      stop(sprintf(paste0("deleting %s (asset %s) failed. This run's file stays on the release as %s ",
                          "(asset %s). If asset %s is still listed, delete it with\n  %s\n",
                          "then finish the swap with\n  %s"),
                   final, old$id, temp, up$id, old$id, delete_cmd(repo, old$id),
                   rename_cmd(repo, up$id, final)))
    }
  }
  renamed <- FALSE
  for (i in seq_len(attempts)) {
    if (i > 1L) pause(i - 1L)
    if (identical(gh(c("api", "-X", "PATCH", asset_path(repo, up$id), "-f",
                       paste0("name=", final)))$status, 0L)) {
      renamed <- TRUE
      break
    }
  }
  # The listing decides: a rename can take effect and still report a failure.
  done <- await_assets(gh, repo, rid, function(a) {
    same(a, final, up$id, size, digest) && !any(a$name == temp) && !any(a$id %in% old$id)
  }, attempts, pause)
  if (!done$ok && !renamed) {
    stop(sprintf(paste0("renaming %s (asset %s) to %s failed%s. %s is left on the release and ",
                        "holds the full file. Recover with:\n  %s"),
                 temp, up$id, final,
                 if (nrow(old) == 1L) sprintf(" after the old %s (asset %s) was deleted", final, old$id) else "",
                 temp, rename_cmd(repo, up$id, final)))
  }
  if (!done$ok) {
    stop(sprintf(paste0("after the rename the release does not show %s as asset %s (%s bytes, %s). ",
                        "Asset %s holds the full file; if it is still named %s, recover with:\n  %s"),
                 final, up$id, format(size, scientific = FALSE), digest, up$id, temp,
                 rename_cmd(repo, up$id, final)))
  }
  message(sprintf("%s is now asset %s (%s bytes, %s)", final, up$id,
                  format(size, scientific = FALSE), digest))
  invisible(up$id)
}

#' Publish out_dir's database and manifest to the release, one swap each.
#' @return 0 on success, 1 on any failure; the message and the job summary say why.
publish_release <- function(out_dir, repo, tag = PUBLISH_TAG, files = PUBLISH_FILES,
                            gh = gh_cli, attempts = 3L,
                            pause = function(n) Sys.sleep(20 * n), summary_path = "") {
  stage <- file.path(out_dir, "publish")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  tryCatch({
    if (!nzchar(repo)) stop("GH_REPO is not set")
    paths <- file.path(out_dir, files)
    if (!all(file.exists(paths))) {
      stop("nothing to publish, missing: ", paste(paths[!file.exists(paths)], collapse = " "))
    }
    rid <- release_id(gh, repo, tag)
    clear_leftover_uploads(gh, repo, list_release_assets(gh, repo, rid), files)
    unlink(stage, recursive = TRUE)
    dir.create(stage, recursive = TRUE)
    for (i in seq_along(files)) {
      staged <- file.path(stage, publish_temp_name(files[i]))
      if (!file.copy(paths[i], staged)) stop("could not stage ", paths[i])
      swap_asset(gh, repo, rid, tag, staged, files[i], attempts, pause)
    }
    0L
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("publish stopped: ", msg)
    if (nzchar(summary_path)) {
      cat("## Publish to the current release stopped", "", "```", msg, "```",
          file = summary_path, sep = "\n", append = TRUE)
    }
    1L
  })
}

if (sys.nframe() == 0L) {
  source(file.path("scripts", "helpers.R"))
  args <- commandArgs(trailingOnly = TRUE)
  quit(save = "no", status = publish_release(
    if (length(args) >= 1L) args[1L] else "out",
    repo = Sys.getenv("GH_REPO"),
    summary_path = Sys.getenv("GITHUB_STEP_SUMMARY")))
}
