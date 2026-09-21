# Overleaf git backend
#
# Charts reach Overleaf through its git bridge. The package keeps a clone of the
# project in a cache directory, charts are written into that clone's atlas
# folder by grattan_save_overleaf(), and this file commits and pushes them.
#
# The clone is managed by the package and never edited by hand, so there is no
# need to merge: if a push is rejected because someone edited the project in
# Overleaf, we reset the clone to the remote, put our charts back on top, and
# push again.


# Project IDs -----------------------------------------------------------------

overleaf_id_pattern <- "[0-9a-f]{24}"


#' Is this an Overleaf project URL or ID?
#'
#' @param x A character string, such as the argument given to
#'   \code{set_overleaf_project()}.
#'
#' @return \code{TRUE} if \code{x} is an Overleaf project URL (like
#'   \code{https://www.overleaf.com/project/0123456789abcdef01234567}) or a bare
#'   24-character project ID, otherwise \code{FALSE}.
#'
#' @keywords internal
is_overleaf_project <- function(x) {

  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    return(FALSE)
  }

  # A bare project ID, or any Overleaf URL containing one
  grepl(paste0("^", overleaf_id_pattern, "$"), x) ||
    (grepl("overleaf\\.com", x, ignore.case = TRUE) &&
       grepl(overleaf_id_pattern, x))
}


#' Extract the project ID from an Overleaf URL
#'
#' @param x An Overleaf project URL or bare project ID.
#'
#' @return The 24-character project ID.
#'
#' @keywords internal
overleaf_project_id <- function(x) {

  ids <- regmatches(x, gregexpr(overleaf_id_pattern, x))[[1]]

  if (length(ids) == 0) {
    stop("Could not find an Overleaf project ID in '", x, "'. ",
         "Project URLs look like ",
         "https://www.overleaf.com/project/0123456789abcdef01234567")
  }

  # Take the last match, so trailing path segments can't be mistaken for the ID
  ids[length(ids)]
}


#' The git remote for an Overleaf project
#'
#' @param id A 24-character Overleaf project ID.
#'
#' @return The HTTPS URL of the project's git remote. The username is embedded
#'   in the URL, as Overleaf requires; the token is supplied separately so it is
#'   never written to the clone's git config.
#'
#' @keywords internal
overleaf_remote_url <- function(id) {
  paste0("https://git@git.overleaf.com/", id)
}


# Tokens ----------------------------------------------------------------------

#' Store your Overleaf git authentication token
#'
#' @description Overleaf's git bridge authenticates with a personal token rather
#'   than your Overleaf password. Generate one at
#'   \url{https://www.overleaf.com/user/settings} under "Git integration", then
#'   call this function to store it.
#'
#' @param token Your Overleaf git authentication token. If \code{NULL} (the
#'   default) you will be prompted for it, so that the token isn't recorded in
#'   your console history or your script.
#' @param save Logical. Should the token be saved to your user \code{.Renviron}
#'   file, so it is available in every future R session on this computer? If
#'   \code{NULL} (the default) you will be asked.
#'
#' @details A single token works for every Overleaf project you can open, so
#'   this is a one-off setup step per computer. Tokens expire after a year, and
#'   you can have at most ten at a time.
#'
#'   Saving writes \code{OVERLEAF_TOKEN} to the \code{.Renviron} file in your
#'   home directory. Note that if your working directory contains its own
#'   \code{.Renviron}, R reads that file \emph{instead of} the one in your home
#'   directory, so the token won't be picked up; you'll be warned if that's the
#'   case.
#'
#' @return The token, invisibly.
#'
#' @seealso \code{\link{set_overleaf_project}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prompts for the token and offers to save it
#' set_overleaf_token()
#' }
set_overleaf_token <- function(token = NULL, save = NULL) {

  if (is.null(token)) {

    if (!interactive()) {
      stop("No Overleaf token supplied. Set the OVERLEAF_TOKEN environment ",
           "variable, or call set_overleaf_token() interactively.")
    }

    message("Generate a git authentication token at ",
            "https://www.overleaf.com/user/settings (under 'Git integration').")

    token <- if (requireNamespace("askpass", quietly = TRUE)) {
      askpass::askpass("Overleaf git authentication token: ")
    } else {
      readline("Overleaf git authentication token: ")
    }
  }

  token <- trimws(as.character(token))

  if (length(token) != 1 || is.na(token) || token == "") {
    stop("No Overleaf token supplied.")
  }

  Sys.setenv(OVERLEAF_TOKEN = token)

  if (is.null(save)) {
    save <- interactive() &&
      utils::menu(c("Yes", "No"),
                  title = paste("Save this token to ~/.Renviron so you don't",
                                "have to enter it again?")) == 1
  }

  if (isTRUE(save)) {
    save_overleaf_token(token)
  }

  invisible(token)
}


#' Append a token to the user's .Renviron file
#'
#' @param token The token to save.
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
save_overleaf_token <- function(token) {

  renviron <- file.path(path.expand("~"), ".Renviron")

  existing <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()

  # Drop any previous token so the file doesn't accumulate stale entries
  existing <- existing[!grepl("^OVERLEAF_TOKEN=", existing)]

  writeLines(c(existing, paste0("OVERLEAF_TOKEN=", token)), renviron)

  message("Token saved to ", renviron, ".")

  # A project-level .Renviron is read *instead of* the user-level one
  if (file.exists(".Renviron")) {
    warning("There is an .Renviron file in your working directory, which R ",
            "reads instead of the one in your home directory. Your token ",
            "won't be picked up in new sessions unless you also add ",
            "OVERLEAF_TOKEN to ", normalizePath(".Renviron"), ".")
  }

  invisible(NULL)
}


#' Get the Overleaf git authentication token
#'
#' @return The token stored in the \code{OVERLEAF_TOKEN} environment variable,
#'   prompting for it if it isn't set and the session is interactive.
#'
#' @keywords internal
overleaf_token <- function() {

  token <- Sys.getenv("OVERLEAF_TOKEN", unset = "")

  if (token != "") {
    return(token)
  }

  if (!interactive()) {
    stop("No Overleaf git token found. Set the OVERLEAF_TOKEN environment ",
         "variable, or run set_overleaf_token() to store one.")
  }

  set_overleaf_token()
}


# The local clone -------------------------------------------------------------

#' Where Overleaf projects are cached locally
#'
#' @description Syncing charts to Overleaf over git requires a local clone of
#'   the Overleaf project. Clones are kept in R's standard per-package cache
#'   directory, one folder per project, and are managed by the package: you
#'   shouldn't need to look at or edit them.
#'
#' @return Character string containing the path to the cache directory. The
#'   directory is not created until a project is cloned into it.
#'
#' @seealso \code{\link{overleaf_clear_cache}} to delete cached clones
#'
#' @export
#'
#' @examples
#' overleaf_cache_dir()
overleaf_cache_dir <- function() {

  # Mostly an escape hatch for tests
  user_set <- Sys.getenv("GRATTANTHEME_OVERLEAF_CACHE", unset = "")

  if (user_set != "") {
    return(user_set)
  }

  base <- if (getRversion() >= "4.0.0") {
    tools::R_user_dir("grattantheme", "cache")
  } else {
    file.path(path.expand("~"), ".grattantheme")
  }

  file.path(base, "overleaf")
}


#' Delete cached Overleaf clones
#'
#' @description Removes the local clones of Overleaf projects kept by
#'   \code{\link{grattan_save_overleaf}}. Anything already pushed to Overleaf is
#'   unaffected; the clone is downloaded again next time you save a chart.
#'
#' @param project An Overleaf project URL or ID. If \code{NULL} (the default),
#'   every cached project is deleted.
#'
#' @return The path that was deleted, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Delete everything
#' overleaf_clear_cache()
#'
#' # Delete a single project
#' overleaf_clear_cache("https://www.overleaf.com/project/0123456789abcdef01234567")
#' }
overleaf_clear_cache <- function(project = NULL) {

  path <- if (is.null(project)) {
    overleaf_cache_dir()
  } else {
    file.path(overleaf_cache_dir(), overleaf_project_id(project))
  }

  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    message("Deleted ", path)
  } else {
    message("Nothing cached at ", path)
  }

  invisible(path)
}


#' Check that gert is available
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
check_gert <- function() {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("Syncing charts to Overleaf over git needs the 'gert' package. ",
         "Install it with install.packages(\"gert\").")
  }
  invisible(NULL)
}


#' Make sure a local clone of an Overleaf project exists
#'
#' @param path Where the clone should live.
#' @param remote The git remote to clone from.
#' @param token The Overleaf git authentication token.
#'
#' @return \code{path}, invisibly.
#'
#' @keywords internal
overleaf_ensure_clone <- function(path, remote, token = overleaf_token()) {

  check_gert()

  if (dir.exists(file.path(path, ".git"))) {
    return(invisible(path))
  }

  message("Downloading the Overleaf project. This happens once per project; ",
          "later saves are much quicker.")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    gert::git_clone(url = remote, path = path, password = token,
                    verbose = FALSE),
    error = function(e) {
      unlink(path, recursive = TRUE)
      stop(overleaf_git_error_message(e, remote), call. = FALSE)
    }
  )

  overleaf_set_identity(path)

  invisible(path)
}


#' Give a clone a commit identity
#'
#' @description Commits need a name and email. Most people at Grattan will have
#'   these set globally, but analysts who have never used git from the command
#'   line won't, so fall back to something sensible rather than erroring.
#'
#' @param repo Path to the clone.
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
overleaf_set_identity <- function(repo) {

  config <- tryCatch(gert::git_config(repo = repo),
                     error = function(e) NULL)

  is_set <- function(name) {
    !is.null(config) &&
      any(config$name == name & !is.na(config$value) & nzchar(config$value))
  }

  if (!is_set("user.name")) {
    gert::git_config_set("user.name", "grattantheme", repo = repo)
  }

  if (!is_set("user.email")) {
    gert::git_config_set("user.email", "noreply@grattaninstitute.edu.au",
                         repo = repo)
  }

  invisible(NULL)
}


#' Turn a git error into something a chart-maker can act on
#'
#' @param e The condition raised by gert.
#' @param remote The remote being contacted.
#'
#' @return A character string.
#'
#' @keywords internal
overleaf_git_error_message <- function(e, remote = NULL) {

  msg <- conditionMessage(e)

  if (grepl("authentication|401|credentials|password", msg, ignore.case = TRUE)) {
    return(paste0("Overleaf rejected your git token. Tokens expire after a ",
                  "year - generate a new one at ",
                  "https://www.overleaf.com/user/settings and run ",
                  "set_overleaf_token().\n(git said: ", msg, ")"))
  }

  if (grepl("404|not found|repository", msg, ignore.case = TRUE)) {
    return(paste0("Couldn't find that Overleaf project (", remote, "). Check ",
                  "the project URL, and that you can open the project in ",
                  "Overleaf.\n(git said: ", msg, ")"))
  }

  paste0("Couldn't reach Overleaf. (git said: ", msg, ")")
}


# Syncing ---------------------------------------------------------------------

#' Which Overleaf backend is in use?
#'
#' @return Either "git" or "dropbox".
#'
#' @keywords internal
overleaf_backend <- function() {
  backend <- Sys.getenv("GRATTANTHEME_OVERLEAF_BACKEND", unset = "dropbox")
  if (backend == "") "dropbox" else backend
}


#' Commit and push charts to Overleaf
#'
#' @description Called by \code{grattan_save_overleaf()} after a chart is
#'   written. Does nothing unless the Overleaf project is synced over git.
#'
#' @param push Logical. Should the commit be pushed to Overleaf?
#' @param commit_message Commit message. If \code{NULL}, one is generated from
#'   the files that changed.
#'
#' @return \code{TRUE} if charts were pushed, otherwise \code{FALSE},
#'   invisibly.
#'
#' @keywords internal
overleaf_sync <- function(push = TRUE, commit_message = NULL) {

  if (overleaf_backend() != "git") {
    return(invisible(FALSE))
  }

  check_gert()

  repo <- Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT_PATH", unset = "")

  if (repo == "" || !dir.exists(file.path(repo, ".git"))) {
    stop("No Overleaf clone found. Call set_overleaf_project() with your ",
         "project's URL.")
  }

  committed <- overleaf_commit(repo, commit_message)

  if (!isTRUE(push)) {
    if (committed) {
      message("Chart committed locally. Call overleaf_push() to send your ",
              "charts to Overleaf.")
    }
    return(invisible(FALSE))
  }

  # Re-saving an unchanged chart shouldn't cost a trip to Overleaf
  if (!committed && !overleaf_has_unpushed(repo)) {
    return(invisible(FALSE))
  }

  overleaf_push_commits(repo)
}


#' Are there commits the remote hasn't got?
#'
#' @param repo Path to the clone.
#'
#' @return \code{TRUE} if the clone is ahead of Overleaf, or if we can't tell.
#'
#' @keywords internal
overleaf_has_unpushed <- function(repo) {

  status <- tryCatch(gert::git_ahead_behind(repo = repo),
                     error = function(e) NULL)

  # gert omits `ahead` entirely when there's nothing to push
  if (is.null(status)) {
    return(TRUE)
  }

  isTRUE(status$ahead > 0)
}


#' Stage and commit everything in the clone's atlas folder
#'
#' @description The clone is only ever written to by the package, so staging the
#'   whole atlas folder is safe: anything in there either came from Overleaf or
#'   was put there by \code{grattan_save_overleaf()}. Git works out what has
#'   actually changed, so re-saving an identical chart produces no commit.
#'
#' @param repo Path to the clone.
#' @param commit_message Commit message, or \code{NULL} to generate one.
#'
#' @return \code{TRUE} if a commit was made, otherwise \code{FALSE}.
#'
#' @keywords internal
overleaf_commit <- function(repo, commit_message = NULL) {

  atlas <- file.path(repo, "atlas")

  if (!dir.exists(atlas)) {
    return(FALSE)
  }

  gert::git_add("atlas", repo = repo)

  staged <- gert::git_status(staged = TRUE, repo = repo)

  if (nrow(staged) == 0) {
    return(FALSE)
  }

  if (is.null(commit_message)) {
    commit_message <- overleaf_commit_message(staged$file)
  }

  gert::git_commit(commit_message, repo = repo)

  TRUE
}


#' Build a commit message from the files that changed
#'
#' @param files Character vector of paths, relative to the clone.
#'
#' @return A character string.
#'
#' @keywords internal
overleaf_commit_message <- function(files) {

  charts <- basename(files)

  what <- if (length(charts) <= 3) {
    paste(charts, collapse = ", ")
  } else {
    paste0(length(charts), " charts")
  }

  paste0("Update ", what, " (grattantheme)")
}


#' Push commits to Overleaf, retrying if the project has moved on
#'
#' @description If someone edited the project in Overleaf since we cloned it,
#'   the push is rejected. Rather than merging, we reset the clone to whatever
#'   Overleaf now has, put our charts back on top, and try again.
#'
#' @param repo Path to the clone.
#' @param token The Overleaf git authentication token.
#' @param attempts How many times to try.
#'
#' @return \code{TRUE} if the push succeeded, otherwise \code{FALSE},
#'   invisibly.
#'
#' @keywords internal
overleaf_push_commits <- function(repo, token = overleaf_token(), attempts = 3) {

  for (attempt in seq_len(attempts)) {

    result <- tryCatch({
      gert::git_push(repo = repo, password = token, verbose = FALSE)
      TRUE
    }, error = function(e) e)

    if (isTRUE(result)) {
      return(invisible(TRUE))
    }

    # No point retrying a bad token
    if (grepl("authentication|401|credentials", conditionMessage(result),
              ignore.case = TRUE)) {
      stop(overleaf_git_error_message(result), call. = FALSE)
    }

    if (attempt < attempts) {
      overleaf_rebuild_on_remote(repo, token)
    }
  }

  warning("Couldn't push your charts to Overleaf (",
          conditionMessage(result), "). They are saved locally - ",
          "call overleaf_push() to try again.")

  invisible(FALSE)
}


#' Reset the clone to the remote, keeping our charts
#'
#' @description Fetches what Overleaf now has and discards local history, then
#'   restores the atlas folder from the working tree and commits it again.
#'
#'   Keeping the whole atlas folder means a chart deleted in Overleaf could
#'   reappear, which is a fair trade for never losing a chart and never having
#'   to resolve a conflict between two PDFs.
#'
#' @param repo Path to the clone.
#' @param token The Overleaf git authentication token.
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
overleaf_rebuild_on_remote <- function(repo, token) {

  atlas <- file.path(repo, "atlas")

  kept <- file.path(tempfile("overleaf-atlas-"))
  dir.create(kept, recursive = TRUE)

  if (dir.exists(atlas)) {
    file.copy(list.files(atlas, full.names = TRUE), kept,
              overwrite = TRUE, recursive = TRUE)
  }

  gert::git_fetch(repo = repo, password = token, verbose = FALSE)
  gert::git_reset_hard(paste0("origin/", gert::git_branch(repo = repo)),
                       repo = repo)

  dir.create(atlas, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(kept, full.names = TRUE), atlas,
            overwrite = TRUE, recursive = TRUE)

  unlink(kept, recursive = TRUE)

  overleaf_commit(repo)

  invisible(NULL)
}


#' Send saved charts to Overleaf
#'
#' @description Pushes any charts that haven't made it to Overleaf yet. You only
#'   need this if you saved charts with \code{grattan_save_overleaf(push =
#'   FALSE)}, or if a push failed earlier in your session.
#'
#' @details Saving many charts with \code{push = FALSE} and calling
#'   \code{overleaf_push()} once at the end is quicker than pushing each chart
#'   as it's saved, because it contacts Overleaf once rather than once per
#'   chart.
#'
#' @return \code{TRUE} if charts were pushed, otherwise \code{FALSE},
#'   invisibly.
#'
#' @seealso \code{\link{grattan_save_overleaf}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_overleaf_project("https://www.overleaf.com/project/0123456789abcdef01234567")
#'
#' grattan_save_overleaf("figure1.pdf", p1, push = FALSE)
#' grattan_save_overleaf("figure2.pdf", p2, push = FALSE)
#'
#' overleaf_push()
#' }
overleaf_push <- function() {

  if (overleaf_backend() != "git") {
    message("Your Overleaf project syncs via Dropbox, so there's nothing to ",
            "push.")
    return(invisible(FALSE))
  }

  overleaf_sync(push = TRUE)
}
