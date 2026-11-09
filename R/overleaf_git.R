# Overleaf git backend
#
# Charts reach Overleaf through its git bridge. The package keeps a clone of the
# project in a cache directory, charts are written into that clone's atlas
# folder by grattan_save_overleaf(), and this file commits and pushes them.
#
# The clone is managed by the package and never edited by hand, so there is no
# need to merge: before pushing, the clone is moved onto whatever Overleaf has
# now and only the charts that changed are committed on top.


# Project IDs -----------------------------------------------------------------

#' Get the project ID from an Overleaf project URL
#'
#' @param x A character string, such as the argument given to
#'   \code{set_overleaf_project()}.
#'
#' @return The 24-character project ID if \code{x} is an Overleaf project URL
#'   (like \code{https://www.overleaf.com/project/0123456789abcdef01234567}) or
#'   a bare project ID, otherwise \code{NULL} - for instance, for a Dropbox
#'   project name.
#'
#' @keywords internal
overleaf_project_id <- function(x) {

  if (!is.character(x) || length(x) != 1 || is.na(x) ||
      !grepl("overleaf\\.com|^[0-9a-f]{24}$", x, ignore.case = TRUE)) {
    return(NULL)
  }

  id <- regmatches(x, regexpr("[0-9a-f]{24}", x))
  if (length(id) == 1) id else NULL
}


#' Add Overleaf credentials to a git remote URL
#'
#' @description Overleaf's git bridge wants username "git" and the token as the
#'   password. These have to go in the URL itself: gert's password callback
#'   isn't reliable here, because a URL holding only a username gets rejected
#'   by Overleaf without the challenge libgit2 needs before it asks for a
#'   password, and gert drops the username if it isn't in the URL.
#'
#' @param url A remote URL. Anything other than an HTTPS URL (like the local
#'   paths used in tests) is returned unchanged.
#' @param token The Overleaf git authentication token, or \code{NULL} to strip
#'   any credentials from the URL.
#'
#' @return A character string.
#'
#' @keywords internal
overleaf_auth_url <- function(url, token = NULL) {

  if (!grepl("^https://", url)) {
    return(url)
  }

  # Drop any credentials already in the URL, such as the bare 'git@' that
  # earlier versions of grattantheme stored
  bare <- sub("^https://[^@/]*@", "https://", url)

  if (is.null(token) || !nzchar(token)) {
    return(bare)
  }

  sub("^https://",
      paste0("https://git:", utils::URLencode(token, reserved = TRUE), "@"),
      bare)
}


#' Run git commands with Overleaf credentials in the remote URL
#'
#' @description Puts the token into the clone's \code{origin} URL for as long
#'   as \code{code} runs, then takes it out again - even if \code{code}
#'   errors - so it isn't left in the clone's git config.
#'
#' @param repo Path to the clone.
#' @param token The Overleaf git authentication token.
#' @param code The git commands to run.
#'
#' @return The value of \code{code}.
#'
#' @keywords internal
with_overleaf_auth <- function(repo, token, code) {

  remotes <- gert::git_remote_list(repo = repo)
  url <- remotes$url[remotes$name == "origin"]

  gert::git_remote_set_url(overleaf_auth_url(url, token), remote = "origin",
                           repo = repo)
  on.exit(gert::git_remote_set_url(overleaf_auth_url(url), remote = "origin",
                                   repo = repo),
          add = TRUE)

  force(code)
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
  file.path(tools::R_user_dir("grattantheme", "cache"), "overleaf")
}


#' Delete cached Overleaf clones
#'
#' @description Removes the local clones of Overleaf projects kept by
#'   \code{\link{grattan_save_overleaf}} and \code{\link{overleaf_report}}.
#'   Anything already pushed to Overleaf is unaffected; a clone is downloaded
#'   again the next time it's needed.
#'
#' @param project An Overleaf project URL or ID. If \code{NULL} (the default),
#'   every cached project is deleted.
#'
#' @return The paths that were deleted, invisibly.
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

  path <- overleaf_cache_dir()

  if (!is.null(project)) {
    id <- overleaf_project_id(project)
    if (is.null(id)) {
      stop("'", project, "' isn't an Overleaf project URL.")
    }
    # The clone used for saving charts, and the read-only one used by
    # overleaf_report()
    path <- file.path(path, c(id, file.path("report", id)))
  }

  path <- path[dir.exists(path)]

  if (length(path) == 0) {
    message("Nothing in the Overleaf cache to delete.")
    return(invisible(path))
  }

  unlink(path, recursive = TRUE)
  message("Deleted ", paste(path, collapse = " and "))

  invisible(path)
}


#' Make sure a local clone of an Overleaf project exists
#'
#' @param path Where the clone should live. The folder is named after the
#'   project ID.
#' @param remote The git remote to clone from. Defaults to the project's
#'   Overleaf git URL, without credentials: the token is added only while
#'   Overleaf is being contacted.
#' @param token The Overleaf git authentication token.
#'
#' @return \code{path}, invisibly.
#'
#' @keywords internal
overleaf_ensure_clone <- function(path,
                                  remote = paste0("https://git.overleaf.com/",
                                                  basename(path)),
                                  token = overleaf_token()) {

  rlang::check_installed("gert", "to sync charts to Overleaf over git.")

  if (dir.exists(file.path(path, ".git"))) {
    return(invisible(path))
  }

  message("Downloading the Overleaf project. This happens once per project; ",
          "later saves are much quicker.")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tryCatch({
    gert::git_clone(url = overleaf_auth_url(remote, token), path = path,
                    verbose = FALSE)
    # Keep the token out of the clone's git config
    gert::git_remote_set_url(overleaf_auth_url(remote), remote = "origin",
                             repo = path)
  },
    error = function(e) {
      unlink(path, recursive = TRUE)
      stop(overleaf_git_error_message(e), call. = FALSE)
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

  if (!gert::user_is_configured(repo = repo)) {
    gert::git_config_set("user.name", "grattantheme", repo = repo)
    gert::git_config_set("user.email", "info@grattan.edu.au",
                         repo = repo)
  }

  invisible(NULL)
}


#' Turn a git error into something a chart-maker can act on
#'
#' @param e The condition raised by gert.
#'
#' @return A character string.
#'
#' @keywords internal
overleaf_git_error_message <- function(e) {

  msg <- conditionMessage(e)

  if (grepl("authentication|401|403|credentials", msg, ignore.case = TRUE)) {
    return(paste0("Overleaf refused access. Either your git token has ",
                  "expired (they last a year - generate a new one at ",
                  "https://www.overleaf.com/user/settings and run ",
                  "set_overleaf_token()), or you don't have git access to ",
                  "this project.\n(git said: ", msg, ")"))
  }

  if (grepl("404|not found|repository", msg, ignore.case = TRUE)) {
    return(paste0("Couldn't find that Overleaf project. Check the project ",
                  "URL, and that you can open the project in ",
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
  if (Sys.getenv("GRATTANTHEME_OVERLEAF_BACKEND") == "git") "git" else "dropbox"
}


#' Send changed charts to Overleaf
#'
#' @description Called by \code{grattan_save_overleaf()} after a chart is
#'   written, and by \code{overleaf_push()}. Does nothing unless the Overleaf
#'   project is synced over git.
#'
#'   The clone is managed by the package and never edited by hand, so there is
#'   never anything to merge. Charts that have changed since the last push are
#'   noted, the clone is moved onto whatever Overleaf has now, and just those
#'   charts are committed on top and pushed. If someone edits the project in
#'   between, the push is rejected and this is tried again.
#'
#' @param push Logical. If \code{FALSE}, the chart stays in the local clone
#'   until \code{overleaf_push()} is called.
#'
#' @return \code{TRUE} if charts were pushed, otherwise \code{FALSE},
#'   invisibly.
#'
#' @keywords internal
overleaf_sync <- function(push = TRUE) {

  if (overleaf_backend() != "git") {
    return(invisible(FALSE))
  }

  repo <- dirname(get_overleaf_project())

  # Charts that differ from what we last pushed. Re-saving an unchanged chart
  # finds nothing, so costs no trip to Overleaf.
  # Only ever files in atlas/: nothing else in the project is ours to change
  gert::git_add("atlas", repo = repo)
  charts <- gert::git_status(staged = TRUE, repo = repo)$file
  charts <- charts[startsWith(charts, "atlas/")]

  if (length(charts) == 0) {
    return(invisible(FALSE))
  }

  if (!isTRUE(push)) {
    message("Saved locally. Call overleaf_push() to send your changes ",
            "to Overleaf.")
    return(invisible(FALSE))
  }

  token <- overleaf_token()
  remote_branch <- paste0("origin/", gert::git_branch(repo = repo))

  for (attempt in 1:3) {

    # Move onto Overleaf's latest, leaving our charts in the working tree,
    # then commit only our charts so nobody else's work is overwritten
    tryCatch(
      with_overleaf_auth(repo, token,
                         gert::git_fetch(repo = repo, verbose = FALSE)),
      error = function(e) stop(overleaf_git_error_message(e), call. = FALSE)
    )
    gert::git_reset_mixed(remote_branch, repo = repo)
    gert::git_add(charts, repo = repo)

    if (nrow(gert::git_status(staged = TRUE, repo = repo)) == 0) {
      # Overleaf already has these exact charts
      gert::git_reset_hard(remote_branch, repo = repo)
      return(invisible(FALSE))
    }

    gert::git_commit(overleaf_commit_message(charts), repo = repo)
    # Bring everything else in the clone up to date with Overleaf
    gert::git_reset_hard("HEAD", repo = repo)

    result <- tryCatch({
      with_overleaf_auth(repo, token,
                         gert::git_push(repo = repo, verbose = FALSE))
      TRUE
    }, error = function(e) e)

    if (isTRUE(result)) {
      return(invisible(TRUE))
    }

    if (grepl("authentication|401|403|credentials", conditionMessage(result),
              ignore.case = TRUE)) {
      stop(overleaf_git_error_message(result), call. = FALSE)
    }
  }

  # Leave the charts uncommitted, so overleaf_push() picks them up next time
  gert::git_reset_mixed(remote_branch, repo = repo)

  warning("Couldn't push your charts to Overleaf (",
          conditionMessage(result), "). They are saved locally - ",
          "call overleaf_push() to try again.")

  invisible(FALSE)
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
    paste0(length(charts), " files")
  }

  paste0("Update ", what, " (grattantheme)")
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
