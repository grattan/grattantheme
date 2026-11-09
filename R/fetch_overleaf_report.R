#' Make a read-only copy of your Overleaf report
#'
#' Copies the text of the report from your Overleaf project into a folder in
#' your working directory, so that you - or an AI assistant such as Claude
#' Code - can read the latest version of the report alongside your analysis.
#' The copy is read-only and is never sent back to Overleaf.
#'
#' @param dir Folder to put the copy in, relative to your working directory.
#'   Defaults to \code{"report_cache"}. It is replaced each time
#'   \code{fetch_overleaf_report()} runs.
#' @param main The report's main file. Defaults to \code{"Report.tex"}. If
#'   there's no such file, the top-level \code{.tex} file containing
#'   \code{\\documentclass} is used instead.
#' @param chapters Folder holding the chapter files. Defaults to
#'   \code{"chapters"}.
#'
#' @details The copy holds:
#'   \itemize{
#'     \item the main file;
#'     \item every \code{.tex} file in the chapters folder;
#'     \item the bibliography (every \code{.bib} file in the project, such as
#'       those in the \code{bib} folder);
#'     \item every \code{.tex} file in \code{atlas}, such as the statistics
#'       written by \code{\link{write_overleaf_tex}}.
#'   }
#'   Charts and other images aren't copied.
#'
#'   Each run fetches the latest version from Overleaf and replaces the whole
#'   folder, so a chapter deleted in Overleaf disappears from the copy too.
#'   The files are marked read-only, and the folder holds a \code{.gitignore}
#'   so it's never committed if your analysis is in a git repository.
#'
#'   Nothing in the copy is connected to Overleaf:
#'   \code{fetch_overleaf_report()} never commits or pushes anything, and
#'   edits to the copy are lost the next time it runs.
#'
#'   For a project that syncs over git, this keeps a second copy of the
#'   project in the package's cache, separate from the one used to send your
#'   charts (see \code{\link{overleaf_cache_dir}}). The first run downloads it,
#'   so is slower than later ones.
#'
#' @return The path of the folder holding the copy, invisibly.
#'
#' @seealso \code{\link{set_overleaf_project}} to choose the project
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_overleaf_project("https://www.overleaf.com/project/0123456789abcdef01234567")
#' fetch_overleaf_report()
#' }
fetch_overleaf_report <- function(dir = "report_cache",
                                  main = "Report.tex",
                                  chapters = "chapters") {

  project_path <- Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT_PATH", unset = "")

  if (project_path == "") {
    stop("No Overleaf project set. Call set_overleaf_project() first.",
         call. = FALSE)
  }

  # Somewhere holding Overleaf's latest version of the project
  if (overleaf_backend() == "git") {
    source <- overleaf_update_report_clone(project_path)
    version <- gert::git_log(max = 1, repo = source)$commit
  } else {
    source <- project_path
    version <- NULL
  }

  main <- overleaf_main_file(source, main)

  tex_files <- list.files(source, pattern = "\\.tex$", recursive = TRUE,
                          ignore.case = TRUE)
  chapter_files <- tex_files[startsWith(tex_files, paste0(chapters, "/"))]
  atlas_files <- tex_files[startsWith(tex_files, "atlas/")]
  # Anywhere in the project: Grattan's template keeps them in bib/
  bib_files <- list.files(source, pattern = "\\.bib$", recursive = TRUE,
                          ignore.case = TRUE)
  files <- c(main, chapter_files, bib_files, atlas_files)

  overleaf_clear_report_dir(dir)

  for (file in files) {
    dir.create(file.path(dir, dirname(file)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(source, file), file.path(dir, file))
  }

  writeLines("*", file.path(dir, ".gitignore"))
  writeLines(overleaf_report_note(version), file.path(dir, report_note_file))

  Sys.chmod(file.path(dir, c(files, report_note_file)), mode = "0444")

  n_files <- function(n, what) paste(n, what, if (n == 1) "file" else "files")
  message("Synced a read-only copy of the report from Overleaf into ", dir,
          "/:\n  ", main, ", ",
          n_files(length(chapter_files), "chapter"), ", ",
          n_files(length(bib_files), "bibliography"), ", ",
          n_files(length(atlas_files), "atlas .tex"))

  invisible(normalizePath(dir))
}


# The note left in the copy, which also marks the folder as one that
# fetch_overleaf_report() made and so can safely replace
report_note_file <- "SYNCED_FROM_OVERLEAF.txt"

overleaf_report_note <- function(version = NULL) {
  c(paste0("A read-only copy of the report, made by ",
           "grattantheme::fetch_overleaf_report() on ",
           format(Sys.time(), "%d %B %Y at %H:%M"),
           if (!is.null(version)) {
             paste0(" from Overleaf version ", substr(version, 1, 7))
           },
           "."),
    "",
    paste0("Nothing here is sent to Overleaf. Any edits are lost the next ",
           "time fetch_overleaf_report() runs - make changes in Overleaf ",
           "instead."))
}


#' Bring the read-only clone of an Overleaf project up to date
#'
#' @param project_path Path to the clone used for saving charts.
#'
#' @return Path to the read-only clone, which matches Overleaf's latest
#'   version.
#'
#' @keywords internal
overleaf_update_report_clone <- function(project_path) {

  # Beside the chart clones, in their own folder so it's never mistaken for one
  path <- file.path(dirname(project_path), "report", basename(project_path))

  if (!dir.exists(file.path(path, ".git"))) {
    # Clone from wherever the chart clone does (in tests, a stand-in for
    # Overleaf), re-making the chart clone if the cache has been cleared
    overleaf_ensure_clone(project_path)
    origin <- gert::git_remote_info("origin", repo = project_path)$url
    overleaf_ensure_clone(path, remote = origin)
    return(path)
  }

  token <- overleaf_token()

  tryCatch(
    with_overleaf_auth(path, token,
                       gert::git_fetch(repo = path, verbose = FALSE)),
    error = function(e) stop(overleaf_git_error_message(e), call. = FALSE)
  )

  # Nothing is ever saved in this clone, so it can always be reset
  gert::git_reset_hard(paste0("origin/", gert::git_branch(repo = path)),
                       repo = path)

  path
}


#' Find the report's main .tex file
#'
#' @param source Folder holding the project.
#' @param main Name of the main file to look for first.
#'
#' @return The main file's name.
#'
#' @keywords internal
overleaf_main_file <- function(source, main) {

  if (file.exists(file.path(source, main))) {
    return(main)
  }

  top_level <- list.files(source, pattern = "\\.tex$", ignore.case = TRUE)
  has_class <- vapply(top_level, function(file) {
    lines <- readLines(file.path(source, file), warn = FALSE)
    any(grepl("^\\s*\\\\documentclass", lines))
  }, logical(1))

  candidates <- top_level[has_class]

  if (length(candidates) == 1) {
    return(candidates)
  }

  if (length(candidates) == 0) {
    stop("Couldn't find ", main, ", or another .tex file with ",
         "\\documentclass, at the top level of the Overleaf project.",
         call. = FALSE)
  }

  stop("Couldn't find ", main, ", and found several possible main files: ",
       paste(candidates, collapse = ", "), ". Choose one with `main`.",
       call. = FALSE)
}


#' Empty the folder that holds the copy of the report
#'
#' Refuses to delete a folder that \code{fetch_overleaf_report()} didn't
#' make, so a mistyped \code{dir} can't remove someone's work.
#'
#' @param dir The folder.
#'
#' @return Nothing, called for its side effect.
#'
#' @keywords internal
overleaf_clear_report_dir <- function(dir) {

  if (dir.exists(dir)) {

    existing <- list.files(dir, recursive = TRUE, all.files = TRUE,
                           full.names = TRUE)

    if (length(existing) > 0 &&
        !file.exists(file.path(dir, report_note_file))) {
      stop("'", dir, "' already exists and wasn't made by ",
           "fetch_overleaf_report(), so it hasn't been replaced. Choose ",
           "another folder with `dir`.", call. = FALSE)
    }

    # Read-only files can't be deleted on Windows
    Sys.chmod(existing, mode = "0644")
    unlink(dir, recursive = TRUE)
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  invisible(NULL)
}
