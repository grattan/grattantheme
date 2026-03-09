#' Write an AI coding guide to your project directory
#'
#' Creates a markdown file with instructions for AI coding assistants
#' (such as Claude, Codex, or Gemini) on how to create Grattan-styled
#' charts using the \code{grattantheme} package. The guide covers chart styling
#' conventions, colour palettes, saving functions, and common patterns.
#'
#' @param filename The name of the file to create. Defaults to \code{"CLAUDE.md"}.
#'   Different AI tools look for different filenames:
#'   \describe{
#'     \item{CLAUDE.md}{Used by Claude Code (Anthropic). Also reads AGENTS.md.}
#'     \item{AGENTS.md}{Cross-tool standard used by OpenAI Codex,
#'     Google Gemini/Jules, and many other AI coding assistants.}
#'   }
#' @param path The directory in which to create the file. Defaults to the
#'   current working directory.
#' @param overwrite Logical. If \code{FALSE} (the default), the function will
#'   not overwrite an existing file and will instead display a message.
#'
#' @return The file path of the created file (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Write a CLAUDE.md file (default, for Claude Code)
#' grattan_write_ai_guide()
#'
#' # Write an AGENTS.md file (cross-tool standard for Codex, Gemini, etc.)
#' grattan_write_ai_guide("AGENTS.md")
#'
#' # Write to a specific directory
#' grattan_write_ai_guide("CLAUDE.md", path = "path/to/project")
#'
#' # Overwrite an existing file
#' grattan_write_ai_guide(overwrite = TRUE)
#' }
grattan_write_ai_guide <- function(filename = "CLAUDE.md",
                                    path = ".",
                                    overwrite = FALSE) {

  filepath <- file.path(path, filename)

  if (file.exists(filepath) && !overwrite) {
    message(filename, " already exists at ",
            normalizePath(filepath),
            ". Use overwrite = TRUE to replace it.")
    return(invisible(filepath))
  }

  template <- system.file("extdata", "ai_guide_template.md",
                           package = "grattantheme")

  if (template == "") {
    stop("Could not find the AI guide template. ",
         "Try reinstalling grattantheme.")
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  file.copy(template, filepath, overwrite = overwrite)

  message("Created ", filename, " at ", normalizePath(filepath))

  invisible(normalizePath(filepath))
}
