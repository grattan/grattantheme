#' Write a number or short text to your Overleaf project
#'
#' Writes a single value - usually a key statistic - to a \code{.tex} file in
#' your Overleaf project's \code{atlas} folder, ready to include in the report
#' with \code{\\input{atlas/filename}}. Designed to sit at the end of a pipe
#' that \code{pull()}s out one number.
#'
#' @param value A single value: a number, or text that is already formatted.
#'   Numbers are written in full (\code{1000000}, never \code{1e+06}) unless
#'   you supply \code{accuracy}.
#' @param filename Name of the file to write in the \code{atlas} folder, with
#'   or without the \code{.tex} extension. Can include a subfolder, such as
#'   \code{"stats/debt-2025"}.
#' @param accuracy Optional. Round a number to this accuracy before writing
#'   it, using \code{scales::number()} with a comma between thousands - so
#'   \code{accuracy = 0.1} turns \code{1234.567} into \code{1,234.6}. Ignored
#'   for text. Defaults to \code{NULL}, which writes the number as it is.
#' @param push Logical. Only used when your Overleaf project syncs over git.
#'   \code{TRUE} by default, meaning the file is sent to Overleaf as soon as
#'   it's written. Set to \code{FALSE} when writing a batch of values, then
#'   call \code{\link{overleaf_push}} once at the end - it's quicker.
#'
#' @details The file ends with a \code{\%}, so that LaTeX doesn't add a space
#'   after the value: \code{\\input{atlas/debt}\\\%} gives "3.4\%", not
#'   "3.4 \%".
#'
#'   The value is written as it is, so text containing LaTeX special
#'   characters needs escaping yourself. In particular a bare \code{\%} would
#'   start a LaTeX comment, so write \code{"50\\\\\%"} rather than
#'   \code{"50\%"}.
#'
#'   Like \code{\link{grattan_save_overleaf}}, this writes to the project set
#'   with \code{\link{set_overleaf_project}}. If the file has been edited or
#'   deleted in Overleaf, writing it again puts your version back.
#'
#' @return The path of the file written, invisibly.
#'
#' @seealso \code{\link{grattan_save_overleaf}} to save charts the same way,
#'   \code{\link{overleaf_push}} to send batched files to Overleaf
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_overleaf_project("https://www.overleaf.com/project/0123456789abcdef01234567")
#'
#' # At the end of a pipe
#' mtcars %>%
#'   summarise(mpg = mean(mpg)) %>%
#'   pull(mpg) %>%
#'   write_overleaf_tex("mean-mpg", accuracy = 0.1)
#'
#' # Already-formatted text
#' write_overleaf_tex("\\$2.5 billion", "cost-total")
#'
#' # Writing a batch of values: push once at the end
#' write_overleaf_tex(12.3, "stat-a", push = FALSE)
#' write_overleaf_tex(45.6, "stat-b", push = FALSE)
#' overleaf_push()
#' }
write_overleaf_tex <- function(value,
                               filename,
                               accuracy = NULL,
                               push = TRUE) {

  if (length(value) != 1 || is.list(value) || is.na(value)) {
    stop("`value` must be a single value, but it has ",
         if (length(value) == 1) "a missing value" else
           paste(length(value), "values"),
         ". Check that the code producing it picks out exactly one row.",
         call. = FALSE)
  }

  if (!is.character(filename) || length(filename) != 1 || !nzchar(filename)) {
    stop("`filename` must be a single file name, such as \"debt-2025\".",
         call. = FALSE)
  }

  text <- if (is.numeric(value) && !is.null(accuracy)) {
    scales::number(value, accuracy = accuracy, big.mark = ",")
  } else if (is.numeric(value)) {
    format(value, scientific = FALSE, trim = TRUE, digits = 15)
  } else {
    as.character(value)
  }

  if (!grepl("\\.tex$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".tex")
  }

  path <- file.path(get_overleaf_project(), filename)
  started <- Sys.time()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # The trailing % stops LaTeX adding a space where the line ends
  writeLines(paste0(text, "%"), path)

  overleaf_sync(since = started, push = push)

  invisible(path)
}
