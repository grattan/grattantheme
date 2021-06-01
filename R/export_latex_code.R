#' Export the figure environment LaTeX code for a ggplot2 graph object.
#' @name export_latex_code
#' @param p The ggplot2 graph object to be saved. Defaults to
#'   \code{last_plot()}, which will save the last plot that was displayed in
#'   your session.
#' @param chart_path The chart_path and export_type will be extracted
#' and used in the \code{\\includegraphics} call.
#' @param export_type The chart_path and export_type will be extracted
#' and used in the \code{\\includegraphics} call.
#' @return LaTeX code to create a figure environment
#' @import stringr
#' @importFrom clipr write_clip
#' @importFrom tools file_path_sans_ext
#' @export
#'


export_latex_code <- function(p = ggplot2::last_plot(),
                              chart_path = "atlas/chart.pdf",
                              export_type = "wholecolumn") {

  code_to_clipboard <- clipr::clipr_available()

  # make export_path
  if (export_type != "") export_type <- paste0("_", export_type)
  dir <- dirname(chart_path)
  name <- basename(chart_path) %>% tools::file_path_sans_ext()
  ext <- substring(chart_path, nchar(chart_path) - 2)
  export_name <- paste0(name, export_type, ".", ext)

  export_path <- paste(dir, name, export_name, sep = "/")

  # label
  lab <- name %>%
    tolower() %>%
    str_replace_all(" ", "-")


  # title
  title <- p$labels$title
  if (is.null(title)) message("No title")

  # units
  units <- p$labels$subtitle
  if (is.null(units)) message("No units")

  # note(s) (and) source(s)
  caption <- p$labels$caption
  if (is.null(caption)) {
    message("No notes or source provided.")
    caption_code <- "\\noteswithsource{}{}"
  }

  if (!is.null(caption)) {

    has_note <- caption %>% str_detect("(N|n)ote")
    has_source <- caption %>% str_detect("(S|s)ource")

    if (has_note & has_source) {
      caption_text <- str_split(caption, " ?Sources? ?:? ?", simplify = TRUE) %>%
        str_remove("(N|n)otes? ?:? ?")

    } else {
      caption_text <- caption %>%
        str_remove("(N|n)otes? ?:? ?") %>%
        str_remove("(S|s)ources? ?:? ?")
    }

    substr(caption_text, 1, 1) <- toupper(substr(caption_text, 1, 1))


    note_or_notes <- caption %>%
      str_detect("(N|n)otes") %>%
      ifelse("notes", "note")

    source_or_sources <- caption %>%
      str_detect("(S|s)ouces") %>%
      ifelse("sources", "source")


    if (has_note & has_source)  caption_command <- paste0("\\", note_or_notes, "with", source_or_sources)
    if (has_note & !has_source) caption_command <- paste0("\\", note_or_notes)
    if (!has_note & has_source) caption_command <- paste0("\\", source_or_sources)
    # if empty, assume note
    if (!has_note & !has_source) caption_command <- paste0("\\note")


    if (has_note & has_source) {
      caption_code <- paste0(caption_command,
                            "{", caption_text[1], "}",
                            "{", caption_text[2], "}")
    } else {
      caption_code <- paste0(caption_command,
                            "{", caption_text[1], "}")
    }

  }

  # combine
  code_to_export <-
  paste0("\\begin{figure}\n",
         "\t\\caption{", title, "\\label{fig:", lab,"}}\n",
         "\t\\units{", units, "}\n",
         "\t\\includegraphics[page= 1, width=1\\columnwidth]{", export_path, "}\n",
         "\t", caption_code, "\n",
         "\\end{figure}")

  code_to_export <- gsub("\\end{figure}\r",
                         "\\end{figure}",
                         code_to_export,
                         fixed = TRUE)

  if (isTRUE(code_to_clipboard)) {
    message(paste0("Copied to clipboard:\n\n", code_to_export, "\n\n"))
    clipr::write_clip(code_to_export, allow_non_interactive = TRUE)
  }

  return(code_to_export)

} # end function



