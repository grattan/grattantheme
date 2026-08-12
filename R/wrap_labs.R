#' Format title, subtitle, and caption of a ggplot2 chart in the Grattan style.
#'
#' Use `wrap_labs()` to wrap the title, subtitle, and caption of a ggplot2 chart
#' onto multiple lines, left-align them, and split 'notes' and 'source' onto
#' multiple lines. Note that this is done automatically for you if you use
#' `grattan_save()` - there is no need to use `wrap_labs()` if you also plan to
#' use `grattan_save()`. The function will warn if the title is too long for
#' the chart format chosen.
#'
#' @name wrap_labs
#' @param object Name of the ggplot2 chart object with the labels you wish to
#'   modify.
#' @param type The type of chart you intend to save (different chart types have
#'   different numbers of characters on each line). `type` can be 'normal',
#'   'fullslide', 'blog', etc. For a full list of possible types, see
#'   \code{?grattan_save()}.
#' @param labs_to_wrap Default is c("title", "subtitle", "caption"), which
#' wraps all three labels. Choose one or two if you only want to wrap those.
#' @param ignore_long_title Default is FALSE. If TRUE, the check on a long title
#' won't be performed. This is useful if using ggtext syntax within titles.
#'
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan() +
#'     labs(title = "My title goes here and is quite long and needs to break
#'     over two lines, as is common for Grattan charts",
#'     subtitle = "Subtitle goes here",
#'     caption = "Notes: Put some notes here. Source: Put your source here.")
#'
#' p_wrapped <- wrap_labs(p, "normal")
#'
#' p_wrapped
#'
#' @export


wrap_labs <- function(object,
                      type,
                      labs_to_wrap = c("title", "subtitle", "caption"),
                      ignore_long_title = TRUE) {

  p <- object

  # The legacy Powerpoint types are allowed here because `grattan_save_pptx()`
  # calls this. Widths are looked up in the full table, rather than the
  # active-only one, so that those types get their proper widths rather than a
  # zero-length width, which strwrap() turns into one word per line
  if (!type %in% c(all_chart_types, pptx_legacy_types)) {
    stop(check_chart_type_message(type))
  }

  type_row <- chart_types_all$type == type

  chart_class <- chart_types_all$class[type_row]

  wrap_title <- ifelse("title" %in% labs_to_wrap, TRUE, FALSE)
  wrap_subtitle <- ifelse("subtitle" %in% labs_to_wrap, TRUE, FALSE)
  wrap_caption <- ifelse("caption" %in% labs_to_wrap, TRUE, FALSE)

  labs <- extract_labs(p)

  # Wrap title ----

  max_title_lines <- if (identical(type, "blog")) 3 else 2

  if (isTRUE(wrap_title)) {

    stored_title <- labs$title

    if (isFALSE(is.null(stored_title))) {

      char_width_grattan_title <- chart_types_all$title[type_row]

      if (isFALSE(ignore_long_title) & (nchar(stored_title) > max_title_lines * char_width_grattan_title)) {
        # if title exceeds the allowed number of lines, emit a throttled
        # warning

        trimmed_title <- strtrim(stored_title, max_title_lines * char_width_grattan_title)
        trimmed_title_final_words <- paste0(utils::tail(strsplit(trimmed_title, split = " ")[[1]], 2), collapse = " ")

        rlang::warn(
          paste0("Your chart title is too long for a Grattan chart of type ",
                 type,
                 ". Please reduce the length of the title.\nEverything after '",
                 trimmed_title_final_words,
                 "' cannot fit onto the slide."),
          .frequency = "regularly",
          .frequency_id = paste0("grattantheme_long_title_", type)
        )
      }

      if (nchar(stored_title) > char_width_grattan_title) {

        wrapped_title <- strwrap(stored_title, char_width_grattan_title)
        stored_title <- paste0(wrapped_title, collapse = "\n")
      }

      labs$title <- stored_title
      }
  }

  # Wrap subtitle ----
  if (isTRUE(wrap_subtitle)) {

    stored_subtitle <- labs$subtitle

    if (isFALSE(is.null(stored_subtitle))) {

      char_width_grattan_subtitle <- chart_types_all$subtitle[type_row]


      if (isFALSE(ignore_long_title) & nchar(stored_subtitle) > 2 * char_width_grattan_subtitle) {
        # code to figure out the final 2 chunks of text before the subtitle limit
        trimmed_subtitle <- strtrim(stored_subtitle, 2 * char_width_grattan_subtitle)
        trimmed_subtitle_final_words <- paste0(utils::tail(strsplit(trimmed_subtitle, split = " ")[[1]],2), collapse = " ")

        rlang::warn(
          paste0("Your chart subtitle is too long for a Grattan Powerpoint slide of type ",
                 type,
                 ". Please reduce subtitle length.\nEverything after '",
                 trimmed_subtitle_final_words,
                 "' cannot fit onto the slide."),
          .frequency = "regularly",
          .frequency_id = paste0("grattantheme_long_subtitle_", type)
        )
      }

      if (nchar(stored_subtitle) <= 2 * char_width_grattan_subtitle &
          nchar(stored_subtitle) > char_width_grattan_subtitle) {

        stored_subtitle <- paste0(strwrap(stored_subtitle, char_width_grattan_subtitle)[1],
                                  "\n",
                                  strwrap(stored_subtitle, char_width_grattan_subtitle)[2])
      }

      labs$subtitle <- stored_subtitle
    }
  }


  # Wrap caption ----

  if (isTRUE(wrap_caption)) {

    stored_caption <- labs$caption

    if (isFALSE(is.null(stored_caption))) {

      char_width_grattan_caption  <- chart_types_all$caption[type_row]

      contains_notes_and_source <- grepl("notes?:", tolower(stored_caption)) & grepl("sources?:", tolower(stored_caption))

      # if the caption doesn't contain "notes" and "source", we want to wrap the whole
      # caption string across lines; if notes and source are present we want to wrap them separately
      if (!contains_notes_and_source) {
        caption_lines <- ceiling(nchar(stored_caption) / char_width_grattan_caption)

        if (caption_lines > 1) {
          stored_caption <- paste0(strwrap(stored_caption,
                                           char_width_grattan_caption),
                                   collapse = "\n")
        }
      } else {# now deal with the case when "notes" and "source" are present
        notes_and_source <- strsplit(stored_caption, split = "Source")
        notes <- notes_and_source[[1]][1]
        source <- paste0("Source", notes_and_source[[1]][2])

        notes <- paste0(strwrap(notes, char_width_grattan_caption),
                        collapse = "\n")

        source <- paste0(strwrap(source, char_width_grattan_caption),
                         collapse = "\n")

        stored_caption <- paste0(notes, "\n", source)
      }


      labs$caption <- stored_caption
    }
  }

  p <- replace_labs(p, labs)

  p
}


#' Split a caption's 'notes' and 'source' onto separate lines, without wrapping
#'
#' Used for PowerPoint exports, where the caption goes into a text box that
#' wraps text itself. Wrapping the lines ourselves produces lines that are
#' slightly too wide for the placeholder, which PowerPoint then re-wraps,
#' orphaning the last word or two of every line.
#'
#' @param p a ggplot2 object
#'
#' @return a ggplot2 object
#' @noRd
split_notes_and_source <- function(p) {

  labs <- extract_labs(p)

  if (is.null(labs$caption)) {
    return(p)
  }

  contains_notes_and_source <- grepl("notes?:", tolower(labs$caption)) &
    grepl("sources?:", tolower(labs$caption))

  if (isFALSE(contains_notes_and_source)) {
    return(p)
  }

  notes_and_source <- strsplit(labs$caption, split = "Source")[[1]]

  labs$caption <- paste0(trimws(notes_and_source[1]),
                         "\n",
                         trimws(paste0("Source", notes_and_source[2])))

  replace_labs(p, labs)
}
