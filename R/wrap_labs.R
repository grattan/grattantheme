#' Format title, subtitle, and caption of a ggplot2 chart in the Grattan style.
#'
#' Use `wrap_labs()` to wrap the title, subtitle, and caption of a ggplot2 chart
#' onto multiple lines, left-align them, and split 'notes' and 'source' onto
#' multiple lines. Note that this is done automatically for you if you use
#' `grattan_save()` - there is no need to use `wrap_labs()` if you also plan to
#' use `grattan_save()`.
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

  chart_class <- chart_types$class[chart_types$type == type]

  wrap_title <- ifelse("title" %in% labs_to_wrap, TRUE, FALSE)
  wrap_subtitle <- ifelse("subtitle" %in% labs_to_wrap, TRUE, FALSE)
  wrap_caption <- ifelse("caption" %in% labs_to_wrap, TRUE, FALSE)

  labs <- extract_labs(p)

  # Wrap title ----

  if (isTRUE(wrap_title)) {

    stored_title <- labs$title

    if (isFALSE(is.null(stored_title))) {

      char_width_grattan_title <- chart_types$title[chart_types$type == type]

      # add line break to title where necessary
      if (nchar(stored_title) <= char_width_grattan_title &
          chart_class == "fullslide" &
          !grepl("\n", stored_title)) {

        stored_title <- paste0("\n", stored_title)
      }


      if (isFALSE(ignore_long_title) & (nchar(stored_title) > 2 * char_width_grattan_title)) {
        # if title > 2 lines, return an informative error that tells users
        # where they need to trim their title to

        # code to figure out the final 2 chunks of text before the title limit
        trimmed_title <- strtrim(stored_title, 2 * char_width_grattan_title)
        trimmed_title_final_words <- paste0(utils::tail(strsplit(trimmed_title, split = " ")[[1]], 2), collapse = " ")

        # return an error and tell the user where the useable string ends
        stop("Your chart title is too long for a Grattan chart of type ",
             type,
             ". Please reduce the length of the title.\nEverything after '",
             trimmed_title_final_words,
             "' cannot fit onto the slide.")
      }

      if (nchar(stored_title) <= 2 * char_width_grattan_title &
          nchar(stored_title) > char_width_grattan_title) {

        stored_title <- paste0(strwrap(stored_title, char_width_grattan_title)[1],
                               "\n",
                               strwrap(stored_title, char_width_grattan_title)[2])
      }

      labs$title <- stored_title
      }
  }

  # Wrap subtitle ----
  if (isTRUE(wrap_subtitle)) {

    stored_subtitle <- labs$subtitle

    if (isFALSE(is.null(stored_subtitle))) {

      char_width_grattan_subtitle <- chart_types$subtitle[chart_types$type == type]


      if (isFALSE(ignore_long_title) & nchar(stored_subtitle) > 2 * char_width_grattan_subtitle) {
        # code to figure out the final 2 chunks of text before the title limit
        trimmed_subtitle <- strtrim(stored_subtitle, 2 * char_width_grattan_subtitle)
        trimmed_subtitle_final_words <- paste0(utils::tail(strsplit(trimmed_subtitle, split = " ")[[1]],2), collapse = " ")
        # return an error and tell the user where the useable string ends
        stop("Your chart subtitle is too long for a Grattan Powerpoint slide of type ",
             type,
             ". Please reduce subtitle length.\nEverything after '",
             trimmed_subtitle_final_words,
             "' cannot fit onto the slide.")


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

      char_width_grattan_caption  <- chart_types$caption[chart_types$type == type]

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
