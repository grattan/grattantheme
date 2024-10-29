# Function takes as input a ggplot2 object and returns a list containing the
# title, subtitle, and caption of the object. Can handle regular ggplot2 plots
# and patchwork plots with labels added using patchwork::plot_annotation()
#' Take a ggplot2 object and return a list containing its title, subtitle, and
#' caption. Recognises Patchwork plots.
#' @param p ggplot2 object
#' @return list with three elements: title, subtitle, caption

extract_labs <- function(p) {

  if (isFALSE(inherits(p, "gg"))) {
    stop("Plot is not a ggplot2 object.")
  }

  if (isTRUE(inherits(p, "patchwork"))) {
    title <- p$patches$annotation$title
    subtitle <- p$patches$annotation$subtitle
    caption <- p$patches$annotation$caption

    # Warn user if labels added with +labs() to a Patchwork plot
    if (isTRUE(is.null(c(title,
                         subtitle,
                         caption)))) {
      if (isFALSE(is.null(c(p$labels$title,
                          p$labels$subtitle,
                          p$labels$caption)))) {
      warning("Add title, subtitle, and/or caption to a Patchwork plot using",
              " `patchwork::plot_annotation()`, not `labs()`.")
      }
    }
  } else {
    title <- p$labels$title
    subtitle <- p$labels$subtitle
    caption <- p$labels$caption

  }

  raw_labs_list <- list(title = title,
                        subtitle = subtitle,
                        caption = caption)

  list_no_html <- purrr::map(raw_labs_list,
                             text_from_html)

  list_no_html
}

text_from_html <- function(x) {

  if (length(x) > 0) {

    x %>%
      charToRaw() %>%
      rvest::read_html() %>%
      rvest::html_text2()
  } else {
    x
  }

}
