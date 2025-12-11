#' Replace a ggplot2's labels (title, subtitle, and/or caption) with a
#' given string. Works for regular ggplot2 plots and patchwork plots.
#'
#' @param p ggplot2 object
#' @param labs named list of labels to replace; must contain elements named
#' 'title', 'subtitle', and 'caption', and no other elements.
#'

replace_labs <- function(p,
                         labs = list(title = NULL,
                                     subtitle = NULL,
                                     caption = NULL)) {

  if (isFALSE(inherits(p, "gg"))) {
    stop("Plot is not a ggplot2 object.")
  }

  if (isFALSE(inherits(labs, "list"))) {
    stop("labs must be a list.")
  }

  if (isFALSE(identical(sort(names(labs)),
                        c("caption", "subtitle", "title")))) {
    stop("labs must be a named list containing elements named title, ",
         "subtitle, and caption, and only those elements.")
  }

  # Patchwork plots
  if (isTRUE(inherits(p, "patchwork"))) {
    # First, remove existing labels
   p$patches$annotation <- subset(p$patches$annotation,
                                  !names(p$patches$annotation) %in%
                                    c("title", "subtitle", "caption"))

   # Then, replace with supplied labels
   p$patches$annotation <- c(p$patches$annotation,
                             labs)

  } else {# Non-patchwork plots

    # Non-patchwork plots: use labs() to properly set labels
    p <- p + ggplot2::labs(title = labs$title,
                           subtitle = labs$subtitle,
                           caption = labs$caption)

  }
  p
}
