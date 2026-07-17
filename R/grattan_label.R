#' Create labels in the Grattan style - no border, minimal padding
#'
#' `grattan_label()` and `grattan_label_repel()` create labels using
#' `ggplot2::geom_label()` and `ggrepel::geom_label_repel()` respectively. The
#' labels will have no border and minimal padding, in keeping with Grattan
#' Institute's visual style.
#'
#' `grattan_label()` labels, just like `ggplot::geom_label()` labels, will be places at the
#' x, y coordinates given by the data. `grattan_label_repel()` labels, just like
#' `ggrepel::geom_label_repel()` labels, will be placed near their x, y
#' coordinates, but will be places so that the labels don't overlap each other.
#'
#' @param ... Arguments passed on to either `ggplot2::geom_label()` (in the
#' case of `grattan_label()`) or `ggrepel::geom_label_repel()` (in the case of
#' `grattan_label_repel()`). See \code{?geom_label} or \code{?geom_label_repel}.
#' @param size Font size for label. Default is 18.
#' @param padding Amount of white padding around label, measured in "lines"
#' (see \code{?unit::grid}). Default is 0.1.
#' @param lineheight Height of lines of text - smaller means the lines
#' of text are closer together. Default is `0.8`.
#' @param linewidth Size of label border line, in mm. Default is `NA`, which means no border.
#' @param label.size `r lifecycle::badge("deprecated")` Use `linewidth` instead.
#' @param fill Colour of label background; default is `"white"`.
#'
#' @examples
#' library(ggplot2)
#'
#' # Create a dataset -- mtcars with rownames as column
#' mt <- mtcars
#' mt$car <- rownames(mt)
#'
#' # Create a plot without labels
#' p <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
#'      geom_point() +
#'      theme_grattan()
#'
#' # A chart with Grattan labels
#' p +
#'     grattan_label()
#'
#' # A chart with Grattan labels that are repelled from each other
#' p +
#'     grattan_label_repel()
#'
#'
#' @importFrom ggrepel geom_label_repel
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @name grattan_label_functions
#'


#' @rdname grattan_label_functions
#' @export
grattan_label <- function(..., size = 18, padding = 0.1, lineheight = 0.8,
                          linewidth = NA, fill = "white") {
  .size = size / ggplot2::.pt

  dots <- list(...)

  args <- list(
    fill = fill,
    linewidth = linewidth,
    size = .size,
    lineheight = lineheight
  )

  # Only apply the `padding` default if the user hasn't passed `label.padding`
  # directly through `...` - otherwise it would be matched twice.
  if (!"label.padding" %in% names(dots)) {
    args$label.padding <- unit(padding, "lines")
  }

  do.call(ggplot2::geom_label, c(dots, args))
}

#' @rdname grattan_label_functions
#' @export
grattan_label_repel <- function(..., size = 18, padding = 0.1, lineheight = 0.8,
                                linewidth = NA, label.size = deprecated(),
                                fill = "white") {
  .size = size / ggplot2::.pt

  if (lifecycle::is_present(label.size)) {
    lifecycle::deprecate_warn("1.5.1", "grattan_label_repel(label.size)",
                              "grattan_label_repel(linewidth)")
    linewidth <- label.size
  }

  dots <- list(...)

  # ggrepel uses both label.size (geom param) and linewidth (aesthetic) for
  # border control; set both to ensure no border regardless of ggrepel version
  args <- list(
    fill = fill,
    label.size = linewidth,
    linewidth = if (is.na(linewidth)) 0 else linewidth,
    size = .size,
    lineheight = lineheight
  )

  # Only apply the `padding` default if the user hasn't passed `label.padding`
  # directly through `...` - otherwise it would be matched twice.
  if (!"label.padding" %in% names(dots)) {
    args$label.padding <- unit(padding, "lines")
  }

  do.call(ggrepel::geom_label_repel, c(dots, args))
}
