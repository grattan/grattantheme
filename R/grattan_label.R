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
#' @name grattan_label_functions
#'


#' @rdname grattan_label_functions
#' @export
grattan_label <- function(..., size = 18, padding = 0.1, lineheight = 0.8) {
  .size = size / ggplot2::.pt

  ggplot2::geom_label(
    ...,
    fill = "white",
    label.padding = unit(padding, "lines"),
    label.size = 0,
    size = .size
  )
}

#' @rdname grattan_label_functions
#' @export
grattan_label_repel <- function(..., size = 18, padding = 0.1) {
  .size = size / ggplot2::.pt

  ggrepel::geom_label_repel(
    ...,
    fill = "white",
    label.padding = unit(padding, "lines"),
    label.size = 0,
    size = .size
  )
}
