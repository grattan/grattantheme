#' Convenience function to create large points with white centres
#'
#' A wrapper around [ggplot2::geom_point()] that uses a filled shape with
#' a white centre by default.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' If `NULL` (the default), inherits the plot mapping.
#' @param data A data frame. If `NULL` (the default), inherits the plot data.
#' @param size Size of the point
#' @param stroke width of the line around the outside of the point
#' @param fill The colour on the inside of the point
#' @param shape The shape of the point; default is 21. See
#' `vignette("ggplot2-specs")` under `Point` for more about this.
#' @param ... arguments passed to [ggplot2::geom_point()]
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   grattantheme::grattan_point_filled()

grattan_point_filled <- function(mapping = NULL,
                                 data = NULL,
                                 size = 3,
                                 stroke = 1.5,
                                 fill = "white",
                                 shape = 21,
                                 ...) {
  ggplot2::geom_point(mapping = mapping,
                      data = data,
                      size = size,
                      stroke = stroke,
                      fill = fill,
                      shape = shape,
                      ...)
}
