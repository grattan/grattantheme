#' Convenience function to create large points with white centres
#'
#' @param size Size of the point
#' @param stroke width of the line around the outside of the point
#' @param fill The colour on the inside of the point
#' @param shape The shape of the point; default is 21. See
#' `vignette("ggplot2-specs")` under `Point` for more about this.
#' @param ... arguments passed to `ggplot2::geom_point()`
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   grattantheme::grattan_point_filled()

grattan_point_filled <- function(size = 3,
                                 stroke = 1.5,
                                 fill = "white",
                                 shape = 21,
                                 ...) {
  ggplot2::geom_point(size = size,
                      stroke = stroke,
                      fill = fill,
                      shape = shape,
                      ...)
}
