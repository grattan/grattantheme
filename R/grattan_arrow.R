#' Create arrows in the Grattan style
#'
#' `grattan_arrow()` adds arrows to lines using
#' `grid::arrow()`. The arrows will have a closed arrow head, in keeping with Grattan
#' Institute's visual style.
#'
#' @param ... Arguments passed on to either `grid::arrow()`.
#'  See \code{?arrow}.
#' @param length A unit specifying the length of the arrow head (from tip to base). Default 0.5.
#' @param type Arrow head type. Default is "closed".
#' @param angle Angle of arrow head. Default is 20.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Create a dataset -- mtcars with rownames as column
#' mt <- mtcars
#' mt$car <- rownames(mt)
#'
#' # Make a basic chart with the mtcars data
#' p <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
#'  geom_point() +
#'  theme_grattan()
#'
#'# Add a simple callout arrow to the chart, using geom_segment, with arrow = grattan_arrow()
#'p +
#'geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
#'  arrow = grattan_arrow(),
#'  colour = grattan_red)
#'
#'# Alternatively we could add a curved arrow, using geom_cuve()
#' p +
#' geom_curve(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
#'  arrow = grattan_arrow(),
#'  curvature = 0.3,
#'  colour = grattan_red)
#'
#'
#' @importFrom grid arrow
#' @name grattan_arrow
#'
#' @export
#'
grattan_arrow <- function(...,
                          type = "closed",
                          angle = 20,
                          length = unit(0.5, "lines")) {
  grid::arrow(...,
              type = type,
              angle = angle,
              length = length
  )
}



