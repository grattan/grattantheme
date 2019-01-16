#' Convenient functions to set Grattan-appropriate palettes
#'
#' @param n Numeric. The number of levels in your colour scale.
#' Minimum value is 1, maximum is 7. Passed to \code{grattan_pal};
#' see ?grattan_pal for more information.
#'
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#' standard colour order.
#'
#' @param ... arguments passed to ggplot2 scales
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'    geom_point() +
#'    grattan_colour_manual(n = 3) +
#'    theme_grattan()
#'
#' @name grattan_scale
#' @aliases NULL
NULL

#' @rdname grattan_scale
#' @import ggplot2
#' @export

grattan_colour_manual <- function(n = 0, reverse = FALSE, ...){
  ggplot2::scale_colour_manual(...,
                      values = grattantheme::grattan_pal(n = n,
                                                         reverse = reverse))
}

#' @rdname grattan_scale
#' @import ggplot2
#' @export
grattan_fill_manual <- function(n = 0, reverse = FALSE, ...){
  ggplot2::scale_fill_manual(...,
                             values = grattantheme::grattan_pal(n = n,
                                                                reverse = reverse))

}

