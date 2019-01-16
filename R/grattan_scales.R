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

#' @import ggplot2
#' @export

grattan_colour_manual <- function(n = 0, reverse = FALSE, ...){
  ggplot2::scale_colour_manual(...,
                      values = grattantheme::grattan_pal(n = n,
                                                         reverse = reverse))
}


