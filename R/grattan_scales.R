#' Convenient functions to set Grattan-appropriate palettes
#'
#' @param n Numeric. The number of levels in your colour scale.
#' Minimum value is 1, maximum is 7. Passed to \code{grattan_pal};
#' see ?grattan_pal for more information.
#'
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#' standard colour order.
#'
#' @param faded Logical. FALSE by default. Setting to TRUE returns faded variations
#' of the standard colours.
#'
#' @param discrete Logical. TRUE by default. Setting to FALSE generates a
#' continuous colour scale.
#'
#' @param palette Sets the colours that will form the continuous palette when discrete = FALSE. One of:
#'
#' \itemize{
##' \item{"full"}{The default. Red, dark orange, light orange, yellow, light yellow}
##' \item{"full_f"}{ faded version of "full"}
##' \item{"light"}{ light orange, yellow, light yellow}
##' \item{"dark"}{ red, dark orange, light orange}
##' \item{"diverging"}{ red, faded red, white, faded light orange, light orange}
##' \item{"grey"}{ grey 1, grey 2, grey 3, grey 4, grey 5}
##'}
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


grattan_colour_manual <- function(n = 0,
                                  reverse = FALSE,
                                  discrete = TRUE,
                                  faded = FALSE,
                                  palette = "full", ...){
  if (discrete) {
    return(
  ggplot2::scale_colour_manual(...,
                      values = grattantheme::grattan_pal(n = n,
                                                         reverse = reverse,
                                                         faded = faded))
    )
  }

  if (!discrete) {
  pal <- grattan_palette(palette = palette, reverse = reverse)
  return(ggplot2::scale_color_gradientn(colours = pal(256), ...))
  }


}

#' @rdname grattan_scale
#' @import ggplot2
#' @export

grattan_fill_manual <- function(n = 0, reverse = FALSE,
                                discrete = TRUE,
                                faded = FALSE,
                                palette = "full", ...){
  if (discrete) {
    return(
      ggplot2::scale_fill_manual(...,
                                 values = grattantheme::grattan_pal(n = n,
                                                                    reverse = reverse,
                                                                    faded = faded))
    )
  }

  if (!discrete) {
    pal <- grattan_palette(palette = palette, reverse = reverse)
    return(ggplot2::scale_fill_gradientn(colours = pal(256), ...))
  }

}


# Generates a full palette
grattan_palette <- function(palette = "full", reverse = FALSE, ...) {

  pal <- grattantheme::grattan_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
