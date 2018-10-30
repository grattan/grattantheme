#' Create a Grattan-appropriate palette for your chart.
#'
#' @param n The number of levels in your colour scale. Minimum value is 1, maximum is 6. If you don't specify `n`, a six-colour palette will be used, which may not look right. Specify `n`.
#' @param reverse FALSE by default. Non-reverse order runs from light to dark.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     scale_colour_manual(grattan_pal(n = 3))
#'
#' p
#'
#' @export

grattan_pal <- function(n = 0, reverse = FALSE){
  if(n == 1){
    palette <- grattan_lightorange
  } else if(n == 2){
    palette <- c(grattan_lightorange, grattan_red)
  } else if(n == 3){
    palette <- c(grattan_yellow, grattan_darkorange, grattan_darkred)
  } else if(n == 4){
    palette <- c(grattan_yellow, grattan_lightorange, grattan_darkorange, grattan_darkred)
  } else if(n == 5){
    palette <- c(grattan_yellow, grattan_lightorange, grattan_darkorange, grattan_red, grattan_darkred)
  } else if(n == 6){
    palette <- c(grattan_lightyellow, grattan_yellow, grattan_lightorange, grattan_darkorange, grattan_red, grattan_darkred)
  } else if(n == 0){
    palette <- c(grattan_lightyellow, grattan_yellow, grattan_lightorange, grattan_darkorange, grattan_red, grattan_darkred)
    warning("If you have fewer than 6 colours, your chart will look better if you specify n in grattan_pal().")
  }

if(reverse == TRUE){
  rev(palette)
} else {
  palette
}

}
