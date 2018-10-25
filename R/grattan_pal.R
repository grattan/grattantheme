#' Create a Grattan-appropriate palette for your chart. Based on Hugh Parsonage's function.
#'
#' @param n The number of levels in your colour scale. Acceptable values are between 1 and 6.
#' @param reverse FALSE by default. Non-reverse order runs from light to dark.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     scale_colour_manual(grattan_pal(n = 3))
#'
#' @export

grattan_pal <- function(n, reverse = FALSE){
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
  }

if(reverse == TRUE){
  rev(palette)
} else {
  palette
}

}
