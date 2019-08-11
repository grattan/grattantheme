#' Create a Grattan-appropriate palette for your chart.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is 1,
#' maximum is 10. Using more than 6 is not recommended. If you don't specify `n`,
#' a five-colour palette will be used, which may not look right. Specify `n`.
#'
#' By default, n = 2 will give you light orange and dark orange.
#' Use n = "2a" if you want light orange and red.
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#' standard colour order. Standard colour order runs from light to dark. If you
#' set reverse to TRUE, colours will run from dark to light.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     scale_colour_manual(values = grattan_pal(n = 3))
#'
#' p
#'
#' # Alternatively, use grattan_colour_manual(), which is a wrapper
#' # around scale_colour_manual():
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     grattan_colour_manual(n = 3)
#'
#' p
#'
#' @export

grattan_pal <- function(n = 0, reverse = FALSE){

  if(n == 0){
    n <- 5
    "Your chart will probably look better if you specify n in grattan_pal()."
  }

  if(n > 6 & n <= 10){
    warning("Using more than six colours is not recommended.")
  }

  if(n > 10 & n != "2a"){
    stop(paste0("You've requested ", n, " colours; grattan_pal() only supports up to 10."))
  }

  if(n == 1){
    palette <- grattantheme::grattan_lightorange
  } else if(n == "2a"){
    palette <- c(grattantheme::grattan_lightorange, grattantheme::grattan_darkorange)
  } else if(n == 2){
    palette <- c(grattantheme::grattan_lightorange, grattantheme::grattan_red)
  } else if(n == 3){
    palette <- c(grattantheme::grattan_yellow, grattantheme::grattan_lightorange,
                 grattantheme::grattan_darkorange)
  } else if(n == 4){
    palette <- c(grattantheme::grattan_yellow, grattantheme::grattan_lightorange,
                 grattantheme::grattan_darkorange, grattantheme::grattan_red)
  } else if(n == 5){
    palette <- c(grattantheme::grattan_yellow, grattantheme::grattan_lightorange,
                 grattantheme::grattan_darkorange, grattantheme::grattan_red,
                 grattantheme::grattan_darkred)
  } else if(n == 6){
    palette <- c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                 grattantheme::grattan_red, grattantheme::grattan_darkred)
  } else if(n == 7){
    palette <- c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                 grattantheme::grattan_red, grattantheme::grattan_darkred, "black")
  } else if(n == 8){
    palette <- c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                 grattantheme::grattan_red, grattantheme::grattan_darkred, "black",
                 grattantheme::grattan_grey4)
  } else if(n == 9){
    palette <- c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                 grattantheme::grattan_red, grattantheme::grattan_darkred, "black",
                 grattantheme::grattan_grey4, grattantheme::grattan_grey3)
  } else if(n == 10){
    palette <- c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                 grattantheme::grattan_red, grattantheme::grattan_darkred, "black",
                 grattantheme::grattan_grey4, grattantheme::grattan_grey3,
                 grattantheme::grattan_grey2)
  }


    if(reverse == TRUE){
      rev(palette)
    } else {
      palette
    }


}
