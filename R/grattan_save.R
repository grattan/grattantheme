#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#'
#' @param filename Required. The filename (including path where necessary) for your image on disk. The extension defines the file type.
#' @param object Defaults to last_plot(). Can specify a different ggplot2 object to be saved.
#' @param height Default is 14.5cm, Grattan default.
#' @param width Default is 22.16cm, Grattan default.
#' @examples
#' library(ggplot)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' grattan_save("p.png", p)
#' @export
#'

requireNamespace("ggplot2", quietly = TRUE)

grattan_save <- function(filename, object = last_plot(), height = 14.5, width = 22.16) {
  ggsave(filename, object,
         width = width, height = height, units = "cm", dpi = "retina")
}
