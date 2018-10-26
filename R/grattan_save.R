#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#'
#' @param filename Required. The filename (including path where necessary) for your image on disk. The extension defines the file type.
#' @param object Defaults to last_plot(). Can specify a different ggplot2 object to be saved.
#' @param height Default is 14.5cm, Grattan normal size default. See \code{type}.
#' @param width Default is 22.16cm, Grattan normal size default. See \code{type}.
#' @param type Sets height and width to Grattan defaults for one of c("normal", "tiny", "wholecolumn", "fullpage"). Normal is the default and uses default height and width. Tiny uses height of 11.08cm and default width. Whole-column uses height of 22.16cm and default width. Full-page uses height 22.16cm and width of 44.32cm.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' grattan_save("p.png", p)
#' @export
#'

requireNamespace("ggplot2", quietly = TRUE)

grattan_save <- function(filename, object = last_plot(), height = 14.5, width = 22.16, type = "normal") {
  
  if (type == "tiny")    height = 11.08
  if (type == "wholecolumn") height = 22.16
  if (type == "fullpage")  {
    height = 22.16
    width  = 44.32
  }

  ggsave(filename, object,
         width = width, height = height, units = "cm", dpi = "retina")
}
