#' Add a watermark annotation layer for a ggplot2 object
#' @name watermark
#' @param watermark String to be added as watermark
#' @param fontsize Font size
#' @param colour Font colour
#' @param alpha Alpha (transparency; lower number = more transparent)
#' @param fontface Font face ("bold" by default)
#' @param angle Angle of the watermark
#' @export
#' @examples
#' # First, start with a plot:
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + theme_grattan()
#'
#' # Then add a watermark:
#'
#' p + watermark("DRAFT")

watermark <- function(watermark, fontsize = 120,
                          colour = "grey90", alpha = 0.1,
                          fontface = "bold", angle = 22) {
  watermark_grob <- textGrob(watermark, gp = gpar(fontsize = fontsize, colour = colour,
                                      alpha = alpha, fontface = fontface),
                   rot = angle)

  annotation_custom(grob = watermark_grob)
}
