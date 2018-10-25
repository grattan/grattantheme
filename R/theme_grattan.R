#' Create a ggplot2 theme consistent with the Grattan style guide.
#'
#' @param base_size Size for text elements. Defaults to 18.
#' @param base_family Font family for text elements. Defaults to "sans", indistinguishable from Arial.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#' p
#' @export

requireNamespace(c("ggthemes", "ggplot2"), quietly = TRUE)


theme_grattan <- function (base_size = 18, base_family = "sans") {

  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(
     line = element_line(colour = grattan_gridlinegrey,
                         size = 0.5),
     rect = element_rect(fill = "white",
                         linetype = 0,
                         colour = NA),
     text = element_text(family = base_family,
                         colour = "black",
                         size = base_size),
     panel.grid.major = element_line(colour = grattan_gridlinegrey),
     panel.grid.major.x = element_blank(),
     axis.title = element_text(size = rel(1)),
     axis.text = element_text(size = rel(1)),
     axis.title.y = element_blank(),
     axis.ticks.y = element_blank(),
     axis.ticks.x = element_line(colour = "black"),
     axis.line.y = element_blank(),
     axis.line.x = element_line(colour = "black"),
     legend.background = element_blank(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     legend.key.height=unit(1, "line"),
     legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm"),
     legend.spacing = unit(0,"cm"),
     legend.title = element_blank(),
     panel.grid = element_line(colour = NULL),
     panel.grid.minor = element_blank(),
     plot.title = element_text(face = "bold",
                               size = rel(1),
                               hjust = 0,
                               colour = grattan_grey_title),
     plot.subtitle = element_text(colour = grattan_grey_title,
                                  hjust = 0),
     plot.margin = unit(c(0.1, 0.75, 0.1, 0.25), "lines"),
     strip.background = element_rect(),
     strip.text = element_text(size = rel(1)),
     plot.caption = element_text(family = base_family,
                                 size = rel(0.555),
                                 hjust = 0,
                                 colour = "black",
                                 face = "italic",
                                 margin = margin(t = 15))))
}
