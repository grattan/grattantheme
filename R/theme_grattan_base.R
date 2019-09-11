# This function is called by `theme_grattan` but is not exported.

theme_grattan_base <- function(base_size = 18,
                               base_family = "sans",
                               background = "white",
                               legend = "none",
                               panel_borders = FALSE) {

  ret <-
    ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(line = ggplot2::element_line(colour = grattantheme::grattan_gridlinegrey,
                                                # style guide says axis line = 0.75 points, need to convert to mm
                                                size = 0.75 / (.pt*72.27/96) ),
                   rect = ggplot2::element_rect(fill = "white",
                                                colour = NA,
                                                linetype = 0),
                   text = ggplot2::element_text(colour = "black", size = base_size),
                   ## Axis
                   axis.line = ggplot2::element_line(size = ggplot2::rel(1),
                                                     colour = "black"),
                   # moved to theme_grattan()
                   # axis.line.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
                   axis.ticks = ggplot2::element_line(colour = "black"),
                   # moved to theme_grattan()
                   # axis.ticks.y = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
                   # style guide:
                   # "there is no need to label the x-axis unless the units are not obvious"
                   axis.title.x = ggplot2::element_text(),
                   # moved to theme_grattan()
                   # axis.title.y = ggplot2::element_blank(),
                   #axis.ticks.length = unit( -base_size * 0.5, "points"),
                   legend.background = ggplot2::element_rect(),
                   #legend.key = element_rect(linetype = 0),
                   #legend.key.size = unit(1.2, "lines"),
                   legend.key.width = NULL,
                   legend.text = ggplot2::element_text(size = ggplot2::rel(1),
                                                       margin = ggplot2::margin(l = base_size / 4,
                                                                                r = base_size, unit = "pt")),
                   legend.text.align = 0,
                   legend.title.align = NULL,
                   legend.position = legend,
                   legend.direction = "horizontal",
                   legend.box = "vertical",
                   legend.spacing = ggplot2::unit(base_size / 18,"cm"),
                   legend.justification = "center",
                   legend.key.height = ggplot2::unit(1, "line"),
                   legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
                   legend.title = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(),
                   # moved to theme_grattan()
                   # panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0.25, "lines"),
                   strip.background = ggplot2::element_rect(),
                   strip.text = ggplot2::element_text(size = ggplot2::rel(1)),
                   plot.background = ggplot2::element_rect(),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1),
                                                      hjust = 0,
                                                      colour = grattantheme::grattan_grey_title,
                                                      face = "bold"),
                   plot.subtitle = element_text(colour = grattantheme::grattan_grey_title,
                                                vjust = 1,
                                                margin = margin(t = 0,
                                                                r = 0,
                                                                b = base_size * .75,
                                                                l = 0,
                                                                unit = "pt"),
                                                hjust = 0),
                   plot.caption = element_text(family = base_family,
                                               size = rel(0.555),
                                               hjust = 0,
                                               colour = "black",
                                               face = "italic",
                                               margin = ggplot2::margin(t = 15)),
                   plot.margin = unit(c(0.5, 0.6, 0.1, 0.01) , "lines"),
                   complete = TRUE)

  # add panel borders if the user requests them
  if(panel_borders) {
    ret <- ret +
      theme(panel.background = element_rect(linetype = 1,
                                            colour = "black"))
  }


  if (background == "orange" |  background == "box") {
    ret <- ret + ggplot2::theme(rect = ggplot2::element_rect(fill = grattantheme::grattan_orange_alpha))
  }

  ret

}




