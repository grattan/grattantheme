# This function is called by `theme_grattan` but is not exported.

theme_grattan_base <- function(base_size = 18,
                               base_family = "sans",
                               base_line_size = points_to_mm(0.75),
                               base_rect_size = points_to_mm(1),
                               background = "white",
                               legend = "none",
                               panel_borders = FALSE) {

  half_line <- base_size / 2

  ret <-
    theme(
      line = element_line(
        colour = grattan_gridlinegrey,
        size = base_line_size,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = grattan_gridlinegrey,
        size = base_rect_size,
        linetype = 0
      ),
      text = element_text(
        colour = "black",
        family = base_family,
        face = "plain",
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        debug = FALSE,
        margin = margin(),
        size = base_size
      ),
      axis.line = element_line(
        size = base_line_size * (1/0.75),
        colour = "black"
      ),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(margin = margin(t = 0.8 *
                                                   half_line /
                                                   2,
                                                 b = 0),
                                 vjust = 1,
                                 lineheight = 0.7),
      axis.text.x.top = element_text(margin = margin(b = 0.8 *
                                                       half_line /
                                                       2), vjust = 0),
      axis.text.y = element_text(margin = margin(r = 0.8 *
                                                   half_line /
                                                   2), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = 0.8 *
                                                         half_line /
                                                         2), hjust = 0),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,
      axis.title = element_text(size = rel(1)),
      axis.title.x = element_text(margin = margin(t = half_line / 2),
                                  vjust = 1),
      axis.title.x.top = element_text(margin = margin(b = half_line / 2),
                                      vjust = 0),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(r = half_line /
                          2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line /
                          2),
        vjust = 0
      ),
      legend.background = element_rect(colour = NA),
      legend.spacing = unit(2 * half_line, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = margin(),
      legend.key = element_rect(fill = "white",
                                colour = "white"),
      legend.key.size = unit(1, "lines"),
      legend.key.height = NULL,
      legend.key.width = unit(0.25, "lines"),
      legend.text = element_text(size = rel(1),
                                 margin = margin(l = 0,
                                                 r = base_size / 4, unit = "pt")),
      legend.text.align = 0,
      legend.title = element_blank(),
      legend.title.align = NULL,
      legend.position = legend,
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box = "vertical",
      legend.box.margin = margin(0, 0,
                                 0, 0, "cm"),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(2 * half_line, "pt"),
      panel.background = element_rect(fill = "white",
                                      colour = NA),
      panel.border = element_blank(),
      panel.grid = element_line(colour = grattan_gridlinegrey),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5,
                           "lines"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_rect(),
      strip.text = element_text(
        size = rel(1),
        margin = margin(0.8 * half_line,
                        0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
      strip.text.x = NULL,
      strip.text.y = element_text(angle = -90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(half_line / 2,
                                   "pt"),
      strip.switch.pad.wrap = unit(half_line / 2,
                                   "pt"),
      plot.background = element_rect(),
      plot.title = element_text(
        size = rel(1),
        hjust = 0,
        vjust = 1,
        colour = grattan_grey_title,
        face = "bold",
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        colour = grattan_grey_title,
        hjust = 0,
        vjust = 1,
        margin = margin(t = 0,
                        r = 0,
                        b = base_size * .75,
                        l = 0,
                        unit = "pt")
      ),
      plot.caption = element_text(
        family = base_family,
        size = rel(0.555),
        hjust = 0,
        colour = "black",
        face = "italic",
        margin = ggplot2::margin(t = half_line)
      ),
      plot.tag = element_text(
        size = rel(1.2),
        hjust = 0.5,
        vjust = 0.5
      ),
      plot.tag.position = "topleft",
      plot.margin = unit(c(0.5, 0.6, 0.1, 0.01), "lines"),
      complete = TRUE
    )

  # add panel borders if the user requests them
  if (panel_borders) {
    ret <- ret +
      theme(panel.background = element_rect(linetype = 1,
                                            colour = "black"))
  }


  if (background == "orange" |  background == "box") {
    ret <- ret +
      ggplot2::theme(rect = ggplot2::element_rect(fill = grattantheme::grattan_orange_alpha))
  }

  ret

}
