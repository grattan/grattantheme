#' Create a ggplot2 theme consistent with the Grattan style guide.
#' @name theme_grattan
#' @param base_size Size for text elements. Defaults to 18, as per the Grattan
#' style guide.
#' @param base_family Font family for text elements. Defaults to "sans",
#' indistinguishable from Arial.
#' @param flipped FALSE by default. Set to TRUE if using coord_flip(). If set to
#' TRUE, the theme will show a vertical axis line, ticks & panel grid, while
#' hiding the horizontals.
#' @param background "white" by default. Set to "orange" if you're making a chart
#' to go in a Grattan report box.
#' @param legend "off" by default. Set to "bottom", "left", "right" or "top" as
#' desired.
#'
#' @importFrom ggthemes theme_foundation
#' @import ggplot2
#'
#' @examples
#'
#' # This function goes most of the way to making your charts Grattan-y,
#' # but manual tweaking will almost definitely be required
#'
#' # A minimal example:
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' # To ensure your x-axis is at zero (or some other value you choose),
#' # you may need to manually tweak the scale of the y-axis.
#' # Use scale_y_continuous_grattan() for some sensible default values, which
#' # may need further tweaking.
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     scale_y_continuous_grattan() +
#'     theme_grattan()
#'
#' # You'll notice in the example above that the top of the chart now looks good;
#' # the bottom has two
#' # points that are half hanging off the axis. Try the following, substituing
#' # any value (incl. 0) for 10 as you like:
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     theme_grattan()
#'
#' # An example with colours follows. See ?grattan_pal for more information and
#' # options.
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan()
#'
#' # The legend is off by default. You may wish to turn it on. Here's how:
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan() +
#'     theme(legend.position = "bottom")
#'
#' # The flipped = TRUE option makes things easier when using coord_flip, as in:
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan(flipped = TRUE) +
#'     theme(legend.position = "bottom") +
#'     coord_flip()
#'
#'
#' # Making a chart to go in a box? Then you'll want the background = "orange"
#' # option, as in:
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan(flipped = TRUE, background = "orange") +
#'     theme(legend.position = "bottom") +
#'     coord_flip()
#'
#'
#' # Want to make a 'self-contained' chart that includes a title/subtitle/caption,
#' # eg. to go on the Grattan Blog? If so, just add them - they'll be properly
#' # left-aligned when you save them with grattan_save(), like this:
#'
#'  ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan() +
#'     labs(title = "Title goes here",
#'          subtitle = "Subtitle goes here",
#'          caption = "Notes: Notes go here\nSource: Source goes here")
#'
#'  # The plot above won't look right in RStudio's viewer - the text is
#'  # aligned to the left of the plot area, not the image. Once you save it,
#'  # the file should have properly-aligned text:
#'
#'  grattan_save("your_file.png")
#'
#' @export

# requireNamespace(c("ggthemes", "ggplot2"), quietly = TRUE)


theme_grattan <- function(base_size = 18,
                           base_family = "sans",
                           flipped = FALSE,
                           background = "white",
                           legend = "none") {

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
          axis.line.y = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
          axis.ticks = ggplot2::element_line(colour = "black"),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
          # style guide:
          # "there is no need to label the x-axis unless the units are not obvious"
          axis.title.x = ggplot2::element_text(),
          axis.title.y = ggplot2::element_blank(),
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
          panel.grid.major.x = ggplot2::element_blank(),
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
                                       hjust = 0),
          plot.caption = element_text(family = base_family,
                                      size = rel(0.555),
                                      hjust = 0,
                                      colour = "black",
                                      face = "italic",
                                      margin = ggplot2::margin(t = 15)),
          plot.margin = unit(c(0.1, 0.75, 0.1, 0.25) , "lines"),
          complete = TRUE)

  # Define defaults for individual geoms in a style guide-consistent way
  ggplot2::update_geom_defaults("point", list(colour = grattantheme::grattan_lightorange,
                                              size = 4 / .pt ))
  ggplot2::update_geom_defaults("bar", list(colour = "white",
                                            fill = grattantheme::grattan_lightorange,
                                            size = 0.75 / .pt ))
  ggplot2::update_geom_defaults("col", list(colour = "white",
                                            fill = grattantheme::grattan_lightorange,
                                            size = 0.75 / .pt ))
  ggplot2::update_geom_defaults("line", list(colour = grattantheme::grattan_lightorange,
                                             size = 3 / .pt))
  ggplot2::update_geom_defaults("text", list(colour = "black",
                                             size = 18 / .pt))
  ggplot2::update_geom_defaults("smooth", list(colour = grattantheme::grattan_lightorange,
                                               fill = grattantheme::grattan_lightorange))

  if (flipped == TRUE) {
    ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       axis.line.x = ggplot2::element_blank(),
                       axis.line.y = ggplot2::element_line(),
                       axis.ticks.y = ggplot2::element_line(),
                       axis.ticks.x = ggplot2::element_blank())
  }
  if (background == "orange") {
    ret <- ret + ggplot2::theme(rect = ggplot2::element_rect(fill = grattantheme::grattan_orange_alpha))
  }

  ret
}
