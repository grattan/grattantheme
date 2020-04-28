#' Assemble a chart featuring the Grattan logo and orange line
#'
#' Takes a ggplot2 object and formats it to look like a
#' Grattan Powerpoint slide. You will rarely need to call this function
#' directly - use `grattan_save()` to save a ggplot2 object as a 'slide'-like
#' image.
#'
#' @param plot A ggplot2 plot
#' @param type Optional. If specified, must be one of "fullslide", "fullslide_169", "fullslide_44", or "blog".
#' This is used to define the size of the white border around the image.
#'
#' @return An object of class "patchwork".
#'
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "My title",
#'          subtitle = "My subtitle",
#'          caption = "My caption") +
#'     theme_grattan()
#'
#' # Create an image that includes the Grattan logo
#'
#' p_logo <- assemble_fullslide(p)
#'
#' # Create an image that's ready for the blog
#'
#' p_blog <- assemble_fullslide(p, "blog")
#'
#' @export
#' @importFrom patchwork wrap_plots wrap_elements plot_spacer plot_annotation

assemble_fullslide <- function(plot = last_plot(),
                               type) {

    # Check inputs and define plot borders ----

    if (!inherits(plot, "ggplot")) {
      stop(deparse(substitute(plot)), " is not a ggplot2 object.")
    }

    if (missing(type)) {
      top_border <- 0.15
      right_border <- 0.15
      bottom_border <- 0.05
      left_border = 0.15

    } else {
      if (!type %in% fullslide_chart_types) {
        stop(type,
             " is not a valid chart type.\nMust be one of: ",
             paste(fullslide_chart_types, collapse = ", "))
      }

      chosen_chart_type <- chart_types[chart_types$type == type, ]
      top_border <- chosen_chart_type$top_border
      right_border <- chosen_chart_type$right_border
      bottom_border <- chosen_chart_type$bottom_border
      left_border <- chosen_chart_type$left_border
    }

    # Create title and subtitle -----
    p <- plot

    stored_title <- p$labels$title
    stored_subtitle <- p$labels$subtitle

    p$labels$title <- NULL
    p$labels$subtitle <- NULL

    title_font_size <- 18

    toptitle <- grid::grid.text(label = stored_title,
                               x = unit(0, "npc"),
                               y = unit(0.1, "npc"),
                               just = c("left", "bottom"),
                               draw = FALSE,
                               gp = gpar(col = "black",
                                         fontsize = title_font_size,
                                         fontface = "bold",
                                         lineheight = 0.9,
                                         fontfamily = "sans"))

    topsubtitle <- grid::grid.text(label = stored_subtitle,
                                   x = unit(0, "npc"),
                                   y = unit(0.925, "npc"),
                                   draw = F,
                                   just = c("left", "top"),
                                   gp = gpar(col = "black",
                                             fontsize = 18,
                                             lineheight = 0.9,
                                             fontfamily = "sans"))


    # Define additional grobs -----
    blank_grob <- rectGrob(gp = gpar(lwd = 0))

    orange_line <- grid.lines(y = c(0.5, 0.5),
                              draw = FALSE,
                              gp = gpar(col = grattan_lightorange,
                                       lwd = 2))

    orange_line_height <- 0.08

    logo_height <- 1.1
    logo_width <- 4
    logo_padding <- 0.1

    layout <- "
    T#L
    OOO
    SSS
    PPP
    "

    wrap_plots(T = wrap_elements(full = toptitle),
               L = wrap_elements(full = logogrob),
               O = wrap_elements(full = orange_line),
               S = wrap_elements(full = topsubtitle),
               P = wrap_elements(full = p),
               design = layout,
               heights = unit(c(logo_height,
                                0.001,
                                logo_height,
                                1),
                              c("cm",
                                "cm",
                                "cm",
                                "null")),
               widths = unit(c(1,
                               logo_padding,
                               logo_width),
                             c("null",
                               "cm",
                               "cm"))
               ) +
      plot_annotation(theme = theme(plot.margin = margin(top_border,
                                                         right_border,
                                                         bottom_border,
                                                         left_border,
                                                         "cm")))
}
