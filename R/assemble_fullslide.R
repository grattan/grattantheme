
#' @importFrom patchwork wrap_plots wrap_elements plot_spacer

assemble_fullslide <- function(plot = last_plot(),
                               width,
                               height,
                               title_font_size = 18,
                               top_border = 0.65,
                               right_border = 1.46,
                               bottom_border = 0.1,
                               left_border = 1.78) {

    p <- plot

    stored_title <- p$labels$title
    stored_subtitle <- p$labels$subtitle

    p$labels$title <- NULL
    p$labels$subtitle <- NULL

    toptitle <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::labs(title = stored_title) +
      theme_grey(base_family = "sans",
                 base_size = title_font_size) +
      ggplot2::theme(rect = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(colour = "black",
                                                        hjust = 0,
                                                        vjust = 1,
                                                        face = "bold",
                                                        size = ggplot2::rel(1)),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0),
                                                 units = "cm"),
                     plot.title.position = "plot")

    topsubtitle <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::labs(subtitle = stored_subtitle) +
      theme_grey(base_family = "sans", base_size = 18) +
      ggplot2::theme(rect = ggplot2::element_blank(),
                     plot.subtitle = ggplot2::element_text(colour = "black",
                                                           hjust = 0,
                                                           vjust = 0),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"),
                     plot.title.position = "plot")

    blank_grob <- rectGrob(gp = gpar(lwd = 0))

    orange_line <- linesGrob(y = c(1, 1),
                             gp = gpar(col = grattan_lightorange,
                                       lwd = 3))

    title_height <- 1.54
    logo_height <- 1.1
    logo_width <- 4
    logo_vertical_padding <- (title_height - logo_height) / 2

    standard_plot_width <- 22.16
    standard_full_width <- 25.4
    standard_title_width <- 17.73

    orange_line_height <- 0.08

    logo_horizontal_padding <- (standard_plot_width - logo_width - standard_title_width)

    layout <- "
    ####B
    #T###
    #T#L#
    #T###
    #OOO#
    #SSS#
    #PPP#
    #####
    "

    wrap_plots(B = blank_grob,
               T = toptitle,
               L = wrap_elements(full = logogrob),
               O = wrap_elements(full = orange_line),
               S = topsubtitle,
               P = p,
               design = layout,
               widths = unit(c(left_border,
                               1,
                               logo_horizontal_padding,
                               logo_width,
                               right_border),
                             c("cm",
                               "null",
                               "cm",
                               "cm",
                               "cm")),
               heights = unit(c(top_border,
                                logo_vertical_padding,
                                logo_height,
                                logo_vertical_padding,
                                orange_line_height,
                                title_height,
                                1,
                                bottom_border),
                              c("cm",
                                "cm",
                                "cm",
                                "cm",
                                "cm",
                                "cm",
                                "null",
                                "cm")))


    # logo_with_spacing <- wrap_plots(
    #   wrap_elements(full = blank_grob),
    #   wrap_elements(full = logogrob),
    #   wrap_elements(full = blank_grob),
    #   heights = unit(c(logo_vertical_padding,
    #                    logo_height,
    #                    logo_vertical_padding),
    #                  "cm")
    # )
    #
    # title_with_logo <- wrap_plots(
    #   toptitle,
    #   logo_with_spacing,
    #   nrow = 1,
    #   heights = unit(title_height,
    #                  "cm"),
    #   widths = unit(c(1, 4),
    #                 c("null",  "cm"))
    # )
    #
    # title_logo_line_subtitle <- wrap_plots(
    #   title_with_logo,
    #   wrap_elements(full = linesGrob(y = c(1, 1),
    #                                  gp = gpar(col = grattan_lightorange,
    #                                            lwd = 3))),
    #   topsubtitle,
    #   ncol = 1,
    #   heights = unit(c(title_height, 0.08, 1),
    #                  c("cm", "cm", "null"))
    # )
    #
    # central_column <- wrap_plots(title_logo_line_subtitle,
    #                              p,
    #                              ncol = 1,
    #                              heights = unit(c(1.75, 1),
    #                                             c("cm", "null")))
    #
    # title_with_logo

}
