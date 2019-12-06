# Assemble an image resembling a full PPT slide containing a Grattan chart. Not
# intended to be called directly, this function is called from grattan_save()

#' @import ggplot2
#' @import gridExtra

create_fullslide <- function(object,
                             type,
                             height = NULL,
                             warn_labs = TRUE,
                             print_object = FALSE) {

  if (missing(type)) {
    stop("You must specify a plot type.")
  }

  p <- object

  if (isFALSE(inherits(p, "ggplot_built"))) {
    p_built <- ggplot_build(p)

  } else {
    p_built <- p
  }

  p <- p_built$plot
  p <- wrap_labs(p, type)

  stored_title <- p$labels$title
  stored_subtitle <- p$labels$subtitle
  stored_caption <- p$labels$caption

  if (stored_title == "\n") {
    if (warn_labs) {
      message("Your plot has no title, which is weird for a fullslide.",
              "\nAdd a title using +labs(title = 'Title')")
    }
    stored_title <- ""
  }

  if (is.null(stored_subtitle) | stored_subtitle == "") {
    if (warn_labs) {
      message(paste0("Your plot has no subtitle, which is weird for type = ",
                     type,
                     "\nConsider adding a subtitle using",
                     "labs(subtitle = 'Text')"))
    }

    stored_subtitle <- NULL
  }

  if (stored_caption == "") {
    if (warn_labs) {
      message("Your plot has no caption, which is weird for full slide charts.",
              "\nConsider adding a caption using labs(caption = 'Text')")
    }
    stored_caption <- ""
  }

  # remove title and subtitle on chart
  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  # how many lines in the subtitle?

  subtitle_lines <- ceiling(nchar(stored_subtitle) /
                              chart_types$subtitle[chart_types$type == type])

  # convert to gtable
  p_built$plot <- p
  p <- p_built
  p <- ggplot2::ggplot_gtable(p)

  # left align caption
  p$layout[which(p$layout$name == "caption"),
           c("l", "r")] <- c(2, max(p$layout$r))

  # create new ggplot object with just the title
  toptitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(title = stored_title) +
    theme_grey(base_family = "sans",
               base_size = ifelse(type == "fullslide_169",
                                     24, 18)) +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(colour = "black",
                                                      hjust = 0,
                                                      vjust = 0,
                                                      face = "bold",
                                                      size = ggplot2::rel(1)),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0),
                                               units = "cm"))

  # create new ggplot object with just the subtitle
  topsubtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(subtitle = stored_subtitle) +
    theme_grey(base_family = "sans", base_size = 18) +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(colour = "black",
                                                         hjust = 0,
                                                         vjust = 0),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"))

  # create new grob of whitespace to be the border
  border <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "white"))

  # create new grob of solid orange to be the horizontal line
  linegrob <- grid::rectGrob(gp = grid::gpar(fill = "#F3901D", col = "white"))

  # define heights of elements
  if (is.null(height)) {
    height <- chart_types$height[chart_types$type == type]
  }

  blog_border <- 0.15

  top_border_height <- ifelse(type == "blog", blog_border, 0.70)
  header_height <- 1.75
  linegrob_height <- 0.1
  subtitle_height <- ifelse(is.null(stored_subtitle), 0.21,
                            ifelse(subtitle_lines == 1, 1.76 / 2, 1.76))
  bottom_border_height <- ifelse(type == "blog", blog_border, 0.24)

  non_plot_height <- sum(top_border_height, header_height, linegrob_height,
                         subtitle_height, bottom_border_height)

  plot_height <- height - non_plot_height

  # define widths of elements
  width <- chart_types$width[chart_types$type == type]

  plot_width <- if (type %in% c("fullslide", "fullslide_44")) {
    22.16
  } else if (type == "fullslide_169") {
    30
  } else if (type == "blog") {
    width - (blog_border * 2)
  }

  width_leftborder <- ifelse(grepl("fullslide", type),
                             (width - plot_width) / 2,
                             blog_border)

  width_rightborder <- width_leftborder

  # create header (= title + logo side by side)

  width_logo <- 4.57

  width_title <- plot_width - width_logo

  header <- gridExtra::arrangeGrob(grobs = list(toptitle, logogrob),
                                   ncol = 2,
                                   widths = unit(c(width_title, width_logo),
                                                 "cm"),
                                   heights = unit(1.48,
                                                  "cm"),
                                   padding = unit(0,
                                                  "line"))

  # create main plotting area
  mainarea <- gridExtra::arrangeGrob(grobs = list(border,
                                                  header,
                                                  linegrob,
                                                  topsubtitle,
                                                  p,
                                                  border),
                                      ncol = 1,
                                      heights = unit(c(top_border_height,
                                                       header_height,
                                                       linegrob_height,
                                                       subtitle_height,
                                                       plot_height,
                                                       bottom_border_height),
                                                     "cm"),
                                      widths = unit(plot_width, "cm"))

  # create total plot

  total <- gridExtra::arrangeGrob(grobs = list(border, mainarea, border),
                                  ncol = 3,
                                  widths = unit(c(width_leftborder,
                                                  plot_width,
                                                  width_rightborder),
                                                 "cm"))

  # plot original chart again if requested
  if (print_object) {
    print(object)
  }

  ggplot2::set_last_plot(object)

  total

}
