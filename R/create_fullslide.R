# Assemble an image resembling a full PPT slide containing a Grattan chart
# Not intended to be called directly, this function is called from grattan_save()

#' @import ggplot2
#' @import gridExtra

create_fullslide <- function(object, type){

  if(!"gg" %in% class(object)){
    stop("type = 'fullslide' only works with ggplot graph objects")
  }

  p <- object

  p <- wrap_labs(p, type)

  stored_title <- p$labels$title
  stored_subtitle <- p$labels$subtitle
  stored_caption <- p$labels$caption

  if(stored_title == "\n"){
    message("Your plot has no title, which is weird for a fullslide.\nAdd a title using +labs(title = 'Title')")
    stored_title <- ""
  }

  if(stored_subtitle == "\n"){
    message("Your plot has no subtitle, which is weird for a fullslide.\nConsider adding a subtitle using labs(subtitle = 'Text')")
    stored_subtitle <- ""
  }

  if(stored_caption == ""){
    message("Your plot has no caption, which is weird for full slide charts.\nConsider adding a caption using labs(caption = 'Text')")
    stored_caption <- ""
  }

  # remove title and subtitle on chart
  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  # left align caption
  p <- ggplot2::ggplotGrob(p)
  p$layout$l[p$layout$name == "caption"] <- 1

  # create new ggplot object with just the title
  toptitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(title = stored_title) +
    theme_grattan(base_size = ifelse(type %in% c("fullslide", "fullslide_44"),
                                     18, 24)) +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(colour = "black", hjust = 0, vjust = 0),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"))

  # create new ggplot object with just the subtitle
  topsubtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(subtitle = stored_subtitle) +
    theme_grattan() +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(colour = "black", hjust = 0, vjust = -2),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"))

  # create new grob of whitespace to be the border
  border <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "white"))

  # create new grob of solid orange to be the horizontal line
  linegrob <- grid::rectGrob(gp = grid::gpar(fill = "#F3901D", col = "white"))

  # create header (= title + logo side by side)
  width_title <- ifelse(type %in% c("fullslide", "fullslide_44"), 17.73,  25.43)

  header <- gridExtra::grid.arrange(toptitle, logogrob,
                                    ncol = 2,
                                    widths = unit(c(width_title,4.57), "cm"),
                                    heights = unit(1.48, "cm"))


  plot_height <- ifelse(type == "fullslide_44", 14.5 + (25.4-19.05), 14.5)

  # create main plotting area
  mainarea <- gridExtra::grid.arrange(border, header, linegrob, topsubtitle, p, border,
                                      ncol = 1,
                                      heights = unit(c(0.73, 1.65, 0.1, 1.73, plot_height, 0.34),
                                                     "cm"),
                                      widths = unit(ifelse(type %in% c("fullslide", "fullslide_44"),
                                                                       22.16, 30), "cm"))

  # create total plot

  width_leftborder <- ifelse(type %in% c("fullslide", "fullslide_44"),
                             (25.4 - 22.16) / 2,
                             (33.87 - 30) / 2)

  width_mainarea <- ifelse(type %in% c("fullslide","fullslide_44"), 22.16, 30)

  width_rightborder <- width_leftborder

  total <- gridExtra::grid.arrange(border, mainarea, border, ncol = 3,
                                   widths = unit(c(width_leftborder, width_mainarea, width_rightborder),
                                                 "cm"))

  # plot original chart again (so last_plot() shows this instead of topsubtitle)
  print(object)

  total

}
