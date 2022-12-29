#' Check that your chart looks alright in the different chart formats
#'
#' This function will open your plot in a new window with the aspect ratio of the
#' chart type selected
#'
#' @inheritParams grattan_save
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' check_chart_aspect_ratio()}
#'
check_chart_aspect_ratio <- function(object = ggplot2::last_plot(),
                                     type = "normal") {

  height <- chart_types_inc_deprecated$height[chart_types_inc_deprecated$type == type]

  width <- chart_types_inc_deprecated$width[chart_types_inc_deprecated$type == type]

  filename <- file.path(tempdir(), "temp.png" )

  plot <- object  +
    ggplot2::theme(plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          plot.caption = ggplot2::element_blank())

  ggplot2::ggsave(filename,
                  plot = plot,
                  width = width,
                  height = height,
                  units = "cm")

  fs::file_show(filename)
}
