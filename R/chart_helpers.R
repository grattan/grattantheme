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


#' Make the ANZSIC and ANZSCO classifications Paul-compliant
#'
#' This inserts oxford commas and and sorts out the capitalisation so that ANZSCO and
#' ANZSIC classifications are compliance with Paul's standards.
#'
#' @param classification (character) the industry classification to be paulified
#' @param remove_services (logical) whether to remove the word services in the classification
#'
#' @return a character
#' @export
#'
#' @examples
#'
#' \dontrun{
#'     strayr::anzsic2006  %>%
#'        mutate(industry = paulify_classifications(anzsic_division))
#' }
#'
paulify_classifications <- function(classification, remove_services = FALSE) {

  classification <- classification %>%
    as.character() %>%
    stringr::str_to_sentence() %>%
    stringr::str_replace("(I|i)ct(?=[\\s,])", "ICT")

  if (remove_services) x <- str_remove(x, " services$")

  # add oxford comma
  needs_oxford_comma <- ".*[,]+.*and.*"

  dplyr::if_else(stringr::str_detect(classification, needs_oxford_comma),
          stringr::str_replace(classification, " and ", ", and "),
          classification)
}
