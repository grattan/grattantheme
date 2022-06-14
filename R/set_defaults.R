#' Set the default ggplot2 aesthetics to QND's
#'
#' A wrapper for a bunch of other functions to set ggplot2 default aesthetics.
#' Used on load.
set_qnd_defaults <- function() {

  check_subclass <- utils::getFromNamespace("check_subclass", "ggplot2")

  defs <- all_geoms %>%
    purrr::map(purrr::safely(check_subclass),
               "Geom",
               env = parent.frame()) %>%
    purrr::map(purrr::pluck,
               "result") %>%
    purrr::map(purrr::pluck,
               "default_aes") %>%
    purrr::set_names(all_geoms)

  # Just font family to Calibri and col/fill to QND blue
  qnd_defs <- defs %>%
    purrr::map(prep_aes,
               aes = "colour",
               setting = grattan_pal(1)) %>%
    purrr::map(prep_aes,
               aes = "fill",
               setting = grattan_pal(1))

  purrr::iwalk(qnd_defs,
               ~set_geom_defaults(geom = .y,
                                  new = .x))
}

#' All ggplot geoms
#'
#' A list of ggplot2 geoms for restoring default aesthetics
#'
all_geoms <- list(
  "abline",
  "area",
  "bar",
  # "bin2d",
  "boxplot",
  "col",
  "contour",
  "contour_filled",
  # "count",
  "crossbar",
  "curve",
  "density",
  "density_2d",
  "density_2d_filled",
  "density2d",
  "density2d_filled",
  "dotplot",
  "errorbar",
  "errorbarh",
  # "freqpoly",
  "function",
  "hex",
  # "histogram",
  "hline",
  # "jitter",
  "label",
  "label_repel",
  "line",
  "linerange",
  "map",
  "path",
  "point",
  "pointrange",
  "polygon",
  # "qq",
  # "qq_line",
  "quantile",
  "raster",
  "rect",
  "ribbon",
  "rug",
  "segment",
  "sf",
  # "sf_label",
  # "sf_text",
  "smooth",
  "spoke",
  "step",
  "text",
  "text_repel",
  "tile",
  "violin",
  "vline"
)

#' Set \code{ggplot2} default colours
#'
#' Sets global options for \code{ggplot2}. If \code{type = "qnd"}, QND colours
#' will be used by default in ggplot. If there are #' more than 32 levels it
#' will switch to \code{viridis} by default, so then you must use
#' \code{scale_colour_qnd}. To restore the defaults use \code{type = "default"}.
#'
#' @param type (character) Which colours to use? One of "qnd" or "default".
#'
#' @export
#' @examples
#' \dontrun{
#' set_plot_colours(type = "default")
#' }
set_plot_colours <- function(type) {
  if (type == "qnd") {
    options(qnd_plot_opts)
    set_qnd_defaults()
    message("ggplot2 will use QND colours (in the absence of a scale_colour etc function).")
  } else if (type == "default") {

    # Set default multi-colours
    options(null_plot_opts)
    # Set default aesthetics
    geom_defaults %>%
      purrr::iwalk(~set_geom_defaults(geom = .y, new = .x))
    message("ggplot2 will use default colours (in the absence of a scale_colour etc function).")
  } else {
    message("Not a valid type. Nothing changed.")
  }

}

#' The QND plotting options
#'
#' These options set the default ggplot2 colours options for the R session.
#'
qnd_plot_opts <- list(
  ggplot2.continuous.colour = grattan_colour_manual,
  ggplot2.continuous.fill   = grattan_fill_manual,
  ggplot2.discrete.colour   = list(grattan_palette()(10),
                                   grattan_palette()(16),
                                   grattan_palette()(24),
                                   grattan_palette()(32)),
  ggplot2.discrete.fill     = list(grattan_palette()(10),
                                   grattan_palette()(16),
                                   grattan_palette()(24),
                                   grattan_palette()(32))
)

#' Prepare ggplot2 geom_ defaults
#'
#' ggplot2 geoms have default aesthetics, however they can be changed for each
#' session. This function is a generalised way to set geom defaults. It only
#' changes settings where the default is not NA; no need for \code{geom_point}
#' to have a default font family.
#'
#' @param geom (character) the geom to change (e.g. \code{point} or
#'   \code{line})
#' @param aes (character) the aesthetic to change (e.g. \code{colour} or
#'   \code{alpha})
#' @param setting (various) what to set the aesthetic (e.g. a colour or a
#'   number)
#'
#' @return the default aesthetic ready to be set
prep_aes <- function(geom,
                     aes,
                     setting) {
  #Check if the geom inherits from elsewhere
  if (is.null(geom)) {
    return(NULL)
  }

  # Check if the setting exists for this geom
  if (is.null(geom[[aes]])) {
    return(geom)
  }


  # Change the setting only is it's not NA
  if (!is.na(geom[[aes]])) {
    geom[[aes]] <- setting
  }


  return(geom)
}
