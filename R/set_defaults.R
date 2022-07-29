#### Load colours and palettes


#' The QND colours
#'
#' These colours are used in \code{qnd_plot} and form the basis for our
#' palettes.
#'
#' @export
#'
qnd_colours <- c(
  grattan_lightyellow = "#FFE07F",
  grattan_yellow = "#FFC35A",
  grattan_orange = "#F68B33",
  grattan_darkorange = "#D4582A",
  grattan_red = "#A02226",
  grattan_darkred = "#621214",
  grattan_blue = "#A3C7DF",
  grattan_darkblue = "#3E81CE",
  grattan_lightgrey = "#828282",
  grattan_darkgrey <- "#575757"
)

#' Extract QND colours as hex codes
#'
#' This function allows you to get the hex code associated with one or more QND
#' colours. It's often used inside \code{qnd_plot} to set specific colours for
#' certain series.
#'
#' @param ... The given name/s of a QND colour (eg 'astronaut blue' or 'iron')
#'
#' @examples
#' get_qnd_hex("persian green")
#' get_qnd_hex("midnight", "mango tango")
#'
#' @export
get_qnd_hex <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return(qnd_colours)

  qnd_colours[cols]
}

#' Predefined QND colours combined into palettes
#'
#' This is a list of QND colours combined into palettes. The palettes are used
#' for different plots and maps.
#' @export
qnd_palettes <- list(
  `graph`      = get_qnd_hex( "grattan_lightyellow", "grattan_yellow", "grattan_orange",
                              "grattan_darkorange", "grattan_red",  "grattan_darkred",
                            "grattan_blue", "grattan_darkblue", "grattan_lightgrey", "grattan_darkgrey"),
  `sequential` = get_qnd_hex("grattan_darkred", "grattan_orange"),
  `diverging`  = get_qnd_hex("grattan_darkred", "grattan_lightgrey", "grattan_orange")
)


#' Interpolate a QND colour palette
#'
#' This function takes a QND colour palette and generates more colours from it,
#' so that there are enough to make your chart.
#'
#' The interpolation method is set to "spline" (the default is "linear") in an
#' attempt to reduce the number of vomit colours that get produced when
#' generating many colours.
#'
#' @param palette (character; default = \code{"graph"}) given name of a QND
#'   palette: \code{\link{qnd_palettes}}
#' @param reverse (boolean; default = \code{FALSE}) indicating if palette should
#'   be reverse
#' @param ... Additional arguments to pass to \code{colorRampPalette} see
#'   details here \code{\link[grDevices]{colorRamp}}
#'
#' @seealso \code{\link{qnd_palettes}}
#'
#' @export
make_qnd_pal <- function(palette = "graph", reverse = FALSE, ...) {
  pal <- qnd_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal,
                              ...,
                              interpolate = "spline")
}


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
               setting = grattan_palette()(1)) %>%
    purrr::map(prep_aes,
               aes = "fill",
               setting = grattan_palette()(1))

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
  ggplot2.discrete.colour   = list(unname(qnd_palettes$graph),
                                   make_qnd_pal()(16),
                                   make_qnd_pal()(24),
                                   make_qnd_pal()(32)),
  ggplot2.discrete.fill     = list(unname(qnd_palettes$graph),
                                   make_qnd_pal()(16),
                                   make_qnd_pal()(24),
                                   make_qnd_pal()(32))
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

#' Set default geom aesthetics
#'
#' All arguments are passed to \code{ggplot2::update_geom_defaults}, but this
#' function adds a \code{NULL} check.
#'
#' @param geom (character) the geom to update
#' @param new (various) the aesthetics to update
#'
set_geom_defaults <- function(geom,
                              new) {
  if (is.null(new)) {
    return(NULL)
  } else {
    ggplot2::update_geom_defaults(geom = geom,
                                  new = new)
  }

}
