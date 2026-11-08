# Suppress R CMD check NOTE about internal data objects
utils::globalVariables("vanilla_geom_aesthetics")

#' A list of plot options reflecting the Grattan brand
#'
plot_opts_grattan <- list(
  ggplot2.continuous.colour = scale_colour_grattan,
  ggplot2.continuous.fill = scale_fill_grattan,
  ggplot2.discrete.colour = function() scale_colour_grattan(discrete = TRUE),
  ggplot2.discrete.fill = function() scale_fill_grattan(discrete = TRUE)
)

#' The vanilla plotting options
#'
#' These options restore the vanilla ggplot2 colours options for the R session.
#'
plot_opts_vanilla <- list(
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.fill   = NULL,
  ggplot2.discrete.colour   = NULL,
  ggplot2.discrete.fill     = NULL
)



#' Set default geom aesthetics
#'
#' All arguments are passed to \code{ggplot2::update_geom_defaults}, but this
#' function adds a \code{NULL} check.
#'
#' @param geom Name of geom/stat to modify (like "point" or "bin"), or a
#' Geom/Stat object (like GeomPoint or StatBin).
#' @inheritParams ggplot2::update_geom_defaults
#'
.set_geom_aesthetics <- function(geom, new) {
  if (is.null(new)) {
    return(NULL)
  } else {
    ggplot2::update_geom_defaults(
      geom = geom,
      new = new
    )
  }
}

.safe_set_geom_aesthetics <- purrr::safely(.set_geom_aesthetics) # nolint

#' Set \code{ggplot2} default aesthetics
#'
#' Sets global options for \code{ggplot2}. If \code{type = "grattan"}, grattan brand
#' elements (colours and fonts) will be used by default in ggplot. To restore
#' the defaults use \code{type = "vanilla"}.
#'
#' @param type (character) Which aesthetics to use? One of "grattan" or "vanilla".
#'
#' @export
#' @examples
#' \dontrun{
#' set_aesthetics(type = "vanilla")
#' }
set_aesthetics <- function(type) {
  nice_type <- ifelse(type == "grattan", "grattan", type)

  rule <- paste(rep("\u2500", 40), collapse = "")

  the_message <- paste0(
    "\u2500\u2500 Aesthetics ", rule, "\n",
    "ggplot2 will use ", nice_type, " aesthetics (in the absence of a scale_colour etc function).\n",
    "Run `set_aesthetics()` again after any more `library()` calls to ggplot2 extension packages to set the aesthetics of geoms from those packages."
  )


  if (type == "grattan") {
    # Geom colours and sizes are set by grattanify_geom_defaults(), which
    # theme_grattan() calls
    options(plot_opts_grattan)
    packageStartupMessage(the_message)
  } else if (type == "vanilla") {
    # Set default aesthetics
    options(plot_opts_vanilla)
    purrr::iwalk(
      vanilla_geom_aesthetics,
      ~ .safe_set_geom_aesthetics(
        geom = .y,
        new = .x
      )
    )
    packageStartupMessage(the_message)
  } else {
    message("Not a valid type. Nothing changed.")
  }
}
