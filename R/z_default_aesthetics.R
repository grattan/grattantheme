#' A vector of geoms
#'
#' A vector of geoms exported from `{ggplot2}` and extension packages. This
#' vector is iterated on to set the default aesthetics for each geom, allowing
#' the grattan colours to be plotted automatically.
#'
#' @format A vector
"all_geoms"


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

#' Prepare `{ggplot2}` geom_ defaults
#'
#' ggplot2 geoms have default aesthetics which can be changed for each
#' session. This function is a generalised way to set geom defaults. It only
#' changes settings where the default is not NA; no need for \code{geom_point}
#' to have a default font family.
#'
#' This is used inside `.set_grattab_aesthetics()`
#'
#' @param geom (character) the geom to change (e.g. \code{point} or
#'   \code{line})
#' @param aes (character) the aesthetic to change (e.g. \code{colour} or
#'   \code{alpha})
#' @param setting (various) what to set the aesthetic (e.g. a colour or a
#'   number)
#'
#' @return the default aesthetic ready to be set
.prep_aes <- function(geom, aes, setting) {
  # Check if the geom inherits from elsewhere
  if (is.null(geom)) {
    return(NULL)
  }

  # Check if the setting exists for this geom
  if (is.null(geom[[aes]])) {
    return(geom)
  }

  # Change the setting only if it's not NA
  if (!is.na(geom[[aes]])) {
    geom[[aes]] <- setting
  }

  return(geom)
}


#' Set the default ggplot2 aesthetics to grattan branding
#'
#' A wrapper for a bunch of other functions to set ggplot2 default aesthetics.
.set_grattan_aesthetics <- function() {
  # Import an unexported function from {ggplot2} used to inspect a ggproto
  # object, the object class that underpins all geoms
  check_subclass <- utils::getFromNamespace("check_subclass", "ggplot2")

  current_aesthetics <- grattantheme::all_geoms %>%
    # Get the details for geoms currently available in the namespace
    # We use safely() because the geom's package must be libraried for it to
    # be accessible (otherwise it errors), and `all_geoms` was build with
    # many ggplot extension packages libraried
    purrr::map(
      purrr::safely(check_subclass),
      "Geom",
      env = parent.frame()
    ) %>%
    purrr::map(
      purrr::pluck,
      "result"
    ) %>%
    purrr::map(
      purrr::pluck,
      "default_aes"
    ) %>%
    purrr::set_names(grattantheme::all_geoms)

  # Overwrite elements of the current_aesthetics to use theme settings
  # Just font family to inter (if available) and col/fill to QND blue stone
  grattan_aesthetics <- current_aesthetics %>%
    # purrr::map(
    #   # We use .prep_aes() (defined above) to handle NULLS and NAs
    #   .prep_aes,
    #   aes = "family",
    #   setting = qnd_font()
    # ) %>%
    purrr::map(
      .prep_aes,
      aes = "colour",
      setting <- grattan_orange
    ) %>%
    purrr::map(
      .prep_aes,
      aes = "fill",
      setting <- grattan_orange
    )

  # nolint start
  purrr::iwalk(
    grattan_aesthetics,
    ~ .safe_set_geom_aesthetics(
      geom = .y,
      new = .x
    )
  )
  # nolint end

  # Explicit setting of geom_sf defaults
  ggplot2::update_geom_defaults(
    geom = "sf",
    new = list(
      col = grattan_orange,
      fill = grattan_orange
    )
  )
}

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

  the_message <- paste(
    "ggplot2 will use",
    nice_type,
    "aesthetics (in the absence of a scale_colour etc function).",
    "\nRun `set_aesthetics()` again after any more `library()` calls to {ggplot2} extension packages to set the aesthetics of geoms from those packages."
  )


  if (type == "grattan") {
    options(plot_opts_grattan)
    .set_grattan_aesthetics()
    message(the_message)
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
    message(the_message)
  } else {
    message("Not a valid type. Nothing changed.")
  }
}
