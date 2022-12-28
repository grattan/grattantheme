#' Predefined grattan colours combined into palettes
#'
#' This is a list of grattan colours combined into palettes. The palettes are used
#' for different plots and maps.
#' @export
grattan_palettes <- list(
  `graph` = c(grattan_orange,
              grattan_red,
              grattan_yellow,
              grattan_darkorange,
              grattan_darkred,
              grattan_lightyellow,
              grattan_blue,
              grattan_darkblue,
              grattan_lightgrey,
              grattan_darkgrey),
  `sequential` = c(grattan_orange, grattan_darkred),
  `diverging` = c(grattan_darkred, grattan_orange, grattan_yellow)
)

#' Interpolate a grattan colour palette
#'
#' This function takes a grattan colour palette and generates more colours from it,
#' so that there are enough to make your chart.
#'
#' The interpolation method is set to "spline" (the default is "linear") in an
#' attempt to reduce the number of vomit colours that get produced when
#' generating many colours.
#'
#' It returns a function that takes a single value and makes that many colours.
#'
#' @param palette (character; default = \code{"graph"}) given name of a grattan
#'   palette: \code{\link{grattan_palettes}}
#' @param reverse (boolean; default = \code{FALSE}) indicating if palette should
#'   be reverse
#' @param ... Additional arguments to pass to \code{colorRampPalette} see
#'   details here \code{\link[grDevices]{colorRamp}}
#'
#' @seealso \code{\link{grattan_palettes}}
#'
#' @examples
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, colour = as.character(wt))) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_colour_manual(values = make_grattan_pal()(29))
#'
#' @export
make_grattan_pal <- function(palette = "graph", reverse = FALSE, ...) {
  pal <- grattan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(
    pal,
    ...,
    interpolate = "spline"
  )
}

#' Create a grattan colour palette
#'
#' This function takes a the grattan graph colour palette and returns a vector of colours equal to n.
#' It is used in \code{\link{scale_colour_grattan}} and \code{\link{scale_fill_grattan}} to make the discrete
#' colour scale as the order of colours is specific in the grattan branding guides and so using an interpolated scale
#' does not work.
#'
#'
#' @param n how many colours to return
#'
#' @seealso \code{\link{grattan_palettes}}
#'
#' @export
make_grattan_pal_discrete <- function(n) {
  pal <- grattan_palettes[["graph"]]

  return(pal[1:n])
}

#' Register the option for which palette to use.
#'
#' Grattan now has an old palette and a new palette. Depending which is needed users can
#' set options("grattan_palette" = "old") to have the colours map to the old colours. Otherwise the colours
#' default to the new colours.
#'
#' @export
register_palette <- function() {
  palette_option <- options("grattan_palette")
  if (is.null(palette_option$grattan_palette)) {
    message("No palette option declared for grattantheme, setting it to new")
    options("grattan_palette" = "new")
  }
}

### Deprecated ####
# Generates a full palette
# deprecated in version 1.0.0
grattan_palette <- function(palette = "full", reverse = FALSE, ...) {

  pal <- grattantheme::grattan_palette_set[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Create a Grattan-appropriate palette for your chart.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 is not recommended. If you don't
#'   specify `n`, a five-colour palette will be used, which may not look right.
#'   Specify `n`.
#'
#'   By default, n = 2 will give you light orange and dark orange. Use n = "2a"
#'   if you want light orange and red.
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order. Standard colour order runs from light to dark. If
#'   you set reverse to TRUE, colours will run from dark to light.
#'
#' @param faded Deprecated - use \code{faded_level}. Logical. FALSE by default. Setting to TRUE returns the faded
#'   variations of the standard colours. If TRUE and \code{faded_level = 0}, sets \code{faded_level} to 5.
#'
#' @param faded_level Integer between 0 (no fade; the default) and 8 (most faded).
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     scale_colour_manual(values = grattan_pal(n = 3))
#'
#' p
#'
#' # Alternatively, use grattan_colour_manual(), which is a wrapper
#' # around scale_colour_manual():
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     grattan_colour_manual(n = 3)
#'
#' p
#'
#' @export

grattan_pal <- function(n = 0,
                        reverse = FALSE,
                        faded_level = 0,
                        faded = FALSE) {

  lifecycle::deprecate_warn(when = "1.0.0", what = "grattan_pal()",
                            details = "Please use `make_grattan_pal` or `make_grattan_pal_discrete` instead.")

  if (isTRUE(faded) & faded_level == 0) {
    faded_level <- 4
    warning("faded argument is deprecated. Please use faded_level instead.",
            "Setting faded_level to 4.")
  }

  if (!faded_level %in% c(0:8)) stop("faded_level must be an integer between 0 and 8.")

  if (n == 0) {
    n <- 6
    "Your chart will probably look better if you specify n in grattan_pal()."
  }

  if (n > 10 & n != "2a") {
    stop(paste0("You've requested ", n,
                " colours; grattan_pal() only supports up to 10."))
  }

  palette <- get_palette(n, faded_level)

  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }

  palette
}

get_palette <- function(n, f) {

  if (n == 1) {
    palette <- "orange"
  } else if (n == "2a") {
    palette <- c("orange",
                         "darkorange")
  } else if (n == 2) {
    palette <- c("orange",
                         "red")
  } else if (n == 3) {
    palette <- c("yellow",
                         "orange",
                         "red")
  } else if (n == 4) {
    palette <- c("yellow",
                         "orange",
                         "darkorange",
                         "red")
  } else if (n == 5) {
    palette <- c("yellow",
                         "orange",
                         "darkorange",
                         "red",
                         "darkred")
  } else if (n == 6) {
    palette <- c("lightyellow",
                              "yellow",
                              "orange",
                              "darkorange",
                              "red",
                              "darkred")
  } else if (n == 7) {
    palette <- c("lightyellow",
                              "yellow",
                              "lightorange",
                              "darkorange",
                              "red",
                              "darkred",
                              "blue")
  } else if (n == 8) {
    palette <- c("lightyellow",
                              "yellow",
                              "lightorange",
                              "darkorange",
                              "red",
                              "darkred",
                              "blue",
                              "darkblue")
  } else if (n == 9) {
    palette <- c("lightyellow",
                              "yellow",
                              "lightorange",
                              "darkorange",
                              "red",
                              "darkred",
                              "blue",
                              "darkblue",
                              "lightgrey")
  } else if (n == 10) {
    palette <- c("lightyellow",
                              "yellow",
                              "lightorange",
                              "darkorange",
                              "red",
                              "darkred",
                              "blue",
                              "darkblue",
                              "lightgrey",
                              "darkgrey")
  }

  if (f == 0) f <- ""

  palette <- purrr::map_chr(
    paste0("grattan_", palette, f),
    get
  )

  return(palette)

}


