#' Scale constructor for grattan colours
#'
#' If you get an error about not enough colours, try a different palette (such
#' as \code{graph}) or set \code{discrete = FALSE}. If you are plotting discrete colours
#' and need more than 10 colours, then you will have to make your own palette using
#' \code{\link{make_grattan_pal}}.
#'
#' @param palette Character name of palette in \code{grattan_palettes}
#' @param discrete Boolean indicating whether colour aesthetic is discrete or
#'   not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{\link{discrete_scale}} or
#'   \code{\link{scale_color_gradientn}}, used respectively when discrete is
#'   TRUE or FALSE
#'
#' @describeIn scale_colour_grattan For colour scales
#'
#' @export
scale_colour_grattan <- function(
    palette = NULL,
    discrete = FALSE,
    reverse = FALSE,
    ...
) {
  if (is.null(palette) && discrete) {
    palette <- "graph"
  }

  if (is.null(palette) && !discrete) {
    palette <- "sequential"
  }

  pal <- make_grattan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("grattan_", palette), palette = make_grattan_pal_discrete, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256L), ...)
  }
}


#' @describeIn scale_colour_grattan For fill scales
#'
#' @export
#'
scale_fill_grattan <- function(
    palette = NULL,
    discrete = FALSE,
    reverse = FALSE,
    ...
) {
  if (is.null(palette) && discrete) {
    palette <- "graph"
  }

  if (is.null(palette) && !discrete) {
    palette <- "sequential"
  }

  pal <- make_grattan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("grattan_", palette), palette =  make_grattan_pal_discrete, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256L), ...)
  }
}

### Deprecated ###

#' Convenient functions to set Grattan-appropriate palettes
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 7. Passed to \code{grattan_pal}; see ?grattan_pal for more
#'   information.
#'
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order.
#'
#' @param faded Deprecated - use \code{faded_level}. Logical. FALSE by default. Setting to TRUE returns the faded
#'   variations of the standard colours. If TRUE and \code{faded_level = 0}, sets \code{faded_level} to 5.
#'
#' @param faded_level Integer between 0 (no fade; the default) and 8 (most faded).
#'
#' @param discrete Logical. TRUE by default. Setting to FALSE generates a
#'   continuous colour scale.
#'
#' @param palette Sets the colours that will form the continuous palette when
#'   discrete = FALSE. One of:
#'
#' \itemize{
##' \item{"full"}{The default. Red, dark orange, light orange, yellow, light
##' yellow}
##' \item{"full_f"}{ faded version of "full"}
##' \item{"light"}{ light
##' orange, yellow, light yellow}
##' \item{"dark"}{ red, dark orange, light orange}
##' \item{"diverging"}{ red, faded red, white, faded light orange, light orange}
##' \item{"grey"}{ grey 1, grey 2, grey 3, grey 4, grey 5}
##'}
#'
#' @param ... arguments passed to ggplot2 scales
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'    geom_point() +
#'    grattan_colour_manual(n = 3) +
#'    theme_grattan()
#'
#' @name grattan_scale
#' @aliases NULL
NULL

#' @rdname grattan_scale
#' @import ggplot2
#' @export


grattan_colour_manual <- function(n = 0,
                                  reverse = FALSE,
                                  discrete = TRUE,
                                  faded_level = 0,
                                  faded = FALSE,
                                  palette = "full", ...) {

  lifecycle::deprecate_warn(when = "1.0.0", what = "grattan_colour_manual()",
                            details = "`grattan_colour_manual` has been deprecated which means it will no longer be maintained or updated. Please use `scale_colour_grattan` instead.")

  if (discrete) {
    return(
      ggplot2::scale_colour_manual(...,
                                   values = grattantheme::grattan_pal(n = n,
                                                                      reverse = reverse,
                                                                      faded_level = faded_level,
                                                                      faded = faded))
    )
  }

  if (!discrete) {
    pal <- make_grattan_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_color_gradientn(colours = pal(256), ...))
  }


}

#' @rdname grattan_scale
#' @import ggplot2
#' @export

grattan_fill_manual <- function(n = 0,
                                reverse = FALSE,
                                discrete = TRUE,
                                faded_level = 0,
                                faded = FALSE,
                                palette = "full", ...) {
  lifecycle::deprecate_warn(when = "1.0.0", what = "grattan_fill_manual()",
                            details = "`grattan_colour_manual` has been deprecated which means it will no longer be maintained or updated. Please use `scale_fill_grattan` instead.")

  if (discrete) {
    return(
      ggplot2::scale_fill_manual(...,
                                 values = grattan_pal(n = n,
                                                      reverse = reverse,
                                                      faded_level = faded_level,
                                                      faded = faded))
    )
  }

  if (!discrete) {
    pal <- make_grattan_pal(palette = palette, reverse = reverse)
    return(ggplot2::scale_fill_gradientn(colours = pal(256), ...))
  }

}
