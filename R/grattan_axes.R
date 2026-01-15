#' Ensure your y- and x-axis are Grattan style guide-consistent and doesn't hang
#' off the edge of the chart.
#'
#' These functions also hide the '0' label when the axis starts at 0.
#'
#' @param expand_bottom default is 0. This will ensure that your x-axis is at
#'   the bottom value of your plotted data. Increase to add some buffer between
#'   the lowest point in your data and the x-axis. Note that the value is
#'   interpreted as a fraction of the total plotting space - a value of 1 will
#'   add white space equal to the whole area of your data.
#' @param expand_top default is 0.015. This will ensure that a small amount of
#'   white space is added to the top of your chart. Increase to add more white
#'   space.
#' @param expand_left default is 0. This will ensure your y-axis is at the
#'   lowest value of your plotted value.
#' @param expand_right default is 0.015. This will ensure that a small amount of
#'   white space is added to the right of your chart.
#' @param ... arguments passed to scale_y_continuous or scale_x_continuous. Note
#'   that if you provide a custom `labels` argument, it will override the default
#'   behavior of hiding the '0' label.
#' @examples
#'
#' # Here's a basic chart in the Grattan style:
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#' p
#'
#' # In the example above, the top label on the y-axis is hanging off the top.
#' # To fix:
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'      geom_point() +
#'      grattan_y_continuous() +
#'      theme_grattan()
#'
#' p
#'
#' # Now the top of the chart looks fine, but the bottom has two points
#' # (at the lower right) that are partially obscured.
#' # We could fix this by doing:
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'      geom_point() +
#'      grattan_y_continuous(expand_bottom = 0.015) +
#'      theme_grattan()
#'
#' p
#'
#' # Alternatively, set the limits of the chart (in this example we'll set
#' # the lower limit to 0, but you could use some other value
#' # like 10 in this case).Note that by setting the second value of limits
#' # to NA, we're telling ggplot2 to calculate the upper limit as usual based
#' # on the data. All the usual arguments of `scale_y_continuous()`
#' # (limits, breaks, labels, etc.) can be used.
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'      geom_point() +
#'      grattan_y_continuous(limits = c(0, NA)) +
#'      theme_grattan()
#'
#' p
#'
#' @name grattan_axes
#' @aliases NULL
NULL

#' Helper function to create axis labels without '0' when axis starts at 0
#' @param x numeric vector of break values
#' @noRd
grattan_label_hide_zero <- function(x) {
  labels <- scales::label_comma()(x)
  # Only hide '0' label if 0 is the minimum value (i.e., axis starts at 0)
  if (length(x) > 0 && min(x, na.rm = TRUE) == 0) {
    labels[x == 0] <- ""
  }
  labels
}

#' @rdname grattan_axes
#' @export

grattan_y_continuous <- function(expand_bottom = 0, expand_top = 0.015, ...) {
  # Get the list of additional arguments
  args <- list(...)

  # If labels argument is not provided, use the zero-hiding function
  if (!"labels" %in% names(args)) {
    args$labels <- grattan_label_hide_zero
  }

  # Add expand argument
  args$expand <- ggplot2::expansion(mult = c(expand_bottom, expand_top))

  # Call scale_y_continuous with all arguments
  do.call(ggplot2::scale_y_continuous, args)
}


#' @rdname grattan_axes
#' @export

scale_y_continuous_grattan <- function(expand_bottom = 0,
                                       expand_top = 0.015,
                                       ...) {

  grattan_y_continuous(expand_bottom = expand_bottom,
                       expand_top = expand_top,
                       ...)
}

#' @rdname grattan_axes
#' @importFrom ggplot2 scale_y_continuous expand_scale
#' @export

grattan_x_continuous <- function(expand_left = 0,
                                 expand_right = 0.015,
                                 ...) {

  # Get the list of additional arguments
  args <- list(...)

  # If labels argument is not provided, use the zero-hiding function
  if (!"labels" %in% names(args)) {
    args$labels <- grattan_label_hide_zero
  }

  # Add expand argument
  args$expand <- ggplot2::expansion(mult = c(expand_left, expand_right))

  # Call scale_x_continuous with all arguments
  do.call(ggplot2::scale_x_continuous, args)

}

#' @rdname grattan_axes
#' @export

scale_x_continuous_grattan <- function(expand_left = 0,
                                       expand_right = 0.015,
                                       ...) {

  grattan_x_continuous(expand_left = expand_left,
                       expand_right = expand_right,
                       ...)
}
