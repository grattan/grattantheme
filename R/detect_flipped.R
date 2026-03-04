# Auto-detection of flipped/horizontal charts for theme_grattan()
#
# When theme_grattan() is added to a plot via `+`, the ggplot_add method
# below inspects the plot to determine if it represents a horizontal/flipped
# chart. If so, and the user hasn't explicitly set `flipped`, the theme is
# rebuilt with flipped = TRUE and a throttled message is shown.


#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.grattan_theme <- function(object, plot, ...) {
  args <- attr(object, "grattan_args")

  # Only run detection if flipped was left as NULL (the default)
  # and chart_type is not "scatter" (scatter ignores flipped)
  if (!is.null(args) &&
      is.null(args$flipped) &&
      args$chart_type != "scatter") {

    if (.detect_flipped_chart(plot)) {
      # Rebuild theme with flipped = TRUE
      object <- theme_grattan_normal(
        base_size = args$base_size,
        base_family = args$base_family,
        background = args$background,
        legend = args$legend,
        panel_borders = args$panel_borders,
        flipped = TRUE
      )

      .notify_auto_flip()
    }
  }

  # Strip custom class and attributes, delegate to standard theme adding
  attr(object, "grattan_args") <- NULL
  class(object) <- setdiff(class(object), "grattan_theme")

  ggplot2::ggplot_add(object, plot, ...)
}


# Detect if a plot represents a flipped/horizontal chart.
# Returns TRUE only when confident; FALSE on any error or ambiguity.
.detect_flipped_chart <- function(plot) {
  tryCatch({
    # 1. Check for coord_flip()
    if (inherits(plot$coordinates, "CoordFlip")) {
      return(TRUE)
    }

    # 2. Check plot-level mappings: discrete y + continuous x
    if (.check_mappings_flipped(plot$mapping, plot$data)) {
      return(TRUE)
    }

    # 3. Fallback: check first layer's mappings if plot-level is empty
    if (length(plot$mapping) == 0 && length(plot$layers) > 0) {
      layer <- plot$layers[[1]]
      layer_data <- if (is.data.frame(layer$data)) layer$data else plot$data
      if (is.data.frame(layer_data) && length(layer$mapping) > 0) {
        if (.check_mappings_flipped(layer$mapping, layer_data)) {
          return(TRUE)
        }
      }
    }

    FALSE
  }, error = function(e) {
    FALSE
  })
}


# Check if a mapping + data combination indicates a flipped chart.
# A chart is "flipped" when y maps to a discrete variable and x maps to
# a continuous variable (or is absent/NULL).
.check_mappings_flipped <- function(mapping, data) {
  if (is.null(mapping) || length(mapping) == 0) return(FALSE)
  if (!is.data.frame(data) || nrow(data) == 0) return(FALSE)

  tryCatch({
    y_val <- if (!is.null(mapping$y)) {
      rlang::eval_tidy(mapping$y, data = data)
    }

    x_val <- if (!is.null(mapping$x)) {
      rlang::eval_tidy(mapping$x, data = data)
    }

    y_is_discrete <- !is.null(y_val) &&
      (is.factor(y_val) || is.character(y_val) || is.logical(y_val))

    x_is_continuous_or_null <- is.null(x_val) || is.numeric(x_val)

    y_is_discrete && x_is_continuous_or_null
  }, error = function(e) {
    FALSE
  })
}


# Show a message when auto-flip is applied, throttled to once per 8 hours.
# Uses an environment variable to track the last message time, following
# the same pattern as overleaf_utils.R.
.notify_auto_flip <- function() {
  last_str <- Sys.getenv("GRATTANTHEME_FLIP_LAST_MESSAGE", unset = "")

  should_message <- TRUE

  if (last_str != "") {
    tryCatch({
      last_time <- as.POSIXct(last_str)
      if (difftime(Sys.time(), last_time, units = "hours") < 8) {
        should_message <- FALSE
      }
    }, error = function(e) {
      # If timestamp can't be parsed, show the message
    })
  }

  if (should_message) {
    message(
      "grattantheme: Auto-detected a horizontal/flipped chart and applied ",
      "`flipped = TRUE`. Set `theme_grattan(flipped = TRUE)` explicitly to ",
      "silence this message."
    )
    Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = as.character(Sys.time()))
  }
}
