# Non-exported utility functions

# Given a number of points, convert to mm
#' @importFrom grid convertX

points_to_mm <- function(points) {
  as.numeric(grid::convertX(ggplot2::unit(points, "points"), "mm"))[1]
}

cm_to_in <- function(cm, round = FALSE) {
  inches <- cm / 2.54

  if (isTRUE(round)) {
    round(inches, 2)
  } else {
    inches
  }
}
