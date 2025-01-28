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

deendash <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ str_replace_all(., "–", "-")))
}

# Case hen that orders the factors as you order the case when
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]]) # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)}


# Wrap strings without dropping factor levels
str_wrap_factor <- function(x, ...) {
  levels(x) <- stringr::str_wrap(levels(x), ...)
  x
}
