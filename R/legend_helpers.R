#' Create the css needed to colour text using ggtext
#'
#' This is a helper function to wrap the css to make text certain colours. This
#' will be useful when making grattan charts. This should be used within `ggtext::geom_richtext()`
#' or if being used in a sub-title or title it should be used in the `labs()` call together with
#' `theme(plot.subtitle = element_markdown())`
#'
#'
#' @param colour (a character) the colour you want to colour text
#' @param text (a character) what the coloured text should say
#' @param is_note (logical; default = FALSE) is it a note?
#' @param bold_labs (logical; default = TRUE) should the labels be bolded?
#'
#' @return (a glue)
#' @export
#'
#' @examples
#' \dontrun{library(ggtext)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'     labs(
#'         title = glue::glue("**Fisher's *Iris* dataset**
#'             <span style='font-size:11pt'>Sepal width vs. sepal length for
#'                                    {colour_text('#0072B2','setosa')}
#'                                    {colour_text('#D55E00','versicolor')}
#'                                    {colour_text('#009E73','virginica')}
#'                                        </span>"),
#'                                          theme(plot.title = element_markdown(lineheight = 1.1) )
#' }
colour_text <- function(colour, text, is_note = FALSE, bold_labs = TRUE) {
  assertthat::assert_that(is.character(colour), msg = "`colour` should be a character")
  assertthat::assert_that(is.character(text), msg = "`text` should be a character")
  if (!is_note) {
    if (bold_labs) {
      ret <- glue::glue("<b style='color:{colour}'>{text}</b>")
    } else {
      ret <- glue::glue("<span style='color:{colour}'>{text}</span>")
    }
  }
  if (is_note) ret <- glue::glue("<span style='color:{grattan_grey3}'>{text}</span>")

  return(ret)
}

#' Add an on-chart "rich" legend with a white background
#'
#' `grattan_richlegend()` places a coloured, on-chart legend at one of the
#' panel corners (or a custom position).  A white background box sits behind the text so the labels
#' remain readable when they overlap gridlines, in the same no-border style as
#' [grattan_label()]. Inspired by `ggdirectlabel::geom_richlegend()`.
#'
#' @param mapping Aesthetic mapping created by [ggplot2::aes()]. Must include
#'   `label` and `colour`.
#' @param data Data for the layer.
#' @param legend.position Where to place the legend. Either one of
#'   `"topright"`, `"topleft"`, `"bottomright"`, `"bottomleft"`, `"top"`,
#'   `"bottom"`, `"left"`, `"right"`, or a numeric vector of length 2 with
#'   values between 0 and 1 giving the relative `(x, y)` position within the
#'   panel. Defaults to `"topright"`.
#' @param size Font size for legend text. Default is `18`.
#' @param padding Amount of white padding around the legend, measured in
#'   "lines". Default is `0.2`.
#' @param lineheight Line height between legend entries. Default is `0.8`.
#' @param fill Background fill colour. Default is `"white"`.
#' @param na.rm If `FALSE` (default) missing values are removed with a warning.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather
#'   than combining with them.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' df <- data.frame(
#'   year = rep(2000:2010, 3),
#'   value = c(seq(1, 11), seq(2, 12), seq(3, 13)),
#'   series = rep(c("A", "B", "C"), each = 11)
#' )
#'
#' ggplot(df, aes(year, value, colour = series)) +
#'   geom_line() +
#'   grattan_richlegend(aes(label = series)) +
#'   theme_grattan()
#' }
#'
#' @export
grattan_richlegend <- function(mapping = NULL,
                               data = NULL,
                               legend.position = "topright",
                               size = 18,
                               padding = 0.2,
                               lineheight = 0.8,
                               fill = "white",
                               na.rm = FALSE,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomGrattanRichLegend,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      legend.position = legend.position,
      size = size / ggplot2::.pt,
      padding = padding,
      lineheight = lineheight,
      fill = fill,
      ...
    )
  )
}

# Translate a legend.position keyword (or numeric pair) into native panel
# coordinates. Adapted from ggdirectlabel:::legend_pos_to_xy.
grattan_legend_pos_to_xy <- function(legend.position, xrange, yrange, flipped) {
  l <- legend.position
  if (is.numeric(l)) {
    stopifnot(
      "Numeric `legend.position` must have length 2" = length(l) == 2,
      "Numeric `legend.position` must have values between 0 and 1" =
        min(l) >= 0 && max(l) <= 1
    )
    l_num <- l
  } else {
    valid <- c("left", "right", "bottom", "top",
               "topright", "bottomright", "bottomleft", "topleft")
    if (!l %in% valid) {
      stop("`legend.position` must be one of ",
           paste(shQuote(valid), collapse = ", "),
           " or a numeric vector of length 2.", call. = FALSE)
    }
    l_num <- switch(l,
      left = c(0.025, 0.5),
      right = c(0.975, 0.5),
      bottom = c(0.5, 0.025),
      top = c(0.5, 0.975),
      topright = c(0.975, 0.975),
      bottomright = c(0.975, 0.025),
      bottomleft = c(0.025, 0.025),
      topleft = c(0.025, 0.975)
    )
  }

  l_x <- l_num[1] * (xrange[2] - xrange[1]) + xrange[1]
  l_y <- l_num[2] * (yrange[2] - yrange[1]) + yrange[1]

  if (isTRUE(flipped)) c(l_y, l_x) else c(l_x, l_y)
}

# Pick sensible hjust/vjust for the chosen legend.position keyword so the
# legend's anchor sits flush against the corresponding panel edge. Numeric
# positions don't auto-adjust - user controls hjust/vjust directly.
grattan_legend_just <- function(legend.position) {
  if (!is.character(legend.position)) return(NULL)
  switch(legend.position,
    topright    = c(1,   1),
    topleft     = c(0,   1),
    bottomright = c(1,   0),
    bottomleft  = c(0,   0),
    top         = c(0.5, 1),
    bottom      = c(0.5, 0),
    left        = c(0,   0.5),
    right       = c(1,   0.5)
  )
}

#' @noRd
GeomGrattanRichLegend <- ggplot2::ggproto(
  "GeomGrattanRichLegend", ggplot2::Geom,
  required_aes = c("label", "colour"),
  default_aes = ggplot2::aes(
    size = 18 / ggplot2::.pt,
    angle = 0,
    hjust = 1,
    vjust = 1,
    alpha = NA,
    family = "",
    fontface = 1,
    lineheight = 0.8
  ),
  extra_params = c("na.rm", "legend.position", "padding", "fill"),
  draw_panel = function(data, panel_params, coord,
                        legend.position = "topright",
                        padding = 0.2,
                        fill = "white") {
    flipped <- inherits(coord, "CoordFlip")
    xy <- grattan_legend_pos_to_xy(
      legend.position, panel_params$x.range, panel_params$y.range, flipped
    )

    just <- grattan_legend_just(legend.position)
    if (!is.null(just)) {
      data$hjust <- just[1]
      data$vjust <- just[2]
    }

    # Wrap each row's label in an HTML span carrying its own colour, then
    # collapse all rows within a panel into a single rich-text label.
    data$label <- paste0("<span style='color:", data$colour,
                         "'>", data$label, "</span>")
    data$x <- xy[1]
    data$y <- xy[2]

    data <- do.call(rbind, lapply(split(data, data$PANEL), function(d) {
      out <- d[1, , drop = FALSE]
      out$label <- paste(d$label, collapse = "<br>")
      out
    }))
    data$colour <- "#000000"

    data <- coord$transform(data, panel_params)

    gridtext::richtext_grob(
      text = data$label,
      x = data$x,
      y = data$y,
      default.units = "native",
      hjust = data$hjust,
      vjust = data$vjust,
      rot = data$angle,
      padding = grid::unit(rep(padding, 4), "lines"),
      margin = grid::unit(c(0, 0, 0, 0), "lines"),
      gp = grid::gpar(
        col = scales::alpha(data$colour, data$alpha),
        fontsize = data$size * ggplot2::.pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      box_gp = grid::gpar(fill = fill, col = NA),
      r = grid::unit(0, "lines")
    )
  },
  draw_key = ggplot2::draw_key_blank
)
