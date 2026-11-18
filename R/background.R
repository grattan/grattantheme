# Chart backgrounds when saving
#
# `background` in the save functions sets the background of each format:
#
#   "box" (or "orange")  report charts (normal, wholecolumn, fullpage) and
#                        their Powerpoint slides are light orange, to sit in a
#                        report box. Web-ready PNGs stay white: a web page
#                        can't put them in a box.
#   "grey_blog"          blog charts, and their Powerpoint and web versions,
#                        are light grey all over, like the header.
#
# Everything else is white. A chart made with theme_grattan(background =
# "box") counts as background = "box", so its orange is only used where a box
# is. Labels with a white (or background-coloured) fill take the background
# colour, so they don't show as white boxes.


# The light grey of the header on blog and fullslide charts, and of the whole
# image for a grey blog chart
grattan_header_grey <- "#F2F2F2"


# Check the `background` argument of a save function. Returns the backgrounds
# asked for, as a subset of c("box", "grey_blog"): "orange" becomes "box", and
# "white" (or nothing) is dropped.
check_background <- function(background) {
  if (is.null(background) || (is.character(background) &&
                              length(background) == 0)) {
    return(character(0))
  }

  if (!is.character(background)) {
    stop("`background` must be \"white\", \"box\", \"grey_blog\", ",
         "or c(\"box\", \"grey_blog\").", call. = FALSE)
  }

  if (any(background %in% c("grey", "gray"))) {
    stop("`background = \"grey\"` isn't an option. ",
         "For a grey blog chart, use `background = \"grey_blog\"`.",
         call. = FALSE)
  }

  bad <- setdiff(background, c("white", "box", "orange", "grey_blog"))
  if (length(bad) > 0) {
    stop("`background` can't be \"", paste(bad, collapse = "\", \""), "\". ",
         "Use \"white\", \"box\", \"grey_blog\", or c(\"box\", \"grey_blog\").",
         call. = FALSE)
  }

  background[background == "orange"] <- "box"
  background <- unique(background)

  if ("white" %in% background && length(background) > 1) {
    stop("`background = \"white\"` can't be combined with other backgrounds.",
         call. = FALSE)
  }

  setdiff(background, "white")
}


# The backgrounds that apply to a chart being saved: those asked for, plus
# "box" if the chart (or any part of a patchwork chart) was made with
# theme_grattan(background = "box").
resolve_background <- function(plot, background) {
  background <- check_background(background)

  plots <- if (inherits(plot, "patchwork")) {
    c(list(plot), plot$patches$plots)
  } else {
    list(plot)
  }
  box_theme <- vapply(plots, function(p) {
    fill <- p$theme$rect$fill
    is.character(fill) && length(fill) == 1 &&
      toupper(fill) == toupper(grattan_orange_alpha)
  }, logical(1))

  if (any(box_theme)) background <- union(background, "box")
  background
}


# The background colour of one saved format, from the backgrounds that apply
# to the chart. NULL when none apply, so the chart is saved exactly as it is.
background_colour <- function(type, background, web = FALSE) {
  if (length(background) == 0 || identical(type, "all")) return(NULL)

  class <- chart_types_all$class[match(type, chart_types_all$type)]

  if (identical(class, "blog")) {
    return(if ("grey_blog" %in% background) grattan_header_grey else "white")
  }

  if (identical(class, "normal") && !web && "box" %in% background) {
    return(grattan_orange_alpha)
  }

  "white"
}


# Give a chart a background colour. The theme sets the plot, panel, legend and
# strip backgrounds, which all inherit from `rect`. Label boxes (from
# grattan_label(), grattan_label_repel(), grattan_richlegend() and
# geom_label()) whose fill is white, the box orange or the header grey take
# the colour too; labels given any other fill keep it.
apply_background <- function(plot, colour) {
  plot <- add_theme_to_all(
    plot,
    ggplot2::theme(
      rect = ggplot2::element_rect(fill = colour),
      plot.background = ggplot2::element_rect(fill = colour, colour = NA)
    )
  )

  recolour_labels <- function(p) {
    if (inherits(p, "patchwork")) {
      p$patches$plots <- lapply(p$patches$plots, recolour_labels)
    }

    background_fills <- toupper(c("white", "#FFFFFF", "#FFF",
                                  grattan_orange_alpha, grattan_header_grey))
    is_background_fill <- function(fill) {
      is.character(fill) && length(fill) == 1 &&
        toupper(fill) %in% background_fills
    }

    for (i in seq_along(p$layers)) {
      layer <- p$layers[[i]]
      if (!class(layer$geom)[1] %in%
          c("GeomLabel", "GeomLabelRepel", "GeomGrattanRichLegend")) next

      in_aes <- is_background_fill(layer$aes_params$fill)
      in_geom <- is_background_fill(layer$geom_params$fill)
      if (!in_aes && !in_geom) next

      # Layers are environments shared with the caller's plot; copy before
      # changing, so other formats saved from the same plot aren't affected
      layer <- copy_layer(layer)
      if (in_aes) layer$aes_params$fill <- colour
      if (in_geom) layer$geom_params$fill <- colour
      p$layers[[i]] <- layer
    }

    p
  }

  recolour_labels(plot)
}


# Say when a background asked for doesn't apply to the chart types being
# saved, so the chart is white
message_unused_background <- function(background, types, web = FALSE) {
  if ("all" %in% types) types <- all_chart_types
  classes <- chart_types_all$class[match(types, chart_types_all$type)]
  classes <- classes[!is.na(classes)]

  if ("box" %in% background && (web || !any(classes == "normal"))) {
    message("The box background is only used for report charts (normal, ",
            "wholecolumn and fullpage) and their Powerpoint slides, so ",
            "this chart is white.")
  }

  if ("grey_blog" %in% background && !any(classes == "blog")) {
    message("The grey_blog background is only used for blog charts, so ",
            "this chart is white.")
  }

  invisible(NULL)
}
