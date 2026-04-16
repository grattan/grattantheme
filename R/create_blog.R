#' Assemble a square blog-style chart with a grey title header and Grattan logo
#'
#' Takes a ggplot2 object and formats it as a 23.16cm x 23.16cm square image
#' suitable for social media. The layout consists of a grey title bar
#' containing the title and logo, a subtitle area, a chart panel, and a caption
#' area.
#'
#' You will rarely need to call this function directly - use
#' `grattan_save()` with `type = "blog"` (or `grattan_save_web()`) to save a
#' ggplot2 object as a blog image.
#'
#' @param plot A ggplot2 plot.
#' @param font Either "slide" (default) or "normal". "slide" uses DM Serif
#'   Display for the title and Avenir Next for body text (if available).
#'   "normal" uses the default sans-serif font.
#' @param ignore_long_title Default is FALSE. If TRUE, the check on a long
#'   title won't be performed. This is useful if using ggtext syntax within
#'   titles.
#'
#' @return An object of class "patchwork" sized for a 23.16cm x 23.16cm blog
#'   image.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "My title",
#'          subtitle = "My subtitle",
#'          caption = "My caption") +
#'     theme_grattan()
#'
#' p_blog <- create_blog(p, font = "normal")
#'
#' @export
#' @importFrom patchwork wrap_plots wrap_elements plot_annotation
#' @import grid
create_blog <- function(plot = ggplot2::last_plot(),
                        font = c("slide", "normal"),
                        ignore_long_title = FALSE) {

  if (!inherits(plot, "ggplot")) {
    stop(deparse(substitute(plot)), " is not a ggplot2 object.")
  }

  font <- match.arg(font)
  title_font <- get_grattan_font(font, "title")
  main_font <- get_grattan_font(font, "body")

  # Wrap labels and extract
  p <- wrap_labs(plot, "blog", ignore_long_title = ignore_long_title)

  labs <- extract_labs(p)
  p <- replace_labs(p,
                    labs = list(title = NULL,
                                subtitle = NULL,
                                caption = NULL))

  # Apply body font to theme text and text geom layers
  p <- p + ggplot2::theme(text = ggplot2::element_text(family = main_font))
  p <- apply_font_to_geom_text(p, main_font)

  stored_title <- labs$title
  stored_subtitle <- labs$subtitle
  stored_caption <- labs$caption

  title_font_size <- 32
  subtitle_font_size <- 18
  caption_font_size <- 8

  # Detect multiline title (already wrapped by wrap_labs)
  title_is_multiline <- !is.null(stored_title) && grepl("\n", stored_title)

  # Dimensions (cm)
  total_width <- 23.16
  total_height <- 23.16
  side_margin <- 0.5
  inner_width <- total_width - 2 * side_margin   # 22.16

  title_bar_height <- 3.61
  subtitle_area_height <- 2.53
  chart_panel_height <- 14.50
  caption_area_height <- 2.52

  logo_width <- 4

  # Grey header: title on left, logo on right
  grey_box <- grid::rectGrob(gp = grid::gpar(fill = "#F2F2F2",
                                             col = "#F2F2F2"))

  title_y <- if (title_is_multiline) 0.42 else 0.55

  toptitle <- grid::textGrob(
    label = if (is.null(stored_title)) "" else stored_title,
    x = unit(0.5, "cm"),
    y = unit(title_y, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = "black",
                    fontsize = title_font_size,
                    lineheight = 0.9,
                    fontfamily = title_font)
  )

  logo_vp <- grid::viewport(
    x = unit(1, "npc") - unit(0.5, "cm"),
    y = unit(0.5, "npc"),
    width = unit(logo_width, "cm"),
    just = c("right", "center"),
    clip = "off"
  )

  header_grob <- grid::gTree(children = grid::gList(
    grey_box,
    toptitle,
    grid::editGrob(logogrob, vp = logo_vp)
  ))

  # Subtitle (vertically centred)
  subtitle_present <- !is.null(stored_subtitle) && stored_subtitle != ""

  subtitle_grob <- grid::textGrob(
    label = if (subtitle_present) stored_subtitle else "",
    x = unit(0, "npc"),
    y = unit(0.5, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = "black",
                    fontsize = subtitle_font_size,
                    lineheight = 0.9,
                    fontfamily = main_font)
  )

  # Caption, positioned ~0.44 cm below the chart panel
  caption_present <- !is.null(stored_caption) && stored_caption != ""

  caption_grob <- grid::textGrob(
    label = if (caption_present) stored_caption else "",
    x = unit(0, "npc"),
    y = unit(1, "npc") - unit(0.44, "cm"),
    just = c("left", "top"),
    gp = grid::gpar(col = "black",
                    fontsize = caption_font_size,
                    lineheight = 0.9,
                    fontfamily = main_font)
  )

  # Assemble layout
  if (subtitle_present) {
    layout <- "
    HHH
    SSS
    PPP
    CCC
    "
    heights <- grid::unit(c(title_bar_height,
                            subtitle_area_height,
                            chart_panel_height,
                            caption_area_height),
                          "cm")

    assembled <- patchwork::wrap_plots(
      H = patchwork::wrap_elements(full = header_grob),
      S = patchwork::wrap_elements(full = subtitle_grob),
      P = patchwork::wrap_elements(full = p),
      C = patchwork::wrap_elements(full = caption_grob),
      design = layout,
      heights = heights
    )
  } else {
    # Expand chart panel into subtitle space when subtitle is absent
    layout <- "
    HHH
    PPP
    CCC
    "
    heights <- grid::unit(c(title_bar_height,
                            subtitle_area_height + chart_panel_height,
                            caption_area_height),
                          "cm")

    assembled <- patchwork::wrap_plots(
      H = patchwork::wrap_elements(full = header_grob),
      P = patchwork::wrap_elements(full = p),
      C = patchwork::wrap_elements(full = caption_grob),
      design = layout,
      heights = heights
    )
  }

  assembled +
    patchwork::plot_annotation(
      theme = ggplot2::theme(plot.margin = ggplot2::margin(0,
                                                           side_margin,
                                                           0,
                                                           side_margin,
                                                           "cm"))
    )
}
