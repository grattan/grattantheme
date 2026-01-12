#' Assemble a chart featuring the Grattan logo and orange line
#'
#' Takes a ggplot2 object and formats it to look like a
#' Grattan Powerpoint slide. You will rarely need to call this function
#' directly - use `grattan_save()` to save a ggplot2 object as a 'slide'-like
#' image.
#'
#' @param plot A ggplot2 plot
#' @param type Optional. If specified, must be one of "fullslide", "fullslide_169", "fullslide_44", or "blog".
#' This is used to define the size of the white border around the image.
#'
#' @return An object of class "patchwork".
#'
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "My title",
#'          subtitle = "My subtitle",
#'          caption = "My caption") +
#'     theme_grattan()
#'
#' # Create an image that includes the Grattan logo
#'
#' p_logo <- create_fullslide(p)
#'
#'
#' @export
#' @importFrom patchwork wrap_plots wrap_elements plot_spacer plot_annotation
#' @import grid

create_fullslide <- function(plot = last_plot(),
                             type) {

    # Check inputs and define plot borders ----

    if (!inherits(plot, "ggplot")) {
      stop(deparse(substitute(plot)), " is not a ggplot2 object.")
    }

    if (missing(type)) {
      top_border <- 0.15
      right_border <- 0.15
      bottom_border <- 0.05
      left_border = 0.15

    } else {
      if (!type %in% fullslide_chart_types_inc_deprecated) {
        stop(type,
             " is not a valid chart type.\nMust be one of: ",
             paste(fullslide_chart_types, collapse = ", "))
      }

      chosen_chart_type <- chart_types[chart_types$type == type, ]
      top_border <- chosen_chart_type$top_border
      right_border <- chosen_chart_type$right_border
      bottom_border <- chosen_chart_type$bottom_border
      left_border <- chosen_chart_type$left_border
    }

    # Create title and subtitle -----
    p <- plot

    labs <- extract_labs(p)

    p <- replace_labs(p,
                      labs = list(title = NULL,
                                  subtitle = NULL,
                                  caption = NULL))

    # Determine fonts
    available_fonts <- systemfonts::system_fonts()$family

    title_font <- if("DM Serif Display" %in% available_fonts) {
      "DM Serif Display"
    } else {
      "serif"
    }

    main_font <- if("Avenir Next" %in% available_fonts) {
      "Avenir Next"
    } else {
      "sans"
    }

    stored_title <- labs$title
    stored_subtitle <- labs$subtitle
    stored_caption <- labs$caption

    title_font_size <- 32
    subtitle_font_size <- 18
    caption_font_size <- 8

    # Wrap title text to fit within available space (max ~52 chars per line)
    # This prevents text from clipping behind the logo
    if (!is.null(stored_title) && nchar(stored_title) > 52) {
      stored_title <- paste(strwrap(stored_title, width = 52), collapse = "\n")
    }

    # Create grey box as background
    # The grey box needs to extend beyond the plot area into the margins
    # to fill the full slide width. We extend it by the left/right borders.
    # For asymmetric borders (like fullslide_half), we need to calculate the
    # x position offset to account for the different left/right extensions.

    # Calculate the x position: shift left by (right_border - left_border) / 2
    # This centers the extended box on the full slide, not the plot area
    x_offset <- (right_border - left_border) / 2

    grey_box_vp <- grid::viewport(x = unit(0.5, "npc") + unit(x_offset, "cm"),
                                   y = 0.5,
                                   width = unit(1, "npc") + unit(left_border + right_border, "cm"),
                                   height = unit(1, "npc"),
                                   just = c("centre", "centre"),
                                   clip = "off")

    grey_box <- grid::editGrob(
      grid::rectGrob(gp = gpar(fill = "#F2F2F2", col = "#F2F2F2")),
      vp = grey_box_vp
    )

    # Create the title text grob
    # Using vjust = 0.5 for proper vertical centering of multi-line text
    toptitle <- grid::textGrob(label = stored_title,
                               x = unit(0, "npc"),
                               y = unit(0.5, "npc"),
                               just = c("left", "centre"),
                               vjust = 0.5,
                               gp = gpar(col = "black",
                                         fontsize = title_font_size,
                                         lineheight = 0.9,
                                         fontfamily = title_font))

    # Create header grob that combines grey box + title + logo
    # Title and logo should span a consistent width (standard fullslide width)
    # regardless of the chart type below.
    # The header panel width varies by chart type, so for narrower charts we need
    # to extend the title/logo beyond the panel boundaries.
    standard_fullslide_width <- chart_types$width[chart_types$type == "fullslide"]
    logo_width <- 4  # Width of logo in cm

    # Calculate the x offset for title/logo based on chart alignment
    # For centered charts (fullslide_narrow), we need to shift right by left_border
    # For left-aligned charts (fullslide, fullslide_half), no shift needed
    title_logo_x_offset <- if (left_border == right_border) {
      # Symmetric borders = centered chart, so offset title/logo to center
      left_border
    } else {
      # Asymmetric borders = left-aligned chart, no offset
      0
    }

    # Title viewport: spans from offset position to standard_fullslide_width - logo_width
    # For narrower charts, this extends beyond the panel (clip = off)
    title_vp <- grid::viewport(
      x = unit(title_logo_x_offset, "cm"),
      y = 0.5,
      width = unit(standard_fullslide_width - logo_width - 0.1, "cm"),
      height = unit(1, "npc"),
      just = c("left", "center"),
      clip = "off"
    )

    # Logo viewport: positioned at standard_fullslide_width from offset, logo_width cm wide
    # This also extends beyond panel for narrower charts
    logo_vp <- grid::viewport(
      x = unit(title_logo_x_offset + standard_fullslide_width, "cm"),
      y = 0.5,
      width = unit(logo_width, "cm"),
      height = unit(1, "npc"),
      just = c("right", "center"),
      clip = "off"
    )

    header_grob <- grid::gTree(children = grid::gList(
      grey_box,
      grid::editGrob(toptitle, vp = title_vp),
      grid::editGrob(logogrob, vp = logo_vp)
    ))

    topsubtitle <- grid::grid.text(label = stored_subtitle,
                                   x = unit(0, "npc"),
                                   y = unit(0.5, "npc"),
                                   draw = F,
                                   just = c("left", "top"),
                                   gp = gpar(col = "black",
                                             fontsize = subtitle_font_size,
                                             lineheight = 0.9,
                                             fontfamily = main_font))

    # Create caption (positioned with 0.4cm gap above it)

    topcaption <- grid::grid.text(label = stored_caption,
                                  x = unit(0, "npc"),
                                  y = unit(1, "npc"),  # Top of caption area
                                  draw = FALSE,
                                  just = c("left", "top"),
                                  gp = gpar(col = "black",
                                            fontsize = caption_font_size,
                                            lineheight = 0.9,
                                            fontfamily = main_font))

    # Layout using patchwork
    layout <- "
    HHH
    SSS
    PPP
    CCC
    "

    subtitle_present <- !is.null(stored_subtitle)
    caption_present <- !is.null(stored_caption) && stored_caption != ""

    subtitle_height <- ifelse(subtitle_present, 1.82, 0)
    caption_height <- ifelse(caption_present, 2.13 - 0.4, 0)  # Total space minus gap
    caption_gap <- ifelse(caption_present, 0.4, 0)

    wrap_plots(H = wrap_elements(full = header_grob),
               S = wrap_elements(full = topsubtitle),
               P = wrap_elements(full = p),
               C = wrap_elements(full = topcaption),
               design = layout,
               heights = unit(c(3.2,                    # Grey box with title/logo
                                subtitle_height,         # Subtitle area
                                chosen_chart_type$height,                    # Chart panel (matches sysdata)
                                caption_gap + caption_height),  # Gap + caption
                              "cm")) +
      plot_annotation(theme = theme(plot.margin = margin(top_border,
                                                         right_border,
                                                         bottom_border,
                                                         left_border,
                                                         "cm")))
}
