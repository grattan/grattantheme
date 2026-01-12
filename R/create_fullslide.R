#' Assemble a chart featuring the Grattan logo and grey header
#'
#' Takes a ggplot2 object and formats it to look like a
#' Grattan Powerpoint slide. The function adds a grey header box containing
#' the chart title and Grattan logo, along with subtitle and caption text.
#' Title text is automatically wrapped if longer than one line.
#' You will rarely need to call this function directly - use `grattan_save()`
#' to save a ggplot2 object as a 'slide'-like image.
#'
#' The function allows for three types with different layouts:
#' \itemize{
#'   \item{fullslide: Standard width chart with left-aligned text/logo}
#'   \item{fullslide_narrow: Narrower centered chart with text/logo aligned consistently with fullslide}
#'   \item{fullslide_half: Half-width left-aligned chart for side-by-side layouts}
#' }
#'
#' @param plot A ggplot2 plot
#' @param type Optional. If specified, must be one of "fullslide", "fullslide_narrow", or "fullslide_half".
#' This determines the chart width and positioning within the slide.
#' @param font Either "slide" (default) or "normal". "slide" uses DM Serif Display
#' for the title and Avenir Next for body text (if available). "normal" uses Arial.
#'
#' @return An object of class "patchwork" with full slide dimensions (16:9 PowerPoint slide).
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
                             type,
                             font = c("slide", "normal")) {

    # Check inputs and define plot borders ----

    if (!inherits(plot, "ggplot")) {
      stop(deparse(substitute(plot)), " is not a ggplot2 object.")
    }

    if (missing(type)) {
      # Default to fullslide type when not specified
      type <- "fullslide"
    }

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

    # Create title and subtitle -----
    p <- plot

    labs <- extract_labs(p)

    p <- replace_labs(p,
                      labs = list(title = NULL,
                                  subtitle = NULL,
                                  caption = NULL))

    # Determine fonts based on font parameter
    font <- match.arg(font)
    title_font <- get_grattan_font(font, "title")
    main_font <- get_grattan_font(font, "body")

    stored_title <- labs$title
    stored_subtitle <- labs$subtitle
    stored_caption <- labs$caption

    title_font_size <- 32
    subtitle_font_size <- 18
    caption_font_size <- 8

    # Wrap title text to fit within available space (max ~52 chars per line)
    # This prevents text from clipping behind the logo
    title_is_multiline <- FALSE
    if (!is.null(stored_title) && nchar(stored_title) > 52) {
      stored_title <- paste(strwrap(stored_title, width = 52), collapse = "\n")
      title_is_multiline <- TRUE
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
    # Adjust vertical position based on whether title is single or multi-line
    title_y <- if (title_is_multiline) 0.42 else 0.55

    toptitle <- grid::textGrob(label = stored_title,
                               x = 0,
                               y = title_y,
                               just = c("left", "center"),
                               gp = gpar(col = "black",
                                         fontsize = title_font_size,
                                         lineheight = 0.9,
                                         fontfamily = title_font))

    # Create header grob that combines grey box + title + logo
    # The title and logo positioning:
    # - For fullslide and fullslide_half: align with chart left edge
    # - For fullslide_narrow: align with fullslide's position (not the narrow chart edge)
    #   This means offsetting left from the narrow chart by (narrow_border - fullslide_border)
    standard_fullslide_width <- chart_types$width[chart_types$type == "fullslide"]
    standard_fullslide_left_border <- chart_types$left_border[chart_types$type == "fullslide"]
    logo_width <- 4  # Width of logo in cm

    # Calculate x offset for title/logo
    # For fullslide_narrow (centered chart), we need to shift left to match fullslide position
    title_logo_x_offset <- if (left_border == right_border && left_border != standard_fullslide_left_border) {
      -(left_border - standard_fullslide_left_border)
    } else {
      0
    }

    # Title spans standard fullslide width minus logo width
    title_width <- standard_fullslide_width - logo_width - 0.1

    # Create viewport for title
    title_vp <- grid::viewport(
      x = unit(0, "npc") + unit(title_logo_x_offset, "cm"),
      y = 0.5,
      width = unit(title_width, "cm"),
      just = c("left", "center"),
      clip = "off"
    )

    # Create viewport for logo
    logo_vp <- grid::viewport(
      x = unit(0, "npc") + unit(title_logo_x_offset + standard_fullslide_width, "cm"),
      y = 0.35,
      width = unit(logo_width, "cm"),
      just = c("right", "center"),
      clip = "off"
    )

    header_grob <- grid::gTree(children = grid::gList(
      grey_box,
      grid::editGrob(toptitle, vp = title_vp),
      grid::editGrob(logogrob, vp = logo_vp)
    ))

    # Subtitle and caption use the same x offset as title/logo for fullslide_narrow
    # They need viewports with clip="off" to allow drawing outside panel boundaries

    subtitle_grob <- grid::textGrob(label = stored_subtitle,
                                    x = 0,
                                    y = unit(0.7, "npc"),
                                    just = c("left", "top"),
                                    gp = gpar(col = "black",
                                              fontsize = subtitle_font_size,
                                              lineheight = 0.9,
                                              fontfamily = main_font))

    subtitle_vp <- grid::viewport(
      x = unit(0, "npc") + unit(title_logo_x_offset, "cm"),
      y = 0,
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      just = c("left", "bottom"),
      clip = "off"
    )

    topsubtitle <- grid::editGrob(subtitle_grob, vp = subtitle_vp)

    # Create caption (positioned with 0.4cm gap above it)

    caption_grob <- grid::textGrob(label = stored_caption,
                                   x = 0,
                                   y = unit(1, "npc"),
                                   just = c("left", "top"),
                                   gp = gpar(col = "black",
                                             fontsize = caption_font_size,
                                             lineheight = 0.9,
                                             fontfamily = main_font))

    caption_vp <- grid::viewport(
      x = unit(0, "npc") + unit(title_logo_x_offset, "cm"),
      y = 0,
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      just = c("left", "bottom"),
      clip = "off"
    )

    topcaption <- grid::editGrob(caption_grob, vp = caption_vp)

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
