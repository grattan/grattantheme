#' Save a ggplot2 chart, or list of charts, as a Powerpoint presentation
#'
#' @description `grattan_save_pptx()` creates a Grattan-style Powerpoint
#' presentation with editable vector graphics and editable text. The title,
#' subtitle, and caption of your graph are placed in the appropriate places.
#' Speaker notes are included, which include the file path to the R script
#' from which you called `grattan_save_pptx()`. If you supply a list of plots,
#' your Powerpoint presentation will include multiple slides -- one per plot.
#' If you specify multiple types, multiple PPTX files will be created. This
#' function is usually best called by running `grattan_save(..., save_pptx = T)`
#' or `grattan_save_all()`, rather than calling it directly.
#'
#' @param p ggplot2 plot, or a list of ggplot2 plots
#' @param filename Filename (including path where necessary) to save your
#' Powerpoint presentation.
#' @param type Chart type. If you specify multiple types, as
#' in `type = c("fullslide", "wholecolumn")` or `type = "all"`, multiple
#' files will be created, with the type added to the filename. `type = "all"`
#' covers the current chart types only. The deprecated `fullslide_old` and
#' `fullslide_43` types are also accepted, with a warning, so that a Powerpoint
#' deck built in one of those formats can be regenerated; they are not valid
#' anywhere else in the package.
#' @param rich_subtitle Logical. If `TRUE`, the plot will be saved as a high-quality PNG image and inserted into the slide. This is mainly intended for folks using a lot of markdown text in the subtitles and plots.
#' @param png_dpi Integer. The DPI of the PNG image saved when `rich_subtitle = TRUE`.
#' @param font Either "slide", "normal", or NULL (default). NULL automatically
#' uses "slide" for fullslide and blog chart types and "normal" for other
#' types. "slide" uses Avenir Next for body text (if available). "normal" uses
#' Arial.
#' @param author Character. The name recorded as the author in the Powerpoint
#' file's document properties. Defaults to `"Grattan Institute"`. Use `""` to
#' leave it blank. (The "last modified by" name is always your computer's
#' login name.)
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'        geom_point() +
#'        theme_grattan() +
#'        labs(title = "My title",
#'             subtitle = "My subtitle",
#'             caption = "Notes: notes go here. Source: source goes here.")
#'
#' grattan_save_pptx(p, "test.pptx")
#'
#' library(ggplot2)
#
# p2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#   geom_point() +
#   theme_grattan() +
#   theme(plot.subtitle = ggtext::element_markdown()) +
#   labs(title = "My title",
#        subtitle = "My **rich** subtitle",
#        caption = "Notes: notes go here. Source: source goes here.")
#
# grattan_save_pptx(p2, filename = "test.pptx", rich_subtitle = TRUE)
#'
#' }
#' @export
#' @importFrom purrr walk walk2
grattan_save_pptx <- function(filename,
                              p = ggplot2::last_plot(),
                              type = "fullslide",
                              rich_subtitle = FALSE,
                              png_dpi = 300,
                              font = NULL,
                              author = "Grattan Institute") {

  check_author(author)

  plot <- p

  # Deprecated types are still accepted here - and only here - so that an old
  # Powerpoint deck can be regenerated in its original format
  pptx_types <- chart_types_all$type[!is.na(chart_types_all$pptx_template)]

  allowed_types <- c(pptx_types, "all")

  non_conforming_types <- type[!type %in% allowed_types]

  if (length(non_conforming_types) > 0) {
    stop("Type ", paste(non_conforming_types, collapse = ", "),
         " is not one of the allowed types: ",
         paste(allowed_types, collapse = ", "), ".")
  }

  legacy_types <- intersect(type, pptx_legacy_types)

  if (length(legacy_types) > 0) {
    warning("Chart type ", paste(legacy_types, collapse = ", "),
            " is deprecated and is retained only so that older Powerpoint",
            " decks can be regenerated.\nUse one of ",
            paste(chart_types$type[!is.na(chart_types$pptx_template)],
                  collapse = ", "),
            " for new charts.",
            call. = FALSE)
  }

  multiple_types <- if (length(type) > 1) {
    TRUE
  } else if (type == "all") {
    TRUE
  } else {
    FALSE
  }

  if (length(type) == 1 && type == "all") {
    type <- chart_types$type[!is.na(chart_types$pptx_template)]
  }


  # get output directory
  filename <- as.character(filename)
  output_dir <- dirname(filename)

  base_filename <- tools::file_path_sans_ext(basename(filename))
  filetype <- tools::file_ext(filename)
  filenames <- file.path(output_dir,
                         paste0(base_filename, "_", type, ".", filetype))

  # plot must be either a single ggplot2 plot OR a list of ggplot2 plots
  if (inherits(plot, "gg")) {
    plot <- list(plot)
  } else if (!inherits(plot, "list")) {
    stop("plot must be either a ggplot2 object or a list of ggplot2 objects.")
  } else {
    p_is_gg <- purrr::map_lgl(plot, inherits, "gg")
    if (isFALSE(all(p_is_gg))) {
      stop("plot is a list; not all elements of the list are ggplot2 objects.")
    }
  }

  num_slides <- length(plot)

  # Fullslide and blog charts use the "slide" font, as they do in
  # `grattan_save()`; everything else uses "normal". A user-specified font
  # overrides that default.
  type_class <- chart_types_all$class[match(type, chart_types_all$type)]
  fonts <- if (!is.null(font)) {
    rep(font, length(type))
  } else {
    dplyr::if_else(type_class %in% c("fullslide", "blog"), "slide", "normal")
  }

  purrr::pwalk(
    list(filenames, type, fonts),
    function(fn, tp, fnt) {
      add_graph_to_pptx(filename = fn,
                        type = tp,
                        p = plot,
                        num_slides = num_slides,
                        rich_subtitle = rich_subtitle,
                        png_dpi = png_dpi,
                        font = fnt,
                        author = author)
    }
  )

  ggplot2::set_last_plot(p)
  invisible(TRUE)
}

add_graph_to_pptx <- function(p,
                              filename,
                              type,
                              num_slides,
                              rich_subtitle = FALSE,
                              png_dpi = 300,
                              font = "slide",
                              author = "Grattan Institute") {

  message(glue::glue("Making {type}"))

  # Get path to appropriate PPTX template
  template_filename <- system.file("extdata",
                                   chart_types_all$pptx_template[chart_types_all$type == type],
                                   package = "grattantheme")

  # Get PowerPoint dimensions
  pptx <- officer::read_pptx(template_filename)
  slide_size <- officer::slide_size(pptx)

  # Convert PowerPoint units (inches) to mm for ggsave
  width_mm <- slide_size$width * 25.4
  height_mm <- slide_size$height * 25.4

  # The caption goes into a PowerPoint text box, which wraps text itself, so
  # only split 'notes' from 'source' here and leave line wrapping to PowerPoint
  p <- purrr::map(.x = p, .f = split_notes_and_source)

  master <- dplyr::if_else(type == "fullslide_old", "Charts for overheads", "Office Theme")
  x <- pptx

  # Create temp directory for PNGs if using rich_subtitle
  if (rich_subtitle) {
    temp_dir <- tempfile("grattan_pptx_")
    dir.create(temp_dir)

    # Get the layout summary for the "Two Content" layout
    layout_props <- officer::layout_properties(pptx, layout = "Two Content")
    placeholder <- layout_props[layout_props$ph_label == "Content Placeholder 3", ]

    # Get placeholder dimensions in inches (officer uses inches internally)
    image_width_in <- placeholder$cx
    image_height_in <- placeholder$cy
  }

  for (slide in seq_len(num_slides)) {
    notes <- return_script_and_chart_location(filename = filename)
    plot <- p[[slide]]
    labs <- extract_labs(plot)

    # Set the body font on every part of the chart - for a patchwork plot,
    # that means each of its plots, not just the last one
    body_font <- get_grattan_font(font, "body")
    plot <- add_theme_to_all(plot,
                             theme(text = element_text(family = body_font)))
    plot <- apply_font_to_geom_text(plot, body_font)

    x <- x %>%
      officer::add_slide(layout="Two Content", master = master) %>%
      officer::set_notes(value = notes, officer::notes_location_type(type = "body"))

    replace_null <- function(string) {
      ifelse(is.null(string), " ", string)
    }

    if (rich_subtitle) {
      # Save as high-quality PNG using ragg (uses systemfonts)
      png_file <- file.path(temp_dir, paste0("slide_", slide, ".png"))

      plot <- add_theme_to_all(plot,
                               theme(plot.title = element_blank(),
                                     plot.caption = element_blank()))

      ragg::agg_png(png_file,
                    width = image_width_in,
                    height = image_height_in,
                    units = "in",
                    res = png_dpi,
                    background = "transparent")
      print(plot)
      grDevices::dev.off()

      # Add PNG to PowerPoint in content placeholder
      x <- officer::ph_with(x,
                            officer::external_img(png_file),
                            location = officer::ph_location_label("Content Placeholder 3"),
                            use_loc_size = TRUE)

    } else {
      # Use rvg::dml() for editable vector graphics
      x <- officer::ph_with(x,
                            replace_null(labs$subtitle),
                            location = officer::ph_location_label("Content Placeholder 2"))

      plot <- replace_labs(plot)

      # Build font mapping for rvg::dml()
      # Note: rvg uses gdtools for fonts, which only sees system-installed fonts
      pptx_fonts <- if (body_font == "sans") list() else list(sans = body_font)

      x <- officer::ph_with(x,
                            rvg::dml(ggobj = plot,
                                     bg = "transparent",
                                     fonts = pptx_fonts),
                            location = officer::ph_location_label("Content Placeholder 3"))
    }

    x <- officer::ph_with(x,
                          replace_null(labs$title),
                          location = officer::ph_location_label("Title 1"))

    x <- officer::ph_with(x,
                          replace_null(labs$caption),
                          location = officer::ph_location_label("Caption Placeholder 1"))


  }

  save_pptx_with_author(x, target = filename, author = author)
}


#' Return a charter vector of the filepath of the script and powerpoint location
#'
#' An internal function used in `add_graph_to_pptx` to add the location of the script that
#' made a chart and the location where the chart is saved.
#'
#' @param filename the filename of the powerpoint location
#'
#' @return a character
return_script_and_chart_location <- function(filename) {
  if (isTRUE(requireNamespace("rstudioapi", quietly = TRUE)) &&
      isTRUE(rstudioapi::isAvailable())) {
    script_location <- rstudioapi::getActiveDocumentContext()$path
  } else {
    script_location <- ""
  }

  graph_location <- normalizePath(filename, mustWork = FALSE)

  script_location <- trim_synced_path(script_location)
  graph_location <- trim_synced_path(graph_location)

  script_loc_note <- ifelse(script_location == "",
                            "",
                            paste0("R script location: ",
                                   script_location))

  graph_loc_note <- paste0("Powerpoint file location: ",
                           graph_location)

  final_note <- paste0(script_loc_note, "\n", graph_loc_note)

  return(final_note)
}


#' Shorten a file path to the part a colleague would recognise
#'
#' Paths in speaker notes are more useful without the start that is specific
#' to one person's computer. This keeps the path from the shared folder down:
#' \itemize{
#'   \item Dropbox: from "Dropbox" onwards, e.g.
#'     "Dropbox/Projects/chart.pptx".
#'   \item SharePoint libraries synced with OneDrive on a Mac: from the
#'     library onwards, e.g. "Team - Documents/Projects/chart.pptx".
#'   \item Anything else in the home folder, including OneDrive and SharePoint
#'     on Windows: from just below the home folder, e.g.
#'     "Grattan Institute/Team - Documents/Projects/chart.pptx".
#' }
#' Paths outside the home folder, such as network drives, are left as they are.
#'
#' @param path A file path.
#'
#' @return A character string.
#'
#' @keywords internal
trim_synced_path <- function(path) {

  if (path == "") return(path)

  path <- gsub("\\\\", "/", path)

  if (grepl("Dropbox", path, fixed = TRUE)) {
    return(sub("^.*(?=Dropbox)", "", path, perl = TRUE))
  }

  if (grepl("/Library/CloudStorage/", path, fixed = TRUE)) {
    path <- sub("^.*/Library/CloudStorage/", "", path)
    return(sub("^OneDrive-SharedLibraries-[^/]+/", "", path))
  }

  sub("^([A-Za-z]:)?/(Users|home)/[^/]+/", "", path)
}
