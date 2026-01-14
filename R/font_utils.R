# Package-level font configuration storage
.grattan_fonts <- new.env(parent = emptyenv())

#' Get path to shared font files
#'
#' @return Character string path to font folder, or NULL if not found
#' @keywords internal
get_font_folder_path <- function() {

  dropbox_info_location <-
    if (Sys.getenv("OS") == "Windows_NT") {
      file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
    } else {
      "~/.dropbox/info.json"
    }

  if (!file.exists(dropbox_info_location)) {
    return(NULL)
  }

  dropbox_info <- tryCatch(
    jsonlite::fromJSON(dropbox_info_location),
    error = function(e) NULL
  )

  if (is.null(dropbox_info) || !"business" %in% names(dropbox_info)) {
    return(NULL)
  }

  font_path <- file.path(dropbox_info$business$root_path,
                         "Grattan Team", "Templates", "Font")

  if (dir.exists(font_path)) font_path else NULL
}

#' Register fonts from shared folder using systemfonts
#'
#' @param font_folder Path to the font folder
#' @return List with registration status for each font family
#' @keywords internal
#' @importFrom systemfonts register_font register_variant font_feature
register_shared_fonts <- function(font_folder) {

  result <- list(avenir_next = FALSE, dm_serif = FALSE)

  # Register Avenir Next from slide.ttc if available
  avenir_path <- file.path(font_folder, "slide.ttc")

  if (file.exists(avenir_path)) {
    tryCatch({
      systemfonts::register_font(
        name = "Avenir Next",
        plain = avenir_path,
        bold = avenir_path,
        italic = avenir_path,
        bolditalic = avenir_path,
        features = systemfonts::font_feature()
      )
      systemfonts::register_variant(
        name = "Avenir Next",
        plain = list(path = avenir_path, index = 7L),
        bold = list(path = avenir_path, index = 0L),
        italic = list(path = avenir_path, index = 4L),
        bolditalic = list(path = avenir_path, index = 1L)
      )
      result$avenir_next <- TRUE
    }, error = function(e) NULL)
  }

  # Register DM Serif Display
  dm_serif_folder <- file.path(font_folder, "slide_title")
  dm_regular <- file.path(dm_serif_folder, "DMSerifDisplay-Regular.ttf")
  dm_italic <- file.path(dm_serif_folder, "DMSerifDisplay-Italic.ttf")

  if (file.exists(dm_regular)) {
    tryCatch({
      systemfonts::register_font(
        name = "DM Serif Display",
        plain = dm_regular,
        bold = dm_regular,
        italic = if (file.exists(dm_italic)) dm_italic else dm_regular,
        bolditalic = if (file.exists(dm_italic)) dm_italic else dm_regular,
        features = systemfonts::font_feature()
      )
      result$dm_serif <- TRUE
    }, error = function(e) NULL)
  }

  result
}

#' Initialize font configuration on package load
#'
#' @return Invisible NULL
#' @keywords internal
#' @importFrom systemfonts system_fonts registry_fonts
setup_grattan_fonts <- function() {

  # Defaults
  .grattan_fonts$normal <- "sans"
  .grattan_fonts$slide_title <- "sans"
  .grattan_fonts$slide_body <- "sans"

  # Track how fonts were found (for informative startup message)
  .grattan_fonts$avenir_source <- "none"
  .grattan_fonts$dm_serif_source <- "none"

  # Get system-installed fonts

  system_fonts <- systemfonts::system_fonts()$family

  # Check for system-installed fonts first
  if ("DM Serif Display" %in% system_fonts) {
    .grattan_fonts$slide_title <- "DM Serif Display"
    .grattan_fonts$dm_serif_source <- "system"
  }

  if ("Avenir Next" %in% system_fonts) {
    .grattan_fonts$slide_body <- "Avenir Next"
    .grattan_fonts$avenir_source <- "system"
  } else {
    # Try to load from shared folder via systemfonts registration
    font_folder <- get_font_folder_path()

    if (!is.null(font_folder)) {
      registration <- register_shared_fonts(font_folder)

      if (registration$avenir_next) {
        .grattan_fonts$slide_body <- "Avenir Next"
        .grattan_fonts$avenir_source <- "registered"
      }
      if (registration$dm_serif && .grattan_fonts$slide_title == "sans") {
        .grattan_fonts$slide_title <- "DM Serif Display"
        .grattan_fonts$dm_serif_source <- "registered"
      }
    }
  }

  invisible(NULL)
}

#' Get font for chart type
#'
#' @param type Either "normal" or "slide"
#' @param element For "slide" type, either "title" or "body". Ignored for "normal".
#' @return Character string with font family name
#' @keywords internal
get_grattan_font <- function(type = c("normal", "slide"),
                             element = c("body", "title")) {
  type <- match.arg(type)
  element <- match.arg(element)

  if (type == "normal") {
    font <- .grattan_fonts$normal
    return(if (is.null(font)) "sans" else font)
  }

  if (element == "title") {
    font <- .grattan_fonts$slide_title
    return(if (is.null(font)) "sans" else font)
  }

  font <- .grattan_fonts$slide_body
  if (is.null(font)) "sans" else font
}

#' Get startup message about font configuration
#'
#' @return Character string with font status message
#' @keywords internal
get_font_status_message <- function() {

  normal_font <- if (is.null(.grattan_fonts$normal)) "sans" else .grattan_fonts$normal
  slide_title <- if (is.null(.grattan_fonts$slide_title)) "sans" else .grattan_fonts$slide_title
  slide_body <- if (is.null(.grattan_fonts$slide_body)) "sans" else .grattan_fonts$slide_body
  avenir_source <- if (is.null(.grattan_fonts$avenir_source)) "none" else .grattan_fonts$avenir_source
  dm_serif_source <- if (is.null(.grattan_fonts$dm_serif_source)) "none" else .grattan_fonts$dm_serif_source

  rule <- paste(rep("\u2500", 40), collapse = "")
  header <- paste0("\u2500\u2500 Fonts ", rule, "\n")

  # Format font names for display
  normal_font_display <- if (normal_font == "sans") "Default sans-serif font" else normal_font

  # Build the slide font display string
  if (slide_title == slide_body) {
    slide_display <- if (slide_body == "sans") "Default sans-serif font" else slide_body
  } else {
    slide_title_display <- if (slide_title == "sans") "Default sans-serif font" else slide_title
    slide_body_display <- if (slide_body == "sans") "Default sans-serif font" else slide_body
    slide_display <- paste0(slide_title_display, " (title) + ", slide_body_display, " (body)")
  }

  # Case 1: No Grattan fonts found at all
  if (slide_title == "sans" && slide_body == "sans") {
    return(paste0(
      header,
      "'normal' - ", normal_font_display, "\n",
      "'slide'  - ", slide_display, "\n",
      "Required fonts not found - install Avenir Next and DM Serif Display."
    ))
  }

  # Case 2: All fonts found via system install (best case - works everywhere)
  if (avenir_source == "system" &&
      (dm_serif_source == "system" || slide_title == "sans")) {
    return(paste0(
      header,
      "'normal' - ", normal_font_display, "\n",
      "'slide'  - ", slide_display, " (system-installed)\n",
      "Full font support for all outputs."
    ))
  }

  # Case 3: Fonts registered via systemfonts (works for PNG, not PDF/PPTX)
  if (avenir_source == "registered" || dm_serif_source == "registered") {
    registered_fonts <- c()
    if (avenir_source == "registered") registered_fonts <- c(registered_fonts, "Avenir Next")
    if (dm_serif_source == "registered") registered_fonts <- c(registered_fonts, "DM Serif Display")

    return(paste0(
      header,
      "'normal' - ", normal_font_display, "\n",
      "'slide'  - ", slide_display, "\n",
      paste(registered_fonts, collapse = ", "), " loaded from Dropbox.\n",
      "- PNG or JPEG outputs will render correctly\n",
      "- PDF slide fonts require system install to render in Grattan style\n",
      "- PPTX slide chart fonts will require manual adjustment."
    ))
  }

  # Case 4: Mixed - some system, some missing
  paste0(
    header,
    "'normal' - ", normal_font_display, "\n",
    "'slide'  - ", slide_display, "\n",
    "Some fonts may not render in Grattan style."
  )
}
