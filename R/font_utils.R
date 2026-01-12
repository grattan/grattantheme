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

  # Register Avenir Next from slide.ttc
  # Indices: 0=Bold, 1=Bold Italic, 4=Italic, 7=Regular
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

  # Get available fonts (system + any already registered)
  available <- c(
    systemfonts::system_fonts()$family,
    tryCatch(systemfonts::registry_fonts()$family, error = function(e) character(0))
  )

  # Check for system fonts first
  if ("DM Serif Display" %in% available) {
    .grattan_fonts$slide_title <- "DM Serif Display"
  }

  if ("Avenir Next" %in% available) {
    .grattan_fonts$slide_body <- "Avenir Next"
  } else {
    # Try to load from shared folder
    font_folder <- get_font_folder_path()

    if (!is.null(font_folder)) {
      registration <- register_shared_fonts(font_folder)

      if (registration$avenir_next) {
        .grattan_fonts$slide_body <- "Avenir Next"
      }
      if (registration$dm_serif && .grattan_fonts$slide_title == "sans") {
        .grattan_fonts$slide_title <- "DM Serif Display"
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
    return(.grattan_fonts$normal %||% "sans")
  }

  if (element == "title") {
    return(.grattan_fonts$slide_title %||% "sans")
  }

  .grattan_fonts$slide_body %||% "sans"
}

#' Get startup message about font configuration
#'
#' @return Character string with font status message
#' @keywords internal
get_font_status_message <- function() {

  normal <- .grattan_fonts$normal %||% "sans"
  slide_title <- .grattan_fonts$slide_title %||% "sans"
  slide_body <- .grattan_fonts$slide_body %||% "sans"

  if (slide_title == slide_body) {
    slide_display <- slide_body
  } else {
    slide_display <- paste0(slide_title, " (title) + ", slide_body, " (body)")
  }

  if (normal == "sans" && slide_title == "sans" && slide_body == "sans") {
    return("Fonts: using defaults")
  }

  paste0("Fonts: 'normal' = ", normal, ", 'slide' = ", slide_display)
}
