# Package-level font configuration storage
.grattan_fonts <- new.env(parent = emptyenv())

#' Initialize font configuration on package load
#'
#' Uses Avenir Next and DM Serif Display for slide charts if they are installed
#' on the computer, and the default sans-serif font otherwise.
#'
#' @return Invisible NULL
#' @keywords internal
#' @importFrom systemfonts system_fonts
setup_grattan_fonts <- function() {

  installed_fonts <- systemfonts::system_fonts()$family

  .grattan_fonts$normal <- "sans"

  .grattan_fonts$slide_title <-
    if ("DM Serif Display" %in% installed_fonts) "DM Serif Display" else "sans"

  .grattan_fonts$slide_body <-
    if ("Avenir Next" %in% installed_fonts) "Avenir Next" else "sans"

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

  normal_font <- get_grattan_font("normal")
  slide_title <- get_grattan_font("slide", "title")
  slide_body <- get_grattan_font("slide", "body")

  display <- function(font) {
    if (font == "sans") "Default sans-serif font" else font
  }

  rule <- paste(rep("\u2500", 40), collapse = "")
  header <- paste0("\u2500\u2500 Fonts ", rule, "\n")

  slide_display <- if (slide_title == slide_body) {
    display(slide_body)
  } else {
    paste0(display(slide_title), " (title) + ", display(slide_body), " (body)")
  }

  missing_fonts <- c("DM Serif Display", "Avenir Next")[
    c(slide_title == "sans", slide_body == "sans")
  ]

  status <- if (length(missing_fonts) == 0) {
    "Full font support for all outputs."
  } else {
    paste0("Install ", paste(missing_fonts, collapse = " and "),
           " on this computer to make slide charts in the Grattan style.")
  }

  paste0(
    header,
    "'normal' - ", display(normal_font), "\n",
    "'slide'  - ", slide_display, "\n",
    status
  )
}
