
#' @importFrom purrr walk walk2
grattan_save_pptx_2s <- function(plot,
                              filename,
                              type = "fullslide") {

  pptx_types <-  c("fullslide",
                   "fullslide_169",
                   "normal",
                   "wholecolumn")

  allowed_types <- c(pptx_types, "all")

  for (i in seq_along(type)) {
    if (!type[[i]] %in% allowed_types) {
      stop(
        "type '",
        type[[i]],
        "' is not one of the allowed types: ",
        paste(allowed_types, collapse = ", "),
        "."
      )
    }
  }

  multiple_types <- ifelse(type == "all" || length(type) > 1,
                           TRUE,
                           FALSE)

  if (length(type) == 1 && type == "all") {
    type <- pptx_types
  }

  if (isTRUE(multiple_types)) {
    output_dir <- tools::file_path_sans_ext(filename)

    if (isFALSE(dir.exists(output_dir))) {
      dir.create(output_dir, recursive = TRUE)
    }

    base_filename <- tools::file_path_sans_ext(basename(filename))
    filetype <- tools::file_ext(filename)
    filenames <- file.path(output_dir,
                           paste0(base_filename, "_", type, ".", filetype))
  } else {
    filenames <- filename
  }

  # plot must be either a single ggplot2 plot OR a list of ggplot2 plots
  # if a single plot, we want to create a list with the plot as the sole element
  if (inherits(plot, "gg")) {
    plot <- list(plot)
  } else if (class(plot) != "list") {
    stop("plot must be either a ggplot2 object or a list of ggplot2 objects.")
  } else {
    p_is_gg <- purrr::map_lgl(plot, inherits, "gg")
    if (isFALSE(all(p_is_gg))) {
      stop("plot is a list; not all elements of the list are ggplot2 objects.")
    }
  }

  walk2(.x = filenames,
        .y = type,
        .f = create_pptx_shell,
        p = plot)

  plot <- map(.x = plot,
              .f = wrap_labs,
              type = type,
              labs_to_wrap = "caption")

  walk(.x = filenames,
       .f = add_graph_to_pptx,
       p = plot)

  invisible(TRUE)
}


#' Create a Powerpoint document containing no content other than the
#' template + slide-specific speaker notes (extracted from a ggplot2 object)
#' @param p list containing ggplot2 object(s)
#' @param filename filename (incl. path if needed) to save a .pptx file
#' @param type graph type
#' @importFrom purrr map
create_pptx_shell <- function(p,
                              filename,
                              type) {
  pandoc_test()

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  # Get path to appropriate PPTX template from `grattantheme`
  template_filename <- switch (type,
                               "fullslide" = "template_43.pptx",
                               "fullslide_169" = "template_169.pptx",
                               "4:3" = "template_43.pptx",
                               "16:9" = "template_169.pptx")

  ref_doc <- paste0('    reference_doc: "',
                    system.file("extdata",
                                template_filename,
                                package = "grattantheme"),
                    '"')

  yaml_header <- paste("---",
                       "output:",
                       "  powerpoint_presentation: ",
                       ref_doc,
                       "---",
                       "\n",
                       sep = "\n")

  knitr_setup <- paste0(backticks,
                        "{r setup-makeslide-chunk, include=FALSE, message=FALSE}\n",
                        backticks,
                        "\n")

  temp_dir <- file.path(tempdir(), "pptx")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))
  }

  plot_areas <- purrr::map(p, create_slide_shell)

  plot_areas <- paste0(plot_areas, collapse = "")

  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_areas,
                   sep = "\n")

  temp_rmd_file <- file.path(temp_dir, "temp_rmd.Rmd")
  writeLines(fulldoc, temp_rmd_file)
  on.exit(unlink(temp_rmd_file))

  rmarkdown::render(temp_rmd_file,
                    output_file = basename(filename),
                    output_dir = dirname(filename),
                    quiet = TRUE)

  invisible(TRUE)
}

create_slide_shell <- function(p, type, temp_dir) {


  if (isTRUE(requireNamespace("rstudioapi")) &&
      isTRUE(rstudioapi::isAvailable())) {
    script_location <- rstudioapi::getActiveDocumentContext()$path
  } else {
    script_location <- ""
  }

  script_loc_note <- ifelse(script_location == "",
                            "",
                            paste0("R script location: ",
                                   script_location))


  plot_area <- paste("",
                     ":::::::::::::: {.columns}",
                     "::: {.column}",
                     "",
                     ":::",
                     "::: {.column}",
                     "",
                     ":::",
                     "::::::::::::::",
                     "::: notes",
                     paste("Title:",
                           p$labels$title,
                           "\n"),
                     paste("Subtitle:",
                           p$labels$subtitle,
                           "\n"),
                     paste(p$labels$caption,
                           "\n"),
                     script_loc_note,
                     ":::\n",
                     sep = "\n")

  plot_area
}


pandoc_test <- function() {

  if (!rmarkdown::pandoc_available()) {
    stop("To use this function, you must install 'pandoc' on your system",
         " from pandoc.org. See the grattantheme administrators",
         " if you need help.")
  }

  if (rmarkdown::pandoc_version() < 2.1) {
    stop("The version of 'pandoc' on your system is too old to use this",
         " function. Install a newer version from pandoc.org.",
         " See the grattantheme administrators if you need help.")
  }

  invisible(TRUE)

}

#' @importFrom officer read_pptx on_slide ph_with ph_location_label
#' @importFrom rvg dml
add_graph_to_pptx <- function(p,
                              filename) {
  x <- read_pptx(filename)
  num_slides <- length(x)

  if (length(p) != num_slides) {
    stop("Number of slides in shell must match number of plots in list")
  }

  for (slide in seq_len(num_slides)) {

    plot <- p[[slide]]

    x <- on_slide(x, index = slide)

    x <- ph_with(x,
                 plot$labels$title,
                 location = ph_location_label("Title 1"))

    plot$labels$title <- NULL

    x <- ph_with(x,
                 plot$labels$subtitle,
                 location = ph_location_label("Content Placeholder 2"))

    plot$labels$subtitle <- NULL

    # pptx <- pptx %>%
    #   ph_with(p$labels$caption,
    #           location = ph_location_label("Footer Placeholder 5"))
    #

    # Add graph as SVG object
    x <- ph_with(x,
                 rvg::dml(ggobj = plot),
                 location = ph_location_label("Content Placeholder 3"))
  }

  print(x, filename)

}
