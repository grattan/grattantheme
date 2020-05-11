
grattan_save_pptx <- function(plot,
                              filename,
                              type = "fullslide") {

  pptx_types <-  c("fullslide",
                      "fullslide_169",
                      "normal",
                      "wholecolumn")

  allowed_types <- c(pptx_types, "all")

  for (i in seq_along(type)) {
    if (!type[[i]] %in% allowed_types) {
      stop("type '", type[[i]], "' is not one of the allowed types: ",
           paste(allowed_types, collapse = ", "), ".")
    }
  }

  multiple_types <- ifelse(type == "all" || length(type) > 1,
                           TRUE,
                           FALSE)

  if (length(type) == 1) {
    if (type == "all") {
      type <- pptx_types
    }
  }

  if (isTRUE(multiple_types)) {
    output_dir <- tools::file_path_sans_ext(filename)

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    base_filename <- tools::file_path_sans_ext(basename(filename))
    filetype <- tools::file_ext(filename)

    filenames <- file.path(output_dir,
                           paste0(base_filename, "_", type, ".", filetype))
  } else {
    filenames <- filename
  }

  walk2(.x = filenames,
        .y = type,
        .f = grattan_save_pptx_onetype,
        p = plot)

  invisible(TRUE)

}

#' Internal function to create a Grattan-branded PPTX slide using RMd
#' Called by grattan_save_pptx(). This function saves slide(s) in a single
#' format.
#' @param p ggplot2 plot or list of plots
#' @param filename filename (incl. path where necessary) to save file
#' @param type type of slide; see \code{?grattan_save_pptx()} for types
grattan_save_pptx_onetype <- function(p,
                                      filename,
                                      type) {

  pandoc_test()

  # p must be either a single ggplot2 plot OR a list of ggplot2 plots
  # if a single plot, we want to create a list with the plot as the sole element
  if (inherits(p, "gg")) {
    p <- list(p)
  } else if (class(p) != "list") {
    stop("p must be either a ggplot2 object or a list of ggplot2 objects.")
  } else {
    p_is_gg <- purrr::map_lgl(p, inherits, "gg")
    if (isFALSE(all(p_is_gg))) {
      stop("p is a list; not all elements of the list are ggplot2 objects.")
    }
  }

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  # Get path to appropriate PPTX template from `grattantheme`
  template_filename <- switch (type,
                               "fullslide" = "template_43.pptx",
                               "fullslide_169" = "template_169.pptx",
                               "4:3" = "template_43.pptx")

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

  plot_areas <- list()
  plot_filenames <- list()

  for(i in seq_along(p)) {
    slide_rmd <- generate_slide_rmd(p = p[[i]],
                                    type = type,
                                    temp_dir = temp_dir)

    plot_areas[[i]] <- slide_rmd$plot_area
    plot_filenames[[i]] <- slide_rmd$plot_filename

  }

  plot_areas <- paste0(plot_areas, collapse = "")
  on.exit(unlink(plot_filenames))

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



#' Create the RMd for a single-page Powerpoint slide with a graph, title,
#' and subtitle. Does not include YAML header or title page.
#' @param p ggplot2 plot to add to a Powerpoint slide
#' @param type chart type
#' @noRd
#'
generate_slide_rmd <- function(p, type, temp_dir) {
  raw_title <- p$labels$title
  raw_subtitle <- p$labels$subtitle

  graph_title <- paste0("## ", raw_title)
  graph_subtitle <- raw_subtitle

  split_caption <- gsub(" source", " \n\nSource", p$labels$caption,
                        ignore.case = TRUE)

  p <- wrap_labs(p, type = type)

  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  plot_basename <- paste0(sample(letters, 12, replace = TRUE),
                          collapse = "")

  plot_filename <- file.path(temp_dir,
                             paste0(plot_basename,
                                    ".png"))

  ggsave(filename = plot_filename,
         plot = p,
         height = 14.5,
         width = ifelse(type %in% c("16:9", "fullslide_169"),
                        30, 22.2),
         dpi = "retina",
         units = "cm")

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


  plot_area <- paste(graph_title,
                     ":::::::::::::: {.columns}",
                     "::: {.column}",
                     graph_subtitle,
                     ":::",
                     "::: {.column}",
                     paste0("![](",
                            #plot_filename,
                            basename(plot_filename),
                            ")"),
                     ":::",
                     "::::::::::::::",
                     "::: notes",
                     paste("Title:",
                           raw_title,
                           "\n"),
                     paste("Subtitle:",
                           raw_subtitle,
                           "\n"),
                     paste(split_caption,
                           "\n"),
                     script_loc_note,
                     ":::\n",
                     sep = "\n")

  list(plot_filename = plot_filename,
       plot_area = plot_area)
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

