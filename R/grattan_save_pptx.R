#' Save a ggplot2 chart, or list of charts, as a Powerpoint presentation
#'
#' @description `grattan_save_pptx()` creates a Grattan-style Powerpoint
#' presentation with editable vector graphics and editable text. The title,
#' subtitle, and caption of your graph are placed in the appropriate places.
#' Speaker notes are included, which include the file path to the R script
#' from which you called `grattan_save_pptx()`. If you supply a list of plots,
#' your Powerpoint presentation will include multiple slides -- one per plot.
#' If you specify multiple types, multiple PPTX files will be created.
#'
#' @param p ggplot2 plot, or a list of ggplot2 plots
#' @param filename Filename (including path where necessary) to save your
#' Powerpoint presentation.
#' @param type Chart type. If you specify multiple types, as
#' in `type = c("fullslide", "wholecolumn")` or `type = "all"`, multiple
#' files will be created, with the type added to the filename.
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
#' }
#' @export
#' @importFrom purrr walk walk2
grattan_save_pptx <- function(p = ggplot2::last_plot(),
                              filename,
                              type = "fullslide") {

  plot <- p
  pptx_types_inc_deprecated <- chart_types_inc_deprecated$type[!is.na(chart_types_inc_deprecated$pptx_template)]

  allowed_types <- c(pptx_types_inc_deprecated, "all")

  non_conforming_types <- type[!type %in% allowed_types]

  if (length(non_conforming_types) > 0) {
    stop("Type ", paste(non_conforming_types, collapse = ", "),
         " is not one of the allowed types: ",
         paste(allowed_types, collapse = ", "), ".")
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

  output_dir <- tools::file_path_sans_ext(filename)

  if (isFALSE(dir.exists(output_dir))) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_filename <- tools::file_path_sans_ext(basename(filename))
  filetype <- tools::file_ext(filename)
  filenames <- file.path(output_dir,
                         paste0(base_filename, "_", type, ".", filetype))

  # plot must be either a single ggplot2 plot OR a list of ggplot2 plots
  # if a single plot, we want to create a list with the plot as the sole element
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

  purrr::walk2(
    .x = filenames,
    .y = type,
    .f = ~add_graph_to_pptx(filename = .x,
                            type = .y,
                            p = plot,
                            num_slides = num_slides)
  )

  ggplot2::set_last_plot(p)
  invisible(TRUE)
}

#' Take a pre-existing PPTX document with n slides and a list of n ggplot2
#' objects; add one object to each slide using officer
#'
#' @param p a ggplot
#' @param filename filename incl. path to an existing PPTX file
#' @param type a grattantheme chart type with a PPTX template
#' @param num_slides number of slides to make
#'
#' @keywords internal
#' @importFrom officer read_pptx on_slide ph_with ph_location_label
#' @importFrom rvg dml
add_graph_to_pptx <- function(p,
                              filename,
                              type,
                              num_slides) {

  message(glue::glue("Making {type}"))

  # Get path to appropriate PPTX template from `grattantheme`
  template_filename <- system.file("extdata",
                                   chart_types_inc_deprecated$pptx_template[chart_types_inc_deprecated$type == type],
                                   package = "grattantheme")

  p <- purrr::map(
    .x = p,
    .f = wrap_labs,
    type = type,
    labs_to_wrap = "caption"
  )

  master <- dplyr::if_else(type == "fullslide_old", "Charts for overheads", "Office Theme")
  x <- officer::read_pptx(template_filename)

  for (slide in seq_len(num_slides)) {
    notes <- return_script_and_chart_location(filename = filename)

    plot <- p[[slide]]

    x <- x %>%
      officer::add_slide(layout="Two Content", master = master) %>%
      officer::set_notes(value = notes, officer::notes_location_type(type = "body"))

    replace_null <- function(string) {
      ifelse(is.null(string), " ", string)
    }

    labs <- extract_labs(plot)

    x <- officer::ph_with(x,
                 replace_null(labs$title),
                 location = officer::ph_location_label("Title 1"))

    x <- officer::ph_with(x,
                 replace_null(labs$subtitle),
                 location = officer::ph_location_label("Content Placeholder 2"))

    x <- officer::ph_with(x,
                 replace_null(labs$caption),
                 location = officer::ph_location_label("Caption Placeholder 1"))


    plot <- replace_labs(plot)


    # Add caption back in for non-report-bound chart types
    chart_class <- chart_types_inc_deprecated$class[chart_types_inc_deprecated$type == type]
    # Is the chart bound for a report?
    report_bound <- chart_class == "normal"

    if (!report_bound) {
      plot <- replace_labs(plot, list(title = NULL,
                                      subtitle = NULL,
                                      caption = NULL))
    }

    # Define the graph location; if no subtitle exists OR the chart is
    # report bound, we want to fill the subtitle space with the graph
    if (!is.null(labs$subtitle) | report_bound) {
      graph_location <- officer::ph_location_label("Content Placeholder 3")
    } else {
      slide_summ <- officer::slide_summary(x)
      v_offset <- 0.12
      graph_location <- officer::ph_location(
        left = slide_summ$offx[slide_summ$ph_label == "Content Placeholder 2"],
        top = slide_summ$offy[slide_summ$ph_label == "Content Placeholder 2"] +
          v_offset,
        width = slide_summ$cx[slide_summ$ph_label == "Content Placeholder 2"],
        height = officer::slide_size(x)$height -
          slide_summ$offy[slide_summ$ph_label == "Content Placeholder 2"] -
          v_offset
        )

    }

    # Add graph as SVG object
    x <- officer::ph_with(x,
                 rvg::dml(ggobj = plot),
                 location = graph_location)

    if (is.null(labs$subtitle)) {
      x <- officer::ph_remove(x, ph_label = "Content Placeholder 2")

    }
  }

  print(x, filename)

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
  if (isTRUE(requireNamespace("rstudioapi")) &&
      isTRUE(rstudioapi::isAvailable())) {
    script_location <- rstudioapi::getActiveDocumentContext()$path
  } else {
    script_location <- ""
  }

  graph_location <- normalizePath(filename, mustWork = FALSE)

  script_location <- gsub("^.*(?=(Dropbox))", "", script_location, perl = TRUE)
  graph_location <- gsub("^.*(?=(Dropbox))", "", graph_location, perl = TRUE)

  script_loc_note <- ifelse(script_location == "",
                            "",
                            paste0("R script location: ",
                                   script_location))

  graph_loc_note <- paste0("Powerpoint file location: ",
                           graph_location)

  final_note <- paste0(script_loc_note, "\n", graph_loc_note)

  return(final_note)
}

#' Check that pandoc is available and is an appropriate version
#' @keywords internal
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


### deprecated 1.0.0------------------

#' Create a Powerpoint document containing no content other than the
#' template + slide-specific speaker notes (extracted from a ggplot2 object)
#' @param p list containing ggplot2 object(s)
#' @param filename filename (incl. path if needed) to save a .pptx file
#' @param type graph type
#' @importFrom purrr map
#' @keywords internal
create_pptx_shell <- function(p,
                              filename,
                              type) {

  lifecycle::deprecate_stop(when = "1.0.0",
                            what = "create_pptx_shell()",
                            details = "We no longer make a powerpoint shell in markdown, as officer has functionality to edit the notes directly.")
  pandoc_test()

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  # Get path to appropriate PPTX template from `grattantheme`
  template_filename <- system.file("extdata",
                                   chart_types_inc_deprecated$pptx_template[chart_types_inc_deprecated$type == type],
                                   package = "grattantheme")

  if (isFALSE(file.exists(template_filename))) {
    stop("Could not find template for type ", type)
  }

  ref_doc <- paste0('    reference_doc: "',
                    template_filename,
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
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE))
  }

  plot_areas <- purrr::map2(p, filename, create_slide_shell)

  plot_areas <- paste0(plot_areas, collapse = "")

  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_areas,
                   sep = "\n")

  temp_rmd_file <- file.path(temp_dir, "temp_rmd.Rmd")
  writeLines(fulldoc, temp_rmd_file, useBytes = TRUE)
  on.exit(unlink(temp_rmd_file))

  rmarkdown::render(temp_rmd_file,
                    output_file = basename(filename),
                    output_dir = dirname(filename),
                    quiet = TRUE)

  invisible(TRUE)
}

#' Generate the R Markdown code for an individual slide in a PPTX shell
#' @importFrom rstudioapi getActiveDocumentContext
#' @keywords internal
create_slide_shell <- function(p, filename) {
  lifecycle::deprecate_stop(when = "1.0.0",
                            what = "create_slide_shell()",
                            details = "We no longer make a powerpoint shell in markdown, as officer has functionality to edit the notes directly.")

  if (isTRUE(requireNamespace("rstudioapi")) &&
      isTRUE(rstudioapi::isAvailable())) {
    script_location <- rstudioapi::getActiveDocumentContext()$path
  } else {
    script_location <- ""
  }

  graph_location <- normalizePath(filename, mustWork = FALSE)

  script_location <- gsub("^.*(?=(Dropbox))", "", script_location, perl = TRUE)
  graph_location <- gsub("^.*(?=(Dropbox))", "", graph_location, perl = TRUE)

  script_loc_note <- ifelse(script_location == "",
                            "",
                            paste0("R script location: ",
                                   script_location))

  graph_loc_note <- paste0("Powerpoint file location: ",
                           graph_location)

  plot_labels <- extract_labs(p)

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
                           plot_labels$title,
                           "\n"),
                     paste("Subtitle:",
                           plot_labels$subtitle,
                           "\n"),
                     paste(plot_labels$caption,
                           "\n"),
                     script_loc_note,
                     graph_loc_note,
                     ":::\n",
                     sep = "\n")

  plot_area
}

