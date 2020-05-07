#' Create Powerpoint slides
#'
#' @description Use `make_slide()` to create a Powerpoint presentation
#'   from a Grattan graph, or list of graphs.
#'
#'   The slide will contain a Grattan graph and editable title
#'   and subtitle. The title, subtitle and caption will be added to speaker notes
#'   in the PPT slide. The path to your R script will also be added, if
#'   `make_slide()` is called from a script and the `rstudioapi` is present.
#'
#' @param graph The graph you want to include on your slide. Defaults to the
#'   last plot (ggplot2::last_plot())
#' @param filename The filename for your Powerpoint slide.
#' @param path Path to the directory you wish to save your slide in. Defaults to
#'   your working directory.
#' @param type The type of Powerpoint slide to create; either "16:9" (the
#'   default, for a 16:9 orientation slide), or "4:3".
#' @importFrom rmarkdown render
#' @importFrom knitr opts_chunk
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown pandoc_version pandoc_available
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @examples
#'
#' # To take a single ggplot2 graph and turn it into a slide:
#' # First, create your ggplot2 object. Include your subtitle,
#' # title, and caption with +labs()
#'
#'library(ggplot2)
#'library(grattantheme)
#'
#'ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_point() +
#'    theme_grattan() +
#'    labs(title = "Long title goes here with a bunch of text lorem ipsum
#'    Grattan blah blah sensible orange",
#'    subtitle = "Subtitle",
#'    caption = "Source: blah")
#'
#' # Now, create a Powerpoint slide with your graph in your working directory.
#' # By default, make_slide() will use the last plot you generated;
#' # you can specify a different object to use with the `graph` argument.
#'
#' \dontrun{make_slide(filename = "test.pptx")}
#'
#' @export


make_slide <- function(graph = last_plot(),
                       filename = NULL,
                       path = ".",
                       type = "16:9") {

  pandoc_test()

  if (!"gg" %in% class(graph)) {
    stop("The object is not a ggplot2 graph and",
         " cannot be plotted with make_slide()")
  }

  if (is.null(filename)) {
    stop("You must specify a filename (such as 'my_slide')",
         " for the Powerpoint slide you wish to create.")
  }

  if (is.null(path)) {
    stop("You must specify a path to the directory where",
         " you want your slide to be saved.")
  }

  if (is.null(type) | !type %in% c("16:9", "4:3")) {
    stop("You must specify what type of slide you want -",
         " either '16:9' (the default) or '4:3'")
  }

  p <- graph

  filename <- tools::file_path_sans_ext(filename)

  output_file <- paste0(filename, ".pptx")
  output_dir <- path

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # copy template to temporary directory
  if (type == "16:9") {
    template_source <- system.file(file.path("extdata", "template_169.pptx"),
                                   package = "grattantheme")
  } else {
    template_source <- system.file(file.path("extdata", "template_43.pptx"),
                                   package = "grattantheme")
  }


  temp_dir <- file.path(tempdir(), "make_slide")

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }

  on.exit(unlink(temp_dir, recursive = TRUE))

  temp_template <- file.path(temp_dir, basename(template_source))

  result_of_copy <- file.copy(from = template_source,
            to = temp_template,
            overwrite = TRUE)

  if (!result_of_copy) {
    stop("make_slide() encountered a problem copying the Powerpoint",
         " template to a temporary directory.")
  }

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  ref_doc <- paste0('    reference_doc: "',
                    #temp_template,
                    basename(template_source),
                    '"')

  yaml_header <- paste("---",
                       "output:",
                       "  powerpoint_presentation: ",
                       ref_doc,
                       "---",
                       "\n",
                       sep = "\n")

  knitr_optschunk <- ifelse(type == "16:9",
                            "knitr::opts_chunk$set(echo = FALSE,\nfig.height = 5.70,\nfig.width = 11.81,\nfig.retina = 2)\n",
                            "knitr::opts_chunk$set(echo = FALSE,\nfig.height = 5.70,\nfig.width = 8.66,\nfig.retina = 2)\n")

  knitr_setup <- paste0(backticks,
                        "{r setup-makeslide-chunk, include=FALSE, message=FALSE}\n",
                        knitr_optschunk,
                        backticks,
                        "\n")

  slide_rmd <- generate_slide_rmd(p = p,
                                 type = type,
                                 temp_dir = temp_dir)

  plot_area <- slide_rmd$plot_area
  plot_filename <- slide_rmd$plot_filename
  on.exit(unlink(plot_filename))

  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_area,
                   sep = "\n")

  temp_rmd_file <- file.path(temp_dir, "temp_rmd.Rmd")
  writeLines(fulldoc, temp_rmd_file)
  on.exit(unlink(temp_rmd_file))

  rmarkdown::render(temp_rmd_file,
                    output_file = output_file,
                    output_dir = output_dir,
                    quiet = TRUE)


}

#' @param graphs A list of ggplot2 objects
#' @param title Optional. The title of your presentation, if you want a title
#'   slide.
#' @param subtitle Optional. The subtitle of your presentation, if you want a
#'   title slide.
#' @rdname make_slide
#' @name make_presentation
#' @examples
#'
#' # To take multiple ggplot2 graphs and turn them into slides in a single
#' # presentation:
#' # First, create multiple ggplot2 objects. Include your subtitle, title,
#' # and caption with +labs()
#'
#'library(ggplot2)
#'library(grattantheme)
#'
#'graph1 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_point() +
#'    theme_grattan() +
#'    labs(title = "Title goes here with a bunch of text lorem ipsum",
#'    subtitle = "Subtitle",
#'    caption = "Source: blah")
#'
#'graph2 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_col() +
#'    theme_grattan() +
#'    labs(title = "This is another graph to go on the second slide",
#'    subtitle = "Units go here",
#'    caption = "Source: mtcars.")
#'
#'# Now combine your two graphs together in a list:
#'
#'graphs <- list(graph1, graph2)
#'
#' # Now, create a Powerpoint presentation with one
#' # slide per graph in your list:
#'
#' \dontrun{make_presentation(graphs, filename = "test.pptx")}
#'
#' @export


make_presentation <- function(graphs,
                              filename = NULL,
                              path = ".",
                              type = "16:9",
                              title = "Title",
                              subtitle = "Subtitle") {

  pandoc_test()

  .Deprecated("make_slide()",
              msg = "make_presentation() is deprecated and will be removed. make_slide() can save a list of graphs as a multi-slide Powerpoint deck.")

  if (is.null(filename)) {
    stop("You must specify a filename (such as 'my_presentation') for",
         " the Powerpoint presentation you wish to create.")
  }

  if (is.null(path)) {
    stop("You must specify a path to the directory where you",
         " want your presentation to be saved.")
  }

  if (is.null(type) | !type %in% c("16:9", "4:3")) {
    stop("You must specify what type of presentation you want -",
         " either '16:9' (the default) or '4:3'")
  }

  if ("gg" %in% class(graphs)) {
    graphs <- list(graphs)
  }

  filename <- tools::file_path_sans_ext(filename)

  output_file <- paste0(filename, ".pptx")
  output_dir <- path

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # copy template to temporary directory


  if (type == "16:9") {
    template_source <- system.file(file.path("extdata", "template_169.pptx"),
                                   package = "grattantheme")
  } else {
    template_source <- system.file(file.path("extdata", "template_43.pptx"),
                                   package = "grattantheme")
  }


  temp_dir <- file.path(tempdir(), "make_slide")

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }

  on.exit(unlink(temp_dir, recursive = TRUE))

  temp_template <- file.path(temp_dir, basename(template_source))

  result_of_copy <- file.copy(from = template_source,
                              to = temp_template,
                              overwrite = TRUE)

  if (!isTRUE(result_of_copy)) {
    stop("make_presentation() encountered a problem copying the Powerpoint",
         " template to a temporary directory.")
  }

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  ref_doc <- paste0('    reference_doc: "',
                    #temp_template,
                    basename(template_source),
                    '"')

  # create title slide

  if (!is.null(title) | !is.null(subtitle)) {

    null_to_blank <- function(x) {
      x <- ifelse(is.null(x), "", x)
      x
    }

    title <- paste0("title: \"", null_to_blank(title), "\"")
    subtitle <- paste0("author: \"", null_to_blank(subtitle), "\"")

  } else {
    title <- ""
    subtitle <- ""
  }


  yaml_header <- paste("---",
                       "output:",
                       "  powerpoint_presentation: ",
                       ref_doc,
                       title,
                       subtitle,
                       "---",
                       "\n",
                       sep = "\n")

  knitr_setup <- paste0(backticks,
                        "{r setup-makepresentation-chunk, include=FALSE, message=FALSE}\n",
                        "knitr::opts_chunk$set(echo = FALSE,\nfig.height = 5.63,\nfig.width = 11.8,\nfig.retina = 2)\n",
                        backticks,
                        "\n")


  plot_areas <- list()
  plot_filenames <- list()
  for (i in seq_along(graphs)) {

      p <- graphs[[i]]

      slide_rmd <- generate_slide_rmd(p = p,
                                      type = type,
                                      temp_dir = temp_dir)

      plot_area <- slide_rmd$plot_area
      plot_filename <- slide_rmd$plot_filename
      on.exit(unlink(plot_filename))

      plot_filenames[[i]] <- plot_filename
      plot_areas[[i]] <- plot_area
  }

  plot_areas <- paste(plot_areas, collapse = "\n")

  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_areas,
                   sep = "\n")

  writeLines(fulldoc, file.path(temp_dir, "temp_rmd.Rmd"))

  rmarkdown::render(file.path(temp_dir, "temp_rmd.Rmd"),
                    output_file = output_file,
                    output_dir = output_dir,
                    quiet = TRUE)

  remove_pptx_error(file.path(output_dir, output_file))

  result_of_file_remove <- file.remove(file.path(temp_dir, "temp_rmd.Rmd"))

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

  p <- wrap_labs(p,
                 type = ifelse(type == "16:9",
                               "normal_169",
                               "normal"))

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
         width = ifelse(type == "16:9",
                        30, 22.2),
         dpi = "retina",
         units = "cm")

  if (isTRUE(requireNamespace("rstudioapi")) &&
      isTRUE(rstudioapi::isAvailable())) {
    script_location <- rstudioapi::getActiveDocumentContext()$path
  } else {
    script_location <- ""
  }


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
                     paste0("R script location: ",
                            script_location),
                     ":::",
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
