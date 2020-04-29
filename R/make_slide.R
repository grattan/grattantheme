#' Create a Powerpoint slide(s) that contains a Grattan graph and editable title
#' and subtitle.
#'
#' @description Use `make_slide()` to create a Powerpoint presentation
#'   containing a single slide, or `make_presentation()` for a presentation with
#'   multiple slides.
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
  output_dir <- dirname(path)

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

  graph_title <- paste0("## ", p$labels$title)
  graph_subtitle <- p$labels$subtitle

  p <- wrap_labs(p,
                 type = ifelse(type == "16:9",
                               "normal_169",
                               "normal"))

  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  plot_filename <- file.path(temp_dir, "plot.png")

  ggsave(filename = plot_filename,
         plot = p,
         height = 14.5,
         width = ifelse(type == "16:9",
                             30, 22.2),
         units = "cm")


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
                     sep = "\n")


  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_area,
                   sep = "\n")

  writeLines(fulldoc, file.path(temp_dir, "temp_rmd.Rmd"))

  rmarkdown::render(file.path(temp_dir, "temp_rmd.Rmd"),
                    output_file = output_file,
                    output_dir = output_dir,
                    quiet = TRUE)

  result_of_file_remove <- file.remove(file.path(temp_dir, "temp_rmd.Rmd"))

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
  output_dir <- file.path(path)

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

  temp_template <- file.path(temp_dir, basename(template_source))

  result_of_copy <- file.copy(from = template_source,
                              to = temp_template,
                              overwrite = TRUE)

  if (!result_of_copy) {
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
  for (i in seq_along(graphs)) {

      p <- graphs[[i]]

      graph_title <- paste0("## ", p$labels$title)
      graph_subtitle <- p$labels$subtitle


      p <- wrap_labs(p,
                     type = ifelse(type == "16:9",
                                   "normal_169",
                                   "normal"))

      p$labels$title <- NULL
      p$labels$subtitle <- NULL

      plot_filename <- file.path(temp_dir, paste0("plot", i, ".png"))

      ggsave(filename = plot_filename,
             plot = p,
             height = 14.5,
             width = ifelse(type == "16:9",
                            30, 22.2),
             units = "cm")


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
                         "::: notes",
                         paste0("Title: ", gsub("## ", "", graph_title), "\n"),
                         paste0("Subtitle: ", graph_subtitle, "\n"),
                         p$labels$caption,
                         ":::",
                         "::::::::::::::",
                         sep = "\n")

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

  remove_pptx_error(output_file)

  result_of_file_remove <- file.remove(file.path(temp_dir, "temp_rmd.Rmd"))

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

}
