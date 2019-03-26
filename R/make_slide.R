#' Create a Powerpoint slide that contains a Grattan graph and editable title and subtitle
#'
#' @param graph The graph you want to include on your slide. Defaults to the last plot (ggplot2::last_plot())
#' @param filename The filename for your Powerpoint slide.
#' @param path Path to the directory you wish to save your slide in. Defaults to your working directory.
#' @param type The type of Powerpoint slide to create; either "16:9" (the default, for a 16:9 orientation slide), or "4:3".
#' @importFrom rmarkdown render
#' @importFrom knitr opts_chunk
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#'
#' # First, create your ggplot2 object. Include your subtitle, title, and caption with +labs()
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
#' # By default, make_slide() will use the last plot you generated; you can specify a
#' # different object to use with the `graph` argument.
#'
#' make_slide(filename = "test.pptx")
#'
#' @export


make_slide <- function(graph = last_plot(),
                       filename = NULL,
                       path = ".",
                       type = "16:9"){

  if(!"gg" %in% class(graph)){
    stop("The object is not a ggplot2 graph and cannot be plotted with make_slide()")
  }

  if(is.null(filename)){
    stop("You must specify a filename (such as 'my_slide') for the Powerpoint slide you wish to create.")
  }

  if(is.null(path)){
    stop("You must specify a path to the directory where you want your slide to be saved.")
  }

  if(is.null(type) | !type %in% c("16:9", "4:3")){
    stop("You must specify what type of slide you want - either '16:9' (the default) or '4:3'")
  }

  p <- graph

  filename <- tools::file_path_sans_ext(filename)

  output_file <- paste0(filename, ".pptx")
  output_dir <- dirname(path)

  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  # copy template to temporary directory


  if(type == "16:9"){
    template_source <- system.file(file.path("extdata", "template_169.pptx"),
                                   package = "grattantheme")
  } else {
    template_source <- system.file(file.path("extdata", "template_43.pptx"),
                                   package = "grattantheme")
  }


  temp_dir <- file.path(tempdir(), "make_slide")
  #temp_dir <- paste0(tempdir(), "/make_slide/")

  if(!dir.exists(temp_dir)){
    dir.create(temp_dir)
  }

  temp_template <- file.path(temp_dir, basename(template_source))

  result_of_copy <- file.copy(from = template_source,
            to = temp_template,
            overwrite = TRUE)

  if(!result_of_copy){
    stop("make_slide() encountered a problem copying the Powerpoint template to a temporary directory.")
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

  knitr_setup <- paste0(backticks,
                        "{r setup, include=FALSE, message=FALSE}\n",
                        "knitr::opts_chunk$set(echo = FALSE,\nfig.height = 5.63,\nfig.width = 11.8,\nfig.retina = 2)\n",
                        backticks,
                        "\n")

  graph_title <- paste0("## ", p$labels$title)
  graph_subtitle <- p$labels$subtitle

  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  p <- wrap_labs(p,
                 type = ifelse(type == "16:9",
                               "normal_169",
                               "normal"))

  plot_filename <- file.path(temp_dir, "plot.png")

  grattan_save(filename = plot_filename,
               object = p,
               type = ifelse(type == "16:9",
                             "normal_169",
                             "normal"),
               force_labs = TRUE)


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

  writeLines(fulldoc, file.path(temp_dir,"temp_rmd.Rmd"))

  rmarkdown::render(file.path(temp_dir, "temp_rmd.Rmd"),
                    output_file = output_file,
                    output_dir = output_dir,
                    quiet = TRUE)

  result_of_file_remove <- file.remove(file.path(temp_dir, "temp_rmd.Rmd"))

}

