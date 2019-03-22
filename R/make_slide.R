#' Create a Powerpoint slide that contains a Grattan graph and editable title and subtitle
#'
#' @param graph The graph you want to include on your slide. Defaults to the last plot (ggplot2::last_plot())
#' @param filename The filename for your Powerpoint slide.
#' @param path Path to the directory you wish to save your slide in. Defaults to your working directory.
#' @param type The type of Powerpoint slide to create; either "16:9" (the default, for a 16:9 orientation slide), or "4:3".
#' @importFrom rmarkdown render
#' @importFrom tools file_path_sans_ext
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

  filename <- tools::file_path_sans_ext("test.pptx")

  output_file <- paste0(filename, ".pptx")
  output_dir <- dirname(path)

  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  # copy template to temporary directory

  if(type == "16:9"){
    template_source <- system.file("extdata/template_169.pptx", package = "grattantheme")
  } else {
    template_source <- system.file("extdata/template_43.pptx", package = "grattantheme")
  }


  temp_dir <- paste0(tempdir(), "/make_slide/")

  if(!dir.exists(temp_dir)){
    dir.create(temp_dir)
  }

  temp_template <- paste0(temp_dir, basename(template_source))

  file.copy(from = template_source,
            to = temp_template,
            overwrite = TRUE)

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  ref_doc <- paste0('    reference_doc: "',
                    temp_template,
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

  plot_filename <- paste0(temp_dir, "plot.png")

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
                            plot_filename,
                            ")"),
                     ":::",
                     "::::::::::::::",
                     sep = "\n")


  fulldoc <- paste(yaml_header,
                   knitr_setup,
                   plot_area,
                   sep = "\n")

  writeLines(fulldoc, paste0(temp_dir,"/temp_rmd.Rmd"))

  rmarkdown::render(paste0(temp_dir, "/temp_rmd.Rmd"),
                    output_file = output_file,
                    output_dir = output_dir,
                    quiet = TRUE)

  file.remove(paste0(temp_dir, "/temp_rmd.Rmd"))



}

