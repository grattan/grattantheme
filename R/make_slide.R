
#' @importFrom rlang quo as_name
#' @importFrom rmarkdown render
#' @export


make_slide <- function(graph = last_plot(), path = "."){

  if(!"gg" %in% class(graph)){
    stop("The object is not a ggplot2 graph and cannot be plotted with make_slide()")
  }

  p <- graph

  #p <- wrap_labs(p, type = "normal_169")

  graph_name <- rlang::quo(p) %>% rlang::as_name()

  output_file <- paste0(graph_name, ".pptx")
  output_dir <- dirname(path)

  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  # copy template to temporary directory

  template_source <- system.file("extdata/template_169.pptx", package = "grattantheme")

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

  p <- wrap_labs(p, type = "fullslide_169")

  plot_filename <- paste0(temp_dir, "plot.png")

  grattan_save(filename = plot_filename,
               object = p, type = "normal_169", force_labs = TRUE)


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

