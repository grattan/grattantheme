#' @import knitr
#' @import rmarkdown
#'

stitch_pdfs <- function(pdfs, output_file = NULL, output_dir = getwd()){

  if(is.null(output_file)){
    stop("Must specify `output_file`")
  }

  backticks <- paste0((rep("\x60", 3)), collapse = "")

  yaml_header <- paste("---",
                       "output: pdf_document",
                       "geometry: a4paper, margin=0cm",
                       "classoption:",
                       "  - landscape",
                       "---",
                       "\n",
                       sep = "\n")

  knitr_setup <- paste0(backticks,
                        "{r setup, include=FALSE}\n",
                        "knitr::opts_chunk$set(dpi=300,  fig.width=8.7244094, fig.height=5.708661, out.height = \"\\\\textheight\",  out.width = \"\\\\textwidth\")\n",
                        backticks,
                        "\n")

  image_code <- paste0(backticks,
                       "{r, echo = FALSE}\n",
                       "knitr::include_graphics(\"",
                       pdfs,
                       "\")\n",
                       backticks,
                       "\n",
                       collapse = "\n")



  fulldoc <- paste(yaml_header, knitr_setup, image_code, sep = "\n")

  writeLines(fulldoc, "temp_rmd.Rmd")

  rmarkdown::render("temp_rmd.Rmd", output_file = output_file,
                    output_dir = output_dir)

  file.remove("temp_rmd.Rmd")
}
