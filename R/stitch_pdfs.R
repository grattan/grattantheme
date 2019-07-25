#' Combine multiple PDFs into a single PDF
#'
#' @param pdfs character vector containing filenames (with path, if the files
#' aren't in the working directory) of the PDFs to be combined
#'
#' @param output name of the PDF file (including path where necessary)
#' to be created, containing the stiched-together PDFs
#'
#' @import knitr
#' @import rmarkdown
#'
#' @name stitch_pdfs
#' @examples
#' # Deprecated function, will be removed in future versions.
#' # stitch_pdfs() is useful when you have created multiple charts using
#' # grattan_save() and wish to combine them into one document
#'
#' # First, create some charts and save them as individual PDFs:
#' library(ggplot2)
#'
#' chart1 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' chart2 <- ggplot(mtcars, aes(x = carb, y = mpg, group = carb)) +
#'     geom_boxplot() +
#'     theme_grattan()
#'
#' grattan_save("chart1.pdf", chart1)
#' grattan_save("chart2.pdf", chart2)
#'
#' # Now combine the two saved charts into one pdf
#' stitch_pdfs(c("chart1.pdf", "chart2.pdf"), output = "both_charts.pdf")
#'
#' @export


stitch_pdfs <- function(pdfs, output = NULL){

  .Deprecated(new = "pdf_combine()",
              package = "pdftools",
              msg = "stitch_pdfs() is deprecated and will be removed in a future version. Consider using pdftools::pdf_combine().")

  if(is.null(output)){
    stop("Must specify `output` - the filename of the PDF to be created")
  }

  output_file <- basename(output)
  output_dir <- dirname(output)

  if(!dir.exists(output_dir)){
    dir.create(output_dir)
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
                    output_dir = output_dir,
                    quiet = TRUE)

  file.remove("temp_rmd.Rmd")
}
