#' Combine multiple PDFs into a single PDF
#'
#' Deprecated function, no longer available.
#' Consider using pdftools::pdf_combine() to combine multiple PDFs.
#'
#' @param pdfs character vector containing filenames (with path, if the files
#' aren't in the working directory) of the PDFs to be combined
#'
#' @param output name of the PDF file (including path where necessary)
#' to be created, containing the stiched-together PDFs
#' @name stitch_pdfs
#'
#' @export


stitch_pdfs <- function(pdfs, output = NULL){

  .Deprecated(new = "pdf_combine()",
              package = "pdftools",
              msg = "stitch_pdfs() is deprecated. Consider using pdftools::pdf_combine().")


}
