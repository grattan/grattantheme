#' Remove 'image could not be displayed' box from Powerpoint title slide
#' @description When creating .pptx presentations with a title slide using
#' using Pandoc / RMarkdown, the image on the title slide has an 'image cannot
#' be displayed' box over the image. This function removes the error box. This
#' function is not intended to be called directly; it is called at the end of
#' `make_presentation()` where a title slide is present.
#'
#' This is a temporary fix based on: https://github.com/jgm/pandoc/issues/4827#issuecomment-463596185
#'
#' @param filename the filename of the .pptx file to be fixed
#' @importFrom xml2 xml_remove write_xml
#' @importFrom utils unzip
#' @importFrom zip zipr

remove_pptx_error <- function(filename){

  ## unzip it
  utils::unzip(filename, exdir = "unzipped-pptx")

  ### remove the <p:pic> section and write back out
  doc <- xml2::read_xml("unzipped-pptx/ppt/slides/slide1.xml")
  xml2::xml_remove(xml2::xml_find_all(doc, "/p:sld/p:cSld/p:spTree/p:pic"))
  xml2::write_xml(doc, "unzipped-pptx/ppt/slides/slide1.xml")

  ## zip it back up
  wd <- getwd()
  setwd("unzipped-pptx")
  suppressMessages(zip::zip(zipfile = file.path(wd, "out.pptx"),
                            files = list.files(recursive = TRUE,
                                               include.dirs = FALSE,
                                               all.files=TRUE)))
  setwd(wd)

  ## clean up
  file.copy("out.pptx", filename, overwrite = TRUE)
  unlink("out.pptx", recursive = TRUE)
  unlink("unzipped-pptx", recursive = TRUE)
}
