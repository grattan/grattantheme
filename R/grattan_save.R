#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#' @name grattan_save
#' @param filename Required. This parameter sets the filename (including path where necessary)
#' where you want to save your image. The extension defines the file type. Suggested filetypes
#' are "pdf" or "png", but others are available (see \code{?ggsave} for the full list).
#' @param object The ggplot2 graph object to be saved. Defaults to \code{last_plot()}, which
#' will save the last plot that was displayed in your session.
#' @param type Sets height and width to Grattan defaults. The following chart types are available:
#'
#' \itemize{
##'  \item{"normal"}{ The default. Use for normal Grattan report charts, or to paste into a 4:3 Powerpoint slide.
##'  Width: 22.2cm, height: 14.5cm.}
##'  \item{"normal_169"}{ Only useful for pasting into a 16:9 format Grattan Powerpoint slide. Width: 30cm, height: 14.5cm.}
##'  \item{"tiny"}{ Fills the width of a column in a Grattan report, but is shorter than usual. Width: 22.2cm,
##'  height: 11.1cm.}
##'  \item{"wholecolumn"}{ Takes up a whole column in a Grattan report. Width: 22.2cm, height: 22.2cm.}
##'  \item{"fullpage"}{ Fills a whole page of a Grattan report. Width: 44.3cm, height: 22.2cm.}
##'  \item{"fullslide}{ Creates an image that looks like a 4:3 Grattan Powerpoint slide, complete with logo. Use this for the
##'  Grattan Blog. Width: 25.4cm, height: 19.0cm.}
##'  \item{"fullslide_169}{ Creates an image that looks like a 16:9 Grattan Powerpoint slide, complete with logo.
##'  Use this to drop into standard presentations. Width: 33.9cm, height: 19.0cm}
##'  \item{"fullslide_44"}{ Creates an image that looks like a 4:4 Grattan Powerpoint slide. This may be useful for taller charts
##'  for the Grattan blog; not useful for any other purpose. Width: 25.4cm, height: 25.4cm.}
##' }
#'
#' Set type = "all" to save your chart in all available sizes.
#' @param save_data Logical. Default is FALSE, unless type = "all". If set to TRUE, a .csv file will be created containing the dataframe you passed to ggplot(). The filename and path will be the same as your image, but with a .csv extension. Data will always be saved if type = "all".
#' @param force_labs Logical. By default, `grattan_save()` will remove your title, subtitle, and caption (if present) from your graph before saving it, unless `type` = "fullslide". By setting `force_labs` to TRUE, your title/subtitle/caption will be retained regardless of `type`.
#'
#' @import ggplot2
#' @import grid
#' @importFrom utils tail
#' @importFrom utils write.csv
#' @importFrom gridExtra grid.arrange
#' @importFrom purrr walk2
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' grattan_save("p.png", p)
#'
#' # If you don't assign your chart to an object name, that's OK, it will still
#' # save if it was the last plot displayed.
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' grattan_save("p.png")
#'
#'
#' # Want to make a 'self-contained' chart that includes a title/subtitle/caption,
#' # eg. to go on the Grattan Blog? If so, just add them - they'll be properly
#' # left-aligned when you save them with grattan_save(), like this:
#'
#'  ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan() +
#'     labs(title = "Title goes here",
#'          subtitle = "Subtitle goes here",
#'          caption = "Notes: Notes go here\nSource: Source goes here")
#'
#'  # The plot above won't look right in RStudio's viewer - the text is
#'  # aligned to the left of the plot area, not the image. Once you save it,
#'  # the file should have properly-aligned text:
#'
#'  grattan_save("your_file.png")
#'
#'  # Want to make a full Powerpoint slide? Just use type = "fullslide" in grattan_save(), like this.
#'  # If you include 'notes' and 'source' as below, grattan_save() will automatically
#'  # split them onto separate rows. It will also wrap your title and subtitle
#'  # automatically over up to 2 rows; and wrap your caption over as many rows
#'  # as necessary.
#'
#'  ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     scale_y_continuous_grattan(limits = c(10, NA)) +
#'     scale_colour_manual(values = grattan_pal(n = 3)) +
#'     theme_grattan() +
#'     labs(title = "Title goes here",
#'          subtitle = "Subtitle goes here",
#'          caption = "Notes: Notes go here. Source: Source goes here")
#'
#'  grattan_save("your_file.png", type = "normal")
#'
#'
#' @export

# requireNamespace(c("grid",
#                    "gridExtra",
#                    "ggplot2"),
#                    quietly = TRUE)

grattan_save <- function(filename,
                         object = ggplot2::last_plot(),
                         type = "normal",
                         save_data = FALSE,
                         force_labs = FALSE) {

  if(!type %in% c("all", chart_types$type)){
    stop(paste0("`type` not valid"))
  }

  if(type != "all"){

    if(save_data == TRUE){
      if("gg" %in% class(object)){
        utils::write.csv(x = object$data,
                         file = paste0(sub("\\..*", "", filename), ".csv"))
      } else {
        warning("save_data only works with ggplot graph objects. Your data has not been saved.")
      }
    }

    grattan_save_(filename = filename, object = object,
                  type = type, force_labs = force_labs)
  }

  if(type == "all"){
    dir <- tools::file_path_sans_ext(filename)
    filetype <- tools::file_ext(filename)
    file_name <- tools::file_path_sans_ext(basename(filename))

    if(!dir.exists(dir)){
      dir.create(dir, recursive = TRUE)
    }

    types <- chart_types$type

    filenames <- file.path(dir, paste0(file_name, "_", types, ".", filetype))

      if("gg" %in% class(object)){
        utils::write.csv(x = object$data,
                         file = file.path(dir,
                                       paste0(file_name,".csv")))
      } else {
        warning("save_data only works with ggplot graph objects. Your data has not been saved.")
      }


    purrr::walk2(.x = filenames,
                 .y = types,
                 .f = grattan_save_,
                 object = object,
                 force_labs = force_labs)

  }


}



#### grattan_save_() is an internal function that does the actual work of saving
#### individual plots; it is called by grattan_save()
grattan_save_ <- function(filename,
                          object = ggplot2::last_plot(),
                          type = "normal",
                          force_labs = FALSE){

  # at the moment, save_data is inflexible: only saves as .csv and
  # with the same filename (except extension) as the plot.
  # It saves the whole dataframe passed to ggplot(), not limited to the
  # column(s)/row(s) used in the plot.


  # create an image the size of a 4:3 Powerpoint slide complete with Grattan logo
  if(grepl("fullslide", type)){

    # calls another function to do the work of assembling a full slide
    object <- create_fullslide(object = object, type = type)

  } else { # following code only applies if type != "fullslide"

    if(!force_labs){
      # Unless force_labs == TRUE (indicating the user wishes to retain their labels)
      # Remove title, subtitle and caption for type != "fullslide"
      # Politely give warning before removal

      if("title"    %in% names(object$labels) |
         "subtitle" %in% names(object$labels) |
         "caption"  %in% names(object$labels)){
        message(paste0("Note: ", type, " charts remove titles, subtitles, or captions by default.\nSet `force_labs` to TRUE to retain them, or use type = 'fullslide'"))


        object <- object +
          theme(plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.caption = element_blank())
      }
    } else {
    # non-fullslide, force_labs = TRUE
        object <- wrap_labs(object, type)

        object <- ggplotGrob(object)

        object$layout$l[object$layout$name %in% c("title", "subtitle", "caption")] <- 1

        #grid::grid.draw(object)
    }

  } # end of section that only apples to type != "fullslide

  width <- chart_types$width[chart_types$type == type]
  height <- chart_types$height[chart_types$type == type]

  ggplot2::ggsave(filename, object,
                  width = width, height = height, units = "cm", dpi = "retina")

}

