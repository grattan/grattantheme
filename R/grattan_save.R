#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#' @name grattan_save
#' @param filename Required. This parameter sets the filename (including path where necessary) where you want to save your image. The extension defines the file type. Suggested filetypes are "pdf" or "png", but others are available (see \code{?ggsave} for the full list).
#' @param object The ggplot2 graph object to be saved. Defaults to \code{last_plot()}, which will save the last plot that was displayed in your session.
#' @param height Default is 14.5cm, Grattan normal size default. See \code{type}.
#' @param width Default is 22.16cm, Grattan normal size default. See \code{type}.
#' @param type Sets height and width to Grattan defaults for one of c("normal", "tiny", "wholecolumn", "fullpage", "fullslide"). 'normal' is the default and uses default height and width. 'tiny' uses height of 11.08cm and default width. 'wholecolumn' uses height of 22.16cm and default width. 'fullpage' uses height 22.16cm and width of 44.32cm. 'fullslide' saves an image that can be used as a complete 4:3 Powerpoint slide, complete with Grattan logo.
#' @param save_data Logical. Default is FALSE. If set to TRUE, a .csv file will be created containing the dataframe you passed to ggplot(). The filename and path will be the same as your image, but with a .csv extension.
#' @param force_labs Logical. By default, `grattan_save()` will remove your title, subtitle, and caption (if present) from your graph before saving it, unless `type` = \"fullslide\". By setting `force_labs` to TRUE, your title/subtitle/caption will be retained regardless of `type`.
#'
#' @import ggplot2
#' @import grid
#' @importFrom utils tail
#' @importFrom utils write.csv
#' @importFrom gridExtra grid.arrange
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
#'  grattan_save("your_file.png", type = "fullslide")
#'
#'
#' @export

# requireNamespace(c("grid",
#                    "gridExtra",
#                    "ggplot2"),
#                    quietly = TRUE)

grattan_save <- function(filename,
                         object = ggplot2::last_plot(),
                         height = 14.5,
                         width = 22.16,
                         type = "normal",
                         save_data = FALSE,
                         force_labs = FALSE) {

  # at the moment, save_data is inflexible: only saves as .csv and
  # with the same filename (except extension) as the plot.
  # It saves the whole dataframe passed to ggplot(), not limited to the
  # column(s)/row(s) used in the plot.

  if(save_data == TRUE){
    if("gg" %in% class(object)){
     utils::write.csv(x = object$data,
               file = paste0(sub("\\..*", "", filename), ".csv"))
    } else {
      warning("save_data only works with ggplot graph objects. Your data has not been saved.")
    }
  }


  # create an image the size of a 4:3 Powerpoint slide complete with Grattan logo
  if(type == "fullslide"){

    # calls another function to do the work of assembling a full slide
    object <- save_fullslide(object = object)

    } else { # following code only applies if type != "fullslide"

    if(!force_labs){
      # Unless force_labs == TRUE (indicating the user wishes to retain their labels)
      # Remove title, subtitle and caption for type != "fullslide"
      # Politely give warning before removal
      if("title"    %in% names(object$labels)) message("Note: This save type removes titles.")
      if("subtitle" %in% names(object$labels)) message("Note: This save type removes subtitles.")
      if("caption"  %in% names(object$labels)) message("Note: This save type removes captions.")

      if(any(c("title", "subtitle", "caption") %in% names(object$labels))) {
        message("Use type = \"fullslide\" to show titles, subtitles and captions.\nAlternatively, set force_labs = TRUE.")

        object <- object +
            theme(plot.title = element_blank(),
                  plot.subtitle = element_blank(),
                  plot.caption = element_blank())
      }
    }
    } # end of section that only apples to type != "fullslide

  if(type == "tiny")    height = 11.08
  if(type == "wholecolumn") height = 22.16
  if(type == "fullpage")  {
        height = 22.16
        width  = 44.32}
  if(type == "fullslide"){
    height = 19.05
    width = 25.4
  }

  ggplot2::ggsave(filename, object,
         width = width, height = height, units = "cm", dpi = "retina")

}

