#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#' @name grattan_save
#' @param filename Required. The filename (including path where necessary) for your image on disk. The extension defines the file type.
#' @param object Defaults to \code{last_plot()}. Can specify a different ggplot2 object to be saved.
#' @param height Default is 14.5cm, Grattan normal size default. See \code{type}.
#' @param width Default is 22.16cm, Grattan normal size default. See \code{type}.
#' @param type Sets height and width to Grattan defaults for one of c("normal", "tiny", "wholecolumn", "fullpage"). 'normal' is the default and uses default height and width. 'tiny' uses height of 11.08cm and default width. 'wholecolumn' uses height of 22.16cm and default width. 'fullpage' uses height 22.16cm and width of 44.32cm.
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
#' @export

requireNamespace("ggplot2", quietly = TRUE)

grattan_save <- function(filename, object = last_plot(), height = 14.5, width = 22.16, type = "normal") {

  if (type == "tiny")    height = 11.08
  if (type == "wholecolumn") height = 22.16
  if (type == "fullpage")  {
    height = 22.16
    width  = 44.32
  }

  # Modify the graph object so that title/subtitle/caption are properly
  # left-aligned (to the left of the whole image, not just the plot area)
  g <- ggplotGrob(object)

  g$layout$l[g$layout$name == "title"] <- 1
  g$layout$l[g$layout$name == "subtitle"] <- 1
  g$layout$l[g$layout$name == "caption"] <- 1

  ggsave(filename, g,
         width = width, height = height, units = "cm", dpi = "retina")
}

