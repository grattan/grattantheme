#' Save ggplot2 object as an image in the correct size and resolution for Grattan charts. Wrapper around ggsave().
#' @name grattan_save
#' @param filename Required. This parameter sets the filename (including path where necessary) where you want to save your image. The extension defines the file type. Suggested filetypes are "pdf" or "png", but others are available (see \code{?ggsave} for the full list).
#' @param object Defaults to \code{last_plot()}. Can specify a different ggplot2 object to be saved.
#' @param height Default is 14.5cm, Grattan normal size default. See \code{type}.
#' @param width Default is 22.16cm, Grattan normal size default. See \code{type}.
#' @param type Sets height and width to Grattan defaults for one of c("normal", "tiny", "wholecolumn", "fullpage", "fullslide"). 'normal' is the default and uses default height and width. 'tiny' uses height of 11.08cm and default width. 'wholecolumn' uses height of 22.16cm and default width. 'fullpage' uses height 22.16cm and width of 44.32cm. 'fullslide' saves an image that can be used as a complete 4:3 Powerpoint slide, complete with Grattan logo.
#' @param save_data Default is FALSE. If set to TRUE, a .csv file will be created containing the dataframe you passed to ggplot(). The filename and path will be the same as your image, but with a .csv extension.
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
#'  # Want to make a full Powerpoint slide? Just use type = "fullslide" in grattan_save(), like:
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
#'  grattan_save("your_file.png", type = "fullslide")
#'
#'
#' @export

requireNamespace(c("grid",
                   "gridExtra",
                   "ggplot2"),
                   quietly = TRUE)

grattan_save <- function(filename,
                         object = last_plot(),
                         height = 14.5,
                         width = 22.16,
                         type = "normal",
                         save_data = FALSE) {

  # at the moment, save_data is inflexible: only saves as .csv and
  # with the same filename (except extension) as the plot.
  # It saves the whole dataframe passed to ggplot(), not limited to the
  # column(s)/row(s) used in the plot.

  if(save_data == TRUE){
     write.csv(x = object$data,
               file = paste0(sub("\\..*", "", filename), ".csv"))
    }


  # create an image the size of a 4:3 Powerpoint slide complete with Grattan logo
  if(type == "fullslide"){

    p <- object
    char_width_grattan_title <- 50
    char_width_grattan_subtitle <- 70

    # extract title and subtitle, created as usual in the plotting process
    stored_title <- p$labels$title
    stored_subtitle <- p$labels$subtitle

    # add line break to title where necessary
    if(is.null(stored_title)){
      warning("Your plot has no title, which is weird for type='fullslide'.\nAdd a title using +labs(title = 'Title')")
      stored_title <- ""
    }

    if(nchar(stored_title) <= char_width_grattan_title){
      stored_title <- paste0("\n", stored_title)

    } else {

      if(nchar(stored_title) > 2 * char_width_grattan_title) {
        stop('Your chart title is too long for a Grattan Powerpoint slide. Please reduce the length of the title.')
      } else {

        stored_title <- paste0(strwrap(stored_title, char_width_grattan_title)[1],
                               "\n",
                               strwrap(stored_title, char_width_grattan_title)[2])
      }
    }

    # add line break to subtitle where necessary
    if(is.null(stored_subtitle)){
      warning("Your plot has not subtitle, which is weird for type='fullslide'.\nAdd a title using labs(subtitle = 'Text')")
      stored_subtitle <- ""
    }
    if(nchar(stored_subtitle) <= char_width_grattan_subtitle){
      stored_subtitle <- paste0(stored_subtitle, "\n")

    } else {

      if(nchar(stored_subtitle) > 2 * char_width_grattan_subtitle) {
        stop('Your chart subtitle is too long for a Grattan Powerpoint slide. Please reduce the length of the subtitle')
      } else {

        stored_subtitle <- paste0(strwrap(stored_subtitle, char_width_grattan_subtitle)[1],
                                  "\n",
                                  strwrap(stored_subtitle, char_width_grattan_subtitle)[2])
      }

    }

    # remove title and subtitle on chart
    p$labels$title <- NULL
    p$labels$subtitle <- NULL

    # left align caption
    p <- ggplotGrob(p)
    p$layout$l[p$layout$name == "caption"] <- 1

    # create new ggplot object with just the title
    toptitle <- ggplot() +
      geom_blank() +
      labs(title = stored_title) +
      theme_grattan() +
      theme(rect = element_blank(),
            plot.title = element_text(colour = "black", hjust = 0, vjust = 0),
            plot.margin = unit(c(0, 0, 0, 0), units = "cm"))

    # create new ggplot object with just the subtitle
    topsubtitle <- ggplot() +
      geom_blank() +
      labs(subtitle = stored_subtitle) +
      theme_grattan() +
      theme(rect = element_blank(),
            plot.subtitle = element_text(colour = "black", hjust = 0, vjust = 0),
            plot.margin = unit(c(0, 0, 0, 0), units = "cm"))

    # create new grob with the logo
    #logogrob <- grid::rasterGrob(png::readPNG(source = "atlas/logo.png"))

    # create new grob of whitespace to be the border
    border <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "white"))

    # create new grob of solid orange to be the horizontal line
    linegrob <- grid::rectGrob(gp = grid::gpar(fill = "#F68B33", col = "white"))

    # create header (= title + logo side by side)

    header <- gridExtra::grid.arrange(toptitle, logogrob,
                           ncol = 2,
                           widths = unit(c(17.73, 4.57), "cm"),
                           heights = unit(1.48, "cm"))

    # create main plotting area
    mainarea <- gridExtra::grid.arrange(border, header, linegrob, topsubtitle, p, border,
                             ncol = 1,
                             heights = unit(c(0.92, 1.48, 0.1, 1.48, 14.61, 0.46), "cm"),
                             widths = unit(17.73 + 4.57, "cm"))

    # create total plot

    total <- gridExtra::grid.arrange(border, mainarea, border, ncol = 3,
                          widths = unit(c((25.4 - (17.73 + 4.57))/2,
                                          17.73 + 4.57,
                                          (25.4 - (17.73 + 4.57))/2),
                                        "cm"))

    # plot original chart again (so last_plot() shows this instead of topsubtitle)
    print(object)

    # save full image incl. logo etc.
    ggsave(filename, plot = total, width = 25.4, height = 19.05, units = "cm", dpi = "retina")

  } else { # following code only applies if type != "fullslide"

    if (type == "tiny")    height = 11.08
    if (type == "wholecolumn") height = 22.16
    if (type == "fullpage")  {
      height = 22.16
      width  = 44.32
    }

  # Modify the graph object so that title/subtitle/caption are properly
  # left-aligned (to the left of the whole image, not just the plot area)
  # Only applies to non-fullslide chart types

  if("gg" %in% class(object)) { #ie. only apply the following to plots, not grob objects
  g <- ggplotGrob(object)
  } else {
      g <- object
    }

  g$layout$l[g$layout$name == "title"] <- 1
  g$layout$l[g$layout$name == "subtitle"] <- 1
  g$layout$l[g$layout$name == "caption"] <- 1


  ggsave(filename, g,
         width = width, height = height, units = "cm", dpi = "retina")

        }
}

