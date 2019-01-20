# Assemble an image resembling a full PPT slide containing a Grattan chart
# Not intended to be called directly, this function is called from grattan_save()

#' @import ggplot2
#' @import gridExtra

save_fullslide <- function(object){

  if(!"gg" %in% class(object)){
    stop("type = 'fullslide' only works with ggplot graph objects")
  }

  p <- object

  # widths in characters
  char_width_grattan_title <- 50
  char_width_grattan_subtitle <- 70
  char_width_grattan_caption <- 140

  # extract title and subtitle, created as usual in the plotting process
  stored_title <- p$labels$title
  stored_subtitle <- p$labels$subtitle
  stored_caption <- p$labels$caption

  # add line break to title where necessary
  if(is.null(stored_title)){
    message("Your plot has no title, which is weird for type='fullslide'.\nAdd a title using +labs(title = 'Title')")
    stored_title <- ""
  }

  if(nchar(stored_title) <= char_width_grattan_title){
    stored_title <- paste0("\n", stored_title)

  } else {

    if(nchar(stored_title) > 2 * char_width_grattan_title) {
      # if title > 2 lines, return an informative error that tells users
      # where they need to trim their title to

      # code to figure out the final 2 chunks of text before the title limit
      trimmed_title <- strtrim(stored_title, 2* char_width_grattan_title)
      trimmed_title_final_words <- paste0(utils::tail(strsplit(trimmed_title,split=" ")[[1]],2), collapse = " ")

      # return an error and tell the user where the useable string ends
      stop(paste0('Your chart title is too long for a Grattan Powerpoint slide. Please reduce the length of the title.\nEverything after "', trimmed_title_final_words, '" cannot fit onto the slide.'))
    } else {

      stored_title <- paste0(strwrap(stored_title, char_width_grattan_title)[1],
                             "\n",
                             strwrap(stored_title, char_width_grattan_title)[2])
    }
  }

  # add line break to subtitle where necessary
  if(is.null(stored_subtitle)){
    message("Your plot has no subtitle, which is weird for type='fullslide'.\nConsider adding a subtitle using labs(subtitle = 'Text')")
    stored_subtitle <- ""
  }
  if(nchar(stored_subtitle) <= char_width_grattan_subtitle){
    stored_subtitle <- paste0(stored_subtitle, "\n")

  } else {

    if(nchar(stored_subtitle) > 2 * char_width_grattan_subtitle) {
      # code to figure out the final 2 chunks of text before the title limit
      trimmed_subtitle <- strtrim(stored_subtitle, 2* char_width_grattan_subtitle)
      trimmed_subtitle_final_words <- paste0(utils::tail(strsplit(trimmed_subtitle,split=" ")[[1]],2), collapse = " ")
      # return an error and tell the user where the useable string ends
      stop(paste0('Your chart subtitle is too long for a Grattan Powerpoint slide. Please reduce subtitle length.\nEverything after "', trimmed_subtitle_final_words, '" cannot fit onto the slide.'))


    } else {

      stored_subtitle <- paste0(strwrap(stored_subtitle, char_width_grattan_subtitle)[1],
                                "\n",
                                strwrap(stored_subtitle, char_width_grattan_subtitle)[2])
    }

  }

  # add line break to caption where necessary
  if(is.null(stored_caption)){
    message("Your plot has no caption, which is weird for full slide charts.\nConsider adding a caption using labs(caption = 'Text')")
    stored_caption <- ""
  }

  contains_notes_and_source <- grepl("notes?:", tolower(stored_caption)) & grepl("sources?:", tolower(stored_caption))

  # if the caption doesn't contain "notes" and "source", we want to wrap the whole
  # caption string across lines; if notes and source are present we want to wrap them separately
  if(!contains_notes_and_source){
    caption_lines <- ceiling(nchar(stored_caption) / char_width_grattan_caption)

    if(caption_lines > 1){
      stored_caption <- paste0(strwrap(stored_caption, char_width_grattan_caption), collapse = "\n")
    }
  } else { # now deal with the case when "notes" and "source" are present
    notes_and_source <- strsplit(stored_caption, split = "Source:")
    notes <- notes_and_source[[1]][1]
    source <- paste0("Source:", notes_and_source[[1]][2])

    notes <- paste0(strwrap(notes, char_width_grattan_caption),
                    collapse = "\n")

    source <- paste0(strwrap(source, char_width_grattan_caption),
                     collapse = "\n")

    stored_caption <- paste0(notes, "\n", source)
  }


  # remove title and subtitle on chart
  p$labels$title <- NULL
  p$labels$subtitle <- NULL

  # replace caption with version split over several lines (where necessary)

  p$labels$caption <- stored_caption

  # left align caption
  p <- ggplot2::ggplotGrob(p)
  p$layout$l[p$layout$name == "caption"] <- 1

  # create new ggplot object with just the title
  toptitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(title = stored_title) +
    theme_grattan() +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(colour = "black", hjust = 0, vjust = 0),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"))

  # create new ggplot object with just the subtitle
  topsubtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::labs(subtitle = stored_subtitle) +
    theme_grattan() +
    ggplot2::theme(rect = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(colour = "black", hjust = 0, vjust = -2),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), units = "cm"))

  # create new grob with the logo
  #logogrob <- grid::rasterGrob(png::readPNG(source = "atlas/logo.png"))

  # create new grob of whitespace to be the border
  border <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "white"))

  # create new grob of solid orange to be the horizontal line
  linegrob <- grid::rectGrob(gp = grid::gpar(fill = "#F3901D", col = "white"))

  # create header (= title + logo side by side)

  header <- gridExtra::grid.arrange(toptitle, logogrob,
                                    ncol = 2,
                                    widths = unit(c(17.73, 4.57), "cm"),
                                    heights = unit(1.48, "cm"))

  # create main plotting area
  mainarea <- gridExtra::grid.arrange(border, header, linegrob, topsubtitle, p, border,
                                      ncol = 1,
                                      heights = unit(c(0.92, 1.48, 0.1, 1.7, 14.53, 0.46), "cm"),
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
  # ggplot2::ggsave(filename, plot = total, width = 25.4, height = 19.05, units = "cm", dpi = "retina")

  total

}
