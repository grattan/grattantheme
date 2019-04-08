

wrap_labs <- function(object, type){

  p <- object

  # widths in characters
  char_width_grattan_title    <- chart_types$title[chart_types$type == type]
  char_width_grattan_subtitle <- chart_types$subtitle[chart_types$type == type]
  char_width_grattan_caption  <- chart_types$caption[chart_types$type == type]

  # extract title and subtitle, created as usual in the plotting process
  stored_title <- p$labels$title
  stored_subtitle <- p$labels$subtitle
  stored_caption <- p$labels$caption

  stored_title <- ifelse(is.null(stored_title), "", stored_title)
  stored_subtitle <- ifelse(is.null(stored_subtitle), "", stored_subtitle)
  stored_caption <- ifelse(is.null(stored_caption), "", stored_caption)

  # add line break to title where necessary
  if(nchar(stored_title) <= char_width_grattan_title &
     type %in% c("fullslide", "fullslide169")){

      stored_title <- paste0("\n", stored_title)
  }

  if(nchar(stored_title) > 2 * char_width_grattan_title) {
      # if title > 2 lines, return an informative error that tells users
      # where they need to trim their title to

      # code to figure out the final 2 chunks of text before the title limit
      trimmed_title <- strtrim(stored_title, 2* char_width_grattan_title)
      trimmed_title_final_words <- paste0(utils::tail(strsplit(trimmed_title,split=" ")[[1]],2), collapse = " ")

      # return an error and tell the user where the useable string ends
      stop(paste0('Your chart title is too long for a Grattan chart. Please reduce the length of the title.\nEverything after "', trimmed_title_final_words, '" cannot fit onto the slide.'))
    }

  if(nchar(stored_title) <= 2 * char_width_grattan_title &
     nchar(stored_title) > char_width_grattan_title) {

      stored_title <- paste0(strwrap(stored_title, char_width_grattan_title)[1],
                             "\n",
                             strwrap(stored_title, char_width_grattan_title)[2])
    }


  # add line break to subtitle where necessary

  if(nchar(stored_subtitle) <= char_width_grattan_subtitle &
     type %in% c("fullslide", "fullslide_169")){
    stored_subtitle <- paste0(stored_subtitle, "\n")

  }

  if(nchar(stored_subtitle) > 2 * char_width_grattan_subtitle) {
      # code to figure out the final 2 chunks of text before the title limit
      trimmed_subtitle <- strtrim(stored_subtitle, 2* char_width_grattan_subtitle)
      trimmed_subtitle_final_words <- paste0(utils::tail(strsplit(trimmed_subtitle,split=" ")[[1]],2), collapse = " ")
      # return an error and tell the user where the useable string ends
      stop(paste0('Your chart subtitle is too long for a Grattan Powerpoint slide. Please reduce subtitle length.\nEverything after "', trimmed_subtitle_final_words, '" cannot fit onto the slide.'))


  }

  if(nchar(stored_subtitle) <= 2 * char_width_grattan_subtitle &
     nchar(stored_subtitle) > char_width_grattan_subtitle) {

        stored_subtitle <- paste0(strwrap(stored_subtitle, char_width_grattan_subtitle)[1],
                                  "\n",
                                  strwrap(stored_subtitle, char_width_grattan_subtitle)[2])
  }


  # add line break to caption where necessary

  contains_notes_and_source <- grepl("notes?:", tolower(stored_caption)) & grepl("sources?:", tolower(stored_caption))

  # if the caption doesn't contain "notes" and "source", we want to wrap the whole
  # caption string across lines; if notes and source are present we want to wrap them separately
  if(!contains_notes_and_source){
    caption_lines <- ceiling(nchar(stored_caption) / char_width_grattan_caption)

    if(caption_lines > 1){
      stored_caption <- paste0(strwrap(stored_caption, char_width_grattan_caption), collapse = "\n")
    }
  } else { # now deal with the case when "notes" and "source" are present
    notes_and_source <- strsplit(stored_caption, split = "Source")
    notes <- notes_and_source[[1]][1]
    source <- paste0("Source", notes_and_source[[1]][2])

    notes <- paste0(strwrap(notes, char_width_grattan_caption),
                    collapse = "\n")

    source <- paste0(strwrap(source, char_width_grattan_caption),
                     collapse = "\n")

    stored_caption <- paste0(notes, "\n", source)
  }

 p$labels$title <- stored_title
 p$labels$subtitle <- stored_subtitle
 p$labels$caption <- stored_caption

 object <- p

 object

}
