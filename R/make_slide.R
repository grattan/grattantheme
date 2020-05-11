#' Create Powerpoint slides (deprecated)
#'
#' @description
#'
#'   These functions have been deprecated (removed) from the grattantheme
#'   package. Use the new function `grattan_save_pptx()` instead.
#'
#' @param graph The graph you want to include on your slide. Defaults to the
#'   last plot (ggplot2::last_plot())
#' @param filename The filename for your Powerpoint slide.
#' @param path Path to the directory you wish to save your slide in. Defaults to
#'   your working directory.
#' @param type The type of Powerpoint slide to create; either "16:9" (the
#'   default, for a 16:9 orientation slide), or "4:3".
#' @importFrom rmarkdown render
#' @importFrom knitr opts_chunk
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown pandoc_version pandoc_available
#' @importFrom rstudioapi getActiveDocumentContext
#'
#' @examples
#'
#' # To take a single ggplot2 graph and turn it into a slide:
#' # First, create your ggplot2 object. Include your subtitle,
#' # title, and caption with +labs()
#'
#'library(ggplot2)
#'library(grattantheme)
#'
#'ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_point() +
#'    theme_grattan() +
#'    labs(title = "Long title goes here with a bunch of text lorem ipsum
#'    Grattan blah blah sensible orange",
#'    subtitle = "Subtitle",
#'    caption = "Source: blah")
#'
#' # Now, create a Powerpoint slide with your graph in your working directory.
#' # By default, make_slide() will use the last plot you generated;
#' # you can specify a different object to use with the `graph` argument.
#'
#' \dontrun{make_slide(filename = "test.pptx")}
#'
#' @export


make_slide <- function(graph = last_plot(),
                       filename = NULL,
                       path = ".",
                       type = "16:9") {

  .Deprecated("grattan_save_pptx()",
              msg = "make_presentation() is deprecated Use `grattan_save_pptx()`.")

}

#' @param graphs A list of ggplot2 objects
#' @param title Optional. The title of your presentation, if you want a title
#'   slide.
#' @param subtitle Optional. The subtitle of your presentation, if you want a
#'   title slide.
#' @rdname make_slide
#' @name make_presentation
#' @examples
#'
#' # To take multiple ggplot2 graphs and turn them into slides in a single
#' # presentation:
#' # First, create multiple ggplot2 objects. Include your subtitle, title,
#' # and caption with +labs()
#'
#'library(ggplot2)
#'library(grattantheme)
#'
#'graph1 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_point() +
#'    theme_grattan() +
#'    labs(title = "Title goes here with a bunch of text lorem ipsum",
#'    subtitle = "Subtitle",
#'    caption = "Source: blah")
#'
#'graph2 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'    geom_col() +
#'    theme_grattan() +
#'    labs(title = "This is another graph to go on the second slide",
#'    subtitle = "Units go here",
#'    caption = "Source: mtcars.")
#'
#'# Now combine your two graphs together in a list:
#'
#'graphs <- list(graph1, graph2)
#'
#' # Now, create a Powerpoint presentation with one
#' # slide per graph in your list:
#'
#' \dontrun{make_presentation(graphs, filename = "test.pptx")}
#'
#' @export


make_presentation <- function(graphs,
                              filename = NULL,
                              path = ".",
                              type = "16:9",
                              title = "Title",
                              subtitle = "Subtitle") {

  .Deprecated("grattan_save_pptx()",
              msg = "make_presentation() is deprecated. Use `grattan_save_pptx()`.")

}

