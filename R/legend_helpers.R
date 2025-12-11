#' Create the css needed to colour text using ggtext
#'
#' This is a helper function to wrap the css to make text certain colours. This
#' will be useful when making grattan charts. This should be used within `ggtext::geom_richtext()`
#' or if being used in a sub-title or title it should be used in the `labs()` call together with
#' `theme(plot.subtitle = element_markdown())`
#'
#'
#' @param colour (a character) the colour you want to colour text
#' @param text (a character) what the coloured text should say
#' @param is_note (logical; default = FALSE) is it a note?
#' @param bold_labs (logical; default = TRUE) should the labels be bolded?
#'
#' @return (a glue)
#' @export
#'
#' @examples
#' \dontrun{library(ggtext)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'     labs(
#'         title = glue::glue("**Fisher's *Iris* dataset**
#'             <span style='font-size:11pt'>Sepal width vs. sepal length for
#'                                    {colour_text('#0072B2','setosa')}
#'                                    {colour_text('#D55E00','versicolor')}
#'                                    {colour_text('#009E73','virginica')}
#'                                        </span>"),
#'                                          theme(plot.title = element_markdown(lineheight = 1.1) )
#' }
colour_text <- function(colour, text, is_note = FALSE, bold_labs = TRUE) {
  assertthat::assert_that(is.character(colour), msg = "`colour` should be a character")
  assertthat::assert_that(is.character(text), msg = "`text` should be a character")
  if (!is_note) {
    if (bold_labs) {
      ret <- glue::glue("<b style='color:{colour}'>{text}</b>")
    } else {
      ret <- glue::glue("<span style='color:{colour}'>{text}</span>")
    }
  }
  if (is_note) ret <- glue::glue("<span style='color:{grattan_grey3}'>{text}</span>")

  return(ret)
}
