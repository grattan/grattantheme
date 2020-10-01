#' Save plots in Grattan-approved sizes and shapes
#'
#' Save ggplot2 object as an image in the correct size and resolution for
#' Grattan charts. Wrapper around ggsave().
#' `grattan_save_all()` is a wrapper around `grattan_save()` with `type = "all"`,
#' `save_pptx = TRUE`, `save_data = TRUE`.
#'
#' @name grattan_save
#' @param filename Required. The filename (including path
#'   where necessary) where you want to save your image. The filename
#'   should usually end in ".pdf" or ".png" (see \code{?ggsave}
#'   for the full list of allowed extensions/types). Note that if
#'   \code{type = "all"}, a subdirectory will be created based on your filename;
#'   the files will go in that subdirectory.
#'   This is the case for `grattan_save_all()`.
#' @param object The ggplot2 graph object to be saved. Defaults to
#'   \code{last_plot()}, which will save the last plot that was displayed in
#'   your session.
#' @param type Sets height and width to Grattan defaults. The following chart
#'   types are available:
#'
#' \itemize{ \item{"normal"}{ The default. Use for normal Grattan report charts,
#' or to paste into a 4:3 Powerpoint slide. Width: 22.2cm, height: 14.5cm.}
#' \item{"normal_169"}{ Only useful for pasting into a 16:9 format Grattan
#' Powerpoint slide. Width: 30cm, height: 14.5cm.} \item{"tiny"}{ Fills the
#' width of a column in a Grattan report, but is shorter than usual. Width:
#' 22.2cm, height: 11.1cm.} \item{"wholecolumn"}{ Takes up a whole column in a
#' Grattan report. Width: 22.2cm, height: 22.2cm.} \item{"fullpage"}{ Fills a
#' whole page of a Grattan report. Width: 44.3cm, height: 22.2cm.}
#' \item{"fullslide}{ Creates an image that looks like a 4:3 Grattan Powerpoint
#' slide, complete with logo.  Width: 25.4cm, height: 19.0cm.}
#' \item{"fullslide_169}{ Creates an image that looks like a 16:9 Grattan
#' Powerpoint slide, complete with logo. Use this to drop into standard
#' presentations. Width: 33.9cm, height: 19.0cm} \item{"blog"}{"Creates a 4:3
#' image that looks like a Grattan Powerpoint slide, but with less border
#' whitespace than `fullslide`."} \item{"fullslide_44"}{Creates an image that
#' looks like a 4:4 Grattan Powerpoint slide. This may be useful for taller
#' charts for the Grattan blog; not useful for any other purpose. Width: 25.4cm,
#' height: 25.4cm.}
#' }
#' Set type = "all" to save your chart in all available sizes.
#' @param height Numeric, optional. NULL by default. Controls the height (in cm)
#'   of the image you wish to save. If specified, `height` will override the
#'   default height for your chosen chart type.
#' @param save_pptx `FALSE` by default. If `TRUE`, a Powerpoint presentation
#' containing your graph will be created. Note that Powerpoint templates are
#' not available for all chart types; see \code{?grattan_save_pptx()} for
#' available types. If `type = "all"`, Powerpoint presentations will be created
#' for each type for which a Powerpoint template exists.
#' @param save_data Logical. Default is FALSE. If set to
#'   TRUE, a properly-formatted .xlsx file will be created containing the
#'   dataframe you passed to ggplot(). The filename and path will be the same as
#'   your image, but with a .xlsx extension.
#' @param force_labs Logical. By default, `grattan_save()` will remove your
#'   title, subtitle, and caption (if present) from your graph before saving it,
#'   unless `type` = "fullslide". By setting `force_labs` to TRUE, your
#'   title/subtitle/caption will be retained regardless of `type`.
#' @param watermark Character. NULL by default. If a string, like `DRAFT`,
#' is supplied, this string will be added to your plot as a watermark.
#' See `?watermark` for options - to use these, call `watermark()` directly
#' before saving your plot.
#' @param latex Logical. FALSE by default. If TRUE, exports figure environment
#' LaTeX code to clipboard and console.
#' @param dpi Plot resolution. Default is "retina".
#' @param ... arguments passed to `ggsave()`. For example, use
#' `device = cairo_pdf` to use the Cairo PDF rendering engine.
#' For `grattan_save_all()`, the `...` are passed to `grattan_save()`.
#'
#' @import ggplot2
#' @importFrom utils tail
#' @importFrom purrr walk2
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' \dontrun{grattan_save("p.png", p)}
#'
#' # If you don't assign your chart to an object name, that's OK, it will still
#' # save if it was the last plot displayed.
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     theme_grattan()
#'
#' \dontrun{grattan_save("p.png")}
#'
#'
#' # Want to make a 'self-contained' chart that includes a
#' # title/subtitle/caption, eg. to go on the Grattan Blog?
#' # If so, just add them - they'll be properly
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
#'  \dontrun{grattan_save("your_file.png")}
#'
#'  # Want to make a full Powerpoint slide? Just use type = "fullslide"
#'  # in grattan_save(), like this.
#'  # If you include 'notes' and 'source' as below, grattan_save() will
#'  # automatically
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
#'  \dontrun{grattan_save("your_file.png", type = "normal")}
#'
#'
#' @export

grattan_save <- function(filename,
                         object = ggplot2::last_plot(),
                         type = "normal",
                         height = NULL,
                         save_pptx = FALSE,
                         save_data = FALSE,
                         force_labs = FALSE,
                         watermark = NULL,
                         latex = FALSE,
                         dpi = "retina",
                         ...) {

  if (!type %in% c("all", all_chart_types)) {
    stop(type,
         "is not a valid chart type.\n",
         "See ?grattan_save for valid types.")
  }

  if (isFALSE(inherits(object, "gg"))) {
    stop("`object` is not a ggplot2 object.")
  }

  original_object <- object

  if (isTRUE(latex)) export_latex_code(object, filename)

  if (!is.null(watermark)) {
    object <- object + watermark(watermark)
  }

  if (!is.logical(save_pptx)) {
    stop("save_pptx must be either TRUE or FALSE.")
  }

  if (type != "all") {
    if (isTRUE(save_data)) {
        save_chartdata(filename = paste0(sub("\\..*", "", filename), ".xlsx"),
                       object = object,
                       type = type,
                       height = height)
    }

    if (isTRUE(save_pptx)) {
      template <- chart_types$pptx_template[chart_types$type == type]
      template_exists <- ifelse(is.na(template), FALSE, TRUE)

      if (isFALSE(template_exists)) {
        warning("Cannot save Powerpoint for type '", type, "'.")
      } else {
        pptx_filename <- paste0(tools::file_path_sans_ext(filename), ".pptx")

        grattan_save_pptx(p = object,
                          type = type,
                          filename = pptx_filename)
      }
    }

    grattan_save_(filename = filename,
                  object = object,
                  type = type,
                  height = height,
                  force_labs = force_labs,
                  dpi = dpi,
                  save_pptx = save_pptx,
                  ...)
  }

  if (type == "all") {
    dir <- tools::file_path_sans_ext(filename)
    filetype <- tools::file_ext(filename)
    file_name <- tools::file_path_sans_ext(basename(filename))

    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    filenames <- file.path(dir, paste0(file_name, "_", all_chart_types, ".", filetype))

    if (isTRUE(save_data)) {
      save_chartdata(filename = file.path(dir, paste0(file_name, ".xlsx")),
                     object = object,
                     type = "normal",
                     height = height)
    }

    if (isTRUE(save_pptx)) {
      template <- chart_types$pptx_template[chart_types$type == all_chart_types]
      template_exists <- !is.na(template)
      valid_pptx_types <- all_chart_types[template_exists]
      pptx_filenames <- file.path(dir,
                                  paste0(file_name, "_",
                                         valid_pptx_types,
                                         ".pptx"))


      walk2(.x = pptx_filenames,
            .y = valid_pptx_types,
            .f = ~grattan_save_pptx(p = object,
                                    filename = .x,
                                    type = .y))

    }

    purrr::walk2(.x = filenames,
                 .y = all_chart_types,
                 .f = grattan_save_,
                 object = object,
                 height = height,
                 force_labs = force_labs,
                 dpi = dpi,
                 save_pptx = save_pptx,
                 ...)

  }

  ggplot2::set_last_plot(original_object)

}



#### grattan_save_() is an internal function that does the actual work of saving
#### individual plots; it is called by grattan_save()
grattan_save_ <- function(filename,
                          object,
                          type,
                          height,
                          force_labs,
                          dpi,
                          save_pptx,
                          ...) {

  plot_class <- chart_types$class[chart_types$type == type]

  # create an image the size of a 4:3 Powerpoint slide complete with Grattan
  # logo
  if (plot_class == "fullslide") {

    object <- wrap_labs(object, type)

    object <- create_fullslide(plot = object,
                               type = type)

  } else { # following code only applies if type != "fullslide"

    if (isFALSE(force_labs)) {
      # Unless force_labs == TRUE (indicating the user wishes
      # to retain their labels)
      # Remove title, subtitle and caption for type != "fullslide"

      object <- object +
        theme(plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.caption = element_blank())

    } else {
    # non-fullslide, force_labs = TRUE
        object <- wrap_labs(object, type)
    }

  }

  width <- chart_types$width[chart_types$type == type]

  if (is.null(height)) {
    height <- chart_types$height[chart_types$type == type]
  }


  ggplot2::ggsave(filename, object,
                  width = width, height = height, units = "cm",
                  dpi = dpi, ...)

}

#' @name grattan_save_all
#' @rdname grattan_save
#' @export

grattan_save_all <- function(filename,
                             object = ggplot2::last_plot(),
                             ...) {
  grattan_save(filename = filename,
               object = object,
               type = "all",
               save_pptx = TRUE,
               save_data = TRUE,
               ...)
}
