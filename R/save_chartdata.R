#' Save a ggplot2 chart as a properly-formatted "chart data" Excel spreadsheet
#'
#' Grattan reports are accompanied by Excel workbooks containing the data used
#' to produce the chart ("chart data") and the chart itself. This function takes
#' a ggplot2 object and creates a single-sheet Excel workbook with the chart
#' data and chart. If your ggplot2 object has a subtitle and caption, those will
#' be properly displayed in the spreadsheet.
#'
#' @name save_chartdata
#'
#' @param filename filename of the Excel workbook you wish to create. Can be a
#' file path, but the directory must already exist.
#' @param object ggplot2 chart to create chart data from. If left blank,
#' \code{ggplot2::last_plot()} is used to get the last plot displayed.
#' @param type type of plot. Default is "normal". See \code{?grattan_save} for
#' full list of types. Note that if labels (title,
#' subtitle, caption) are included in your chart and height is not manually
#' specified with the `height` argument, the plot height will be expanded a
#' little to accommodate the labels.
#' @param height Numeric, optional. Use this to override the default height
#' for plots of your chosen `type`; see \code{?grattan_save} for more details.
#'
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData insertImage createStyle addStyle setColWidths saveWorkbook
#' @importFrom ggplot2 last_plot
#'
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'      geom_point() +
#'      theme_grattan() +
#'      labs(title = "Title",
#'           subtitle = "Subtitle",
#'           caption = "Caption")
#'
#' \dontrun{save_chartdata("my_chartdata.xlsx", p)}
#'

save_chartdata <- function(filename, object = ggplot2::last_plot(),
                           type = "normal", height = NULL) {

  # check inputs

  if(tools::file_ext(filename) != "xlsx") {
    stop(paste0(filename, " is not a valid filename; filename must end in .xlsx"))
  }

  if(!"ggplot" %in% class(object)) {
    stop("`object` is not a ggplot2 object")
  }

  if(!type %in% chart_types$type) {
    stop(paste0(type, " is not a recognised chart type; see ?grattan_save for types."))
  }

  obj_name <- deparse(substitute(object))

  if(obj_name == "ggplot2::last_plot()") {
    obj_name <- "plot"
  }

  # Expand height of graph if not set manually & labels are present

  if(is.null(height)) {

    labels_present <- ifelse(!is.null(object$labels$caption) |
                               !is.null(object$labels$title) |
                               !is.null(object$labels$subtitle),
                             TRUE,
                             FALSE)

    if(isTRUE(labels_present)) {
      height <- chart_types$height[chart_types$type == "normal"] + 2
    }

  }

  if(is.null(height)) {
    height <- chart_types$height[chart_types$type == type]
  }

  # Save graph
  temp_image_location <- file.path(tempdir(), "chart_data_image.png")

  grattan_save(temp_image_location,
               object = object,
               type = type,
               height = height,
               force_labs = TRUE,
               warn_labs = FALSE)

  # Get chart data
  chart_data <- object$data
  for(col in seq_along(chart_data)) { # To ensure that dates are correctly-formatted, save as strings
    if(inherits(chart_data[[col]], "Date")) {
      chart_data[[col]] <- as.character(chart_data[[col]])
    }
  }

  names(chart_data) <- tools::toTitleCase(names(chart_data))

  data_columns <- ncol(chart_data)
  data_rows <- nrow(chart_data)

  # Create workbook and add content
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb,
                         sheetName = obj_name,
                         gridLines = FALSE)

  plot_subtitle <- object$labels$subtitle
  plot_caption <- object$labels$caption

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = plot_subtitle,
                      startCol = 2,
                      startRow = 1)

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = plot_caption,
                      startCol = 2,
                      startRow = 5 + data_rows)


  openxlsx::writeData(wb,
                      sheet = 1,
                      x = chart_data,
                      startCol = 2,
                      startRow = 3)

  openxlsx::insertImage(wb = wb,
                        sheet = 1,
                        startRow = 3,
                        startCol = data_columns + 3,
                        file = temp_image_location,
                        width = chart_types$width[chart_types$type == type] / 1.5,
                        height = height / 1.5,
                        units = "cm",
                        dpi = 320)


  # Change font of entire sheet
  grattan_font_style <- openxlsx::createStyle(fontName = "Arial",
                                              fontSize = 11,
                                              fontColour = "#000000")

  addStyle(wb, 1, grattan_font_style, cols = 1:100, rows = 1:2000, gridExpand = TRUE)

  # Bold title

  grattan_title_style <- openxlsx::createStyle(textDecoration = "bold",
                                               fontSize = 12)

  addStyle(wb, 1, grattan_title_style, cols = 2, rows = 1, stack = TRUE)

  # Orange fill for table

  grattan_table_style <- openxlsx::createStyle(fgFill = grattantheme::grattan_orange_alpha,
                                               bgFill = grattantheme::grattan_orange_alpha,
                                               wrapText = TRUE)

  addStyle(wb, 1,
           grattan_table_style,
           cols = 2:(data_columns + 1),
           rows = 3:(data_rows + 3),
           stack = TRUE,
           gridExpand = TRUE)

  # Italicise caption

  grattan_caption_style <- openxlsx::createStyle(textDecoration = c("italic",
                                                                    "underline"))

  addStyle(wb,
           1,
           grattan_caption_style,
           rows = 5 + data_rows,
           cols = 2,
           stack = TRUE)

  # Bold header

  grattan_heading_style <- openxlsx::createStyle(textDecoration = "bold")

  addStyle(wb,
           1,
           grattan_heading_style,
           rows = 3,
           cols = 2:(data_columns + 1),
           stack = TRUE)

  # Add borders

  grattan_leftborder <- openxlsx::createStyle(border = "left",
                                              borderColour = grattantheme::grattan_lightorange,
                                              borderStyle = "thick")

  grattan_rightborder <- openxlsx::createStyle(border = "right",
                                               borderColour = grattantheme::grattan_lightorange,
                                               borderStyle = "thick")

  grattan_topborder <- openxlsx::createStyle(border = "top",
                                             borderColour = grattantheme::grattan_lightorange,
                                             borderStyle = "thick")

  grattan_bottomborder <- openxlsx::createStyle(border = "bottom",
                                                borderColour = grattantheme::grattan_lightorange,
                                                borderStyle = "thick")

  openxlsx::addStyle(wb, 1,
                     grattan_leftborder,
                     rows = 3:(data_rows + 3),
                     cols = 2,
                     stack = TRUE)

  openxlsx::addStyle(wb, 1,
                     grattan_rightborder,
                     rows = 3:(data_rows + 3),
                     cols = data_columns + 1,
                     stack = TRUE)
  openxlsx::addStyle(wb, 1,
                     grattan_topborder,
                     rows = 3,
                     cols = 2:(data_columns + 1),
                     stack = TRUE)

  openxlsx::addStyle(wb, 1,
                     grattan_bottomborder,
                     rows = data_rows + 3,
                     cols = 2:(data_columns + 1),
                     stack = TRUE)

  # Resize first column

  openxlsx::setColWidths(wb, 1,
                         cols = 1,
                         widths = 0.45)

  suppressMessages(openxlsx::saveWorkbook(wb = wb,
                                          file = filename,
                                          overwrite = TRUE))


}
