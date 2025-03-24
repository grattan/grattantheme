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
#' @param select_data optional. Removes any columns that are not used in ggplot 
#' mappings and facets from the exported chart data. Default is TRUE.
#' @param round Numeric, optional. Round numbers in the chart data to this
#' number of decimal places. Default is NULL, which does not round numbers.
#'
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData insertImage
#' @importFrom openxlsx createStyle addStyle setColWidths saveWorkbook
#' @importFrom ggplot2 last_plot
#' @importFrom purrr map
#' @importFrom rlang quo_get_expr
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

save_chartdata <- function(filename,
                           object = ggplot2::last_plot(),
                           type = "normal",
                           height = NULL,
                           select_data = TRUE, 
                           round = NULL) {

  if (tools::file_ext(filename) != "xlsx") {
    stop(filename, " is not a valid filename; filename must end in .xlsx")
  }

  # check inputs
  if (!inherits(object, "ggplot")) {
    stop("`object` is not a ggplot2 object")
  }
  
  if (!type %in% chart_types$type) {
    stop(type,
         " is not a recognised chart type;",
         " see ?grattan_save for types.")
  }
  

  obj_name <- deparse(substitute(object))

  if (obj_name == "ggplot2::last_plot()") {
    obj_name <- "plot"
  }

  # Expand height of graph if not set manually & labels are present

  if (is.null(height)) {

    labels_present <- ifelse(!is.null(object$labels$caption) |
                               !is.null(object$labels$title) |
                               !is.null(object$labels$subtitle),
                             TRUE,
                             FALSE)

    if (isTRUE(labels_present)) {
      height <- chart_types$height[chart_types$type == type] + 3
    } else {
      height <- chart_types$height[chart_types$type == type]
    }
  }


  # Save graph
  image_name <- "chart_data_image"
  temp_path <- tempdir()
  temp_image_location <- file.path(temp_path, paste0(image_name, ".png"))
  temp_image_end_location <- file.path(temp_path, image_name, paste0(image_name, "_", type, ".png"))

  grattan_save(temp_image_location,
               object = object,
               type = type,
               height = height,
               force_labs = TRUE,
               dpi = 72)
  
  # Check whether the plot has been created using patchwork
  patchwork <- inherits(object, c("patchwork"))

  # Extract and clean the chart data
  
  if (patchwork) {
    chart_data <- map(seq_along(object), function(i) {clean_chartdata_(object[[i]], select_data, round)})
  } else {
    chart_data <- clean_chartdata_(object, select_data, round)
  }
  
  # Find total number of columns and rows across all data frames

  if(patchwork) {
    # For multiple data frames, find the maximum number of columns...
    max_data_columns <- max(unlist(map(chart_data, ncol)))
    # ...and find the total number of rows, including space between
    max_data_rows <- sum(unlist(map(chart_data, ~nrow(.x) + 1))) - 1
  } else {
    max_data_columns <- ncol(chart_data)
    max_data_rows <- nrow(chart_data)
  }
  
  # Extract subtitle and caption from the ggplot or patchwork objects
  
  if (patchwork) {
    plot_subtitle <- object$patches$annotation$subtitle
    plot_caption <- object$patches$annotation$caption
    if (is.null(plot_subtitle)) {
      warning("No subtitle found in patchwork object. Use plot_annotation() to add a subtitle.")
    }
    if (is.null(plot_caption)) {
      warning("No caption found in patchwork object. Use plot_annotation() to add a caption")
    }
  } else {
    plot_subtitle <- object$labels$subtitle
    plot_caption <- object$labels$caption
  }
  
  # Remove everything before "Source:" in caption
  plot_caption <- gsub(".+?(?=Source:)", "", plot_caption, perl = TRUE)
  
  # Create workbook and add title, caption, footnotes, and chart
  
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb,
                         sheetName = obj_name,
                         gridLines = FALSE)

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = plot_subtitle,
                      startCol = 2,
                      startRow = 1)

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = plot_caption,
                      startCol = 2,
                      startRow = 5 + max_data_rows)

  openxlsx::insertImage(wb = wb,
                        sheet = 1,
                        startRow = 3,
                        startCol = max_data_columns + 3,
                        file = temp_image_end_location,
                        width = chart_types$width[chart_types$type == type] / 1.5,
                        height = height / 1.5,
                        units = "cm",
                        dpi = 320)
  
  # Change font of entire sheet
  grattan_font_style <- openxlsx::createStyle(fontName = "Arial",
                                              fontSize = 11,
                                              halign = "center",
                                              fontColour = "#000000")
  
  addStyle(wb, 1, grattan_font_style, cols = 1:100, rows = 1:2000,
           gridExpand = TRUE)

  # Bold title

  grattan_title_style <- openxlsx::createStyle(textDecoration = "bold",
                                               halign = "left",
                                               fontSize = 12)

  addStyle(wb, 1, grattan_title_style, cols = 2, rows = 1, stack = TRUE)
  
  # Italicise caption
  
  grattan_caption_style <- openxlsx::createStyle(textDecoration = c("italic",
                                                                    "underline"),
                                                 halign = "left")
  
  addStyle(wb,
           1,
           grattan_caption_style,
           rows = 5 + max_data_rows,
           cols = 2,
           stack = TRUE)
  
  # Define the styles needed for formatting the chart data boxes
  
  grattan_table_style <- openxlsx::createStyle(fgFill = grattantheme::grattan_orange_alpha,
                                               bgFill = grattantheme::grattan_orange_alpha,
                                               wrapText = TRUE)
  
  grattan_border <- function(border,
                             border_colour = grattantheme::grattan_lightorange,
                             border_style = "thick") {
    openxlsx::createStyle(border = border,
                          borderColour = border_colour,
                          borderStyle = border_style)
  }
  
  grattan_heading_style <- openxlsx::createStyle(textDecoration = "bold")
  
  # Create a list of dataframes to write
  
  if (is.data.frame(chart_data)) {
    data_list <- list(chart_data)
  } else{
    data_list <- chart_data
  }
  
  # Set starting row for writing data
  
  current_row <- 3
  
  # Add the chart data to the workbook and format for each dataframe
  
  for (i in seq_along(data_list)) {
    
    chart_data <- data_list[[i]]
    data_rows <- nrow(chart_data)
    data_columns <- ncol(chart_data)
    
    # Add data
    openxlsx::writeData(wb,
                        sheet = 1,
                        x = chart_data,
                        startCol = 2,
                        startRow = current_row)
    
    # Orange fill for table
    
    addStyle(wb, 1,
             grattan_table_style,
             cols = 2:(data_columns + 1),
             rows = current_row:(current_row + data_rows),
             stack = TRUE,
             gridExpand = TRUE)
    
    # Bold header
    
    addStyle(wb,
             1,
             grattan_heading_style,
             rows = current_row,
             cols = 2:(data_columns + 1),
             stack = TRUE)
    
    # Add borders
    
    openxlsx::addStyle(wb, 1,
                       grattan_border("left"),
                       rows = current_row:(current_row + data_rows),
                       cols = 2,
                       stack = TRUE)
    
    openxlsx::addStyle(wb, 1,
                       grattan_border("right"),
                       rows = current_row:(current_row + data_rows),
                       cols = data_columns + 1,
                       stack = TRUE)
    
    openxlsx::addStyle(wb, 1,
                       grattan_border("top"),
                       rows = current_row,
                       cols = 2:(data_columns + 1),
                       stack = TRUE)
    
    openxlsx::addStyle(wb, 1,
                       grattan_border("bottom"),
                       rows = current_row + data_rows,
                       cols = 2:(data_columns + 1),
                       stack = TRUE)
    
    # Update current_row for next data frame and add 2 for spacing
    
    current_row <- current_row + data_rows + 2
    
  }

  # Resize first column

  openxlsx::setColWidths(wb, 1,
                         cols = 1,
                         widths = 0.45)

  # Save workbook ----
  # Use minimal compression, for speed
  # Only way to modify openxlsx compression level seems to be through an option
  user_option <- getOption("openxlsx.compressionlevel")
  options("openxlsx.compressionlevel" = 1)

  tempfile <- wb$saveWorkbook()

  file.copy(from = tempfile, to = filename, overwrite = TRUE)

  unlink(tempfile)
  unlink(temp_image_location)

  # Restore previous value for compression level
  options("openxlsx.compressionlevel" = user_option)

}


### clean_chartdata_ is an internal function that is used by save_chartdata to 
### extract and clean data from a ggplot2 object before it is written to Excel.


clean_chartdata_ <- function(object, 
                             select_data,
                             round) {
  
  chart_data <- object$data
  
  # Check dataframe
  if (is.null(chart_data) | !is.data.frame(chart_data)) {
    stop("No data found in chart. Note that charts created in gridExtra or cowplot are not fully supported.")
  }
  
  if (select_data) {
  
  # Find the columns used in the ggplot mappings
  if (!is.null(object$mapping)) {
    used_cols <- unlist(lapply(object$mapping, function(x) as.character(all.vars(x))))
  }
  
  # Find any columns that are used for facets
  if (!is.null(object$facet)) {
    facet_cols <- unlist(lapply(object$facet$params$facets, function(x) as.character(all.vars(x))))
    used_cols <- c(used_cols, facet_cols)
  }
  
  # Check that all of used_cols exist in the underlying data, if not throw a warning and filter only actually existing columns
  
  missing_cols <- setdiff(used_cols, names(chart_data))
  
  if (length(missing_cols) > 0) {
    
    warning(paste("The following columns are not present and have not been exported to chart data:", paste(missing_cols, collapse = ", "), ". Set select_data = FALSE to export all columns to chart data."))
    
    used_cols <- intersect(used_cols, names(chart_data))
    
  }
  
  }
  
  # Filter the chart data for only the columns used in the ggplot mappings and facets
  chart_data <- chart_data %>% 
    select(all_of(unname(used_cols))) 
  
  # Remove any `sf` columns
  chart_data <- as.data.frame(chart_data)
  chart_data <- subset(chart_data,
                       select = sapply(chart_data,
                                       function(x) !inherits(x, "sfc")))
  
  # To ensure that dates are correctly-formatted, save as strings
  for (col in seq_along(chart_data)) {
    if (inherits(chart_data[[col]], "Date")) {
      chart_data[[col]] <- as.character(chart_data[[col]])
    }
  }
  
  # Round numbers in numeric columns if option selected
  
  if (!is.null(round)) {
    chart_data <- chart_data %>% 
      mutate(across(where(is.numeric), ~round(., round)))
  }
  
  names(chart_data) <- tools::toTitleCase(names(chart_data))
  
  return(chart_data)
  
}

