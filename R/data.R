#' Grattan chart types
#'
#' A tibble containing the Grattan chart types and their characteristics.
#'
#' @format A tibble with 12 varialbes:
#' \describe{
#'   \item{type}{Chart type, such as "normal" or "fullslide". Supply this to the `type` argument of `grattan_save()`.}
#'   \item{width}{Default width of the chart type, in cm.}
#'   \item{height}{Default height of the chart type, in cm.}
#'   \item{caption}{Number of characters per line of text in the caption for this chart type.}
#'   \item{title}{Number of characters per line of text in the title for this chart type.}
#'   \item{subtitle}{Number of characters per line of text in the subtitle for this chart type.}
#'   \item{class}{There are two "classes" of chart: "normal" and "fullslide". "Normal" charts are those bound for reports - by default they are saved without their labels and sans logo. Fullslide charts look like a Grattan Powerpoint slide, with the logo and orange line.}
#'   \item{pptx_template}{Filename of the Powerpoint corresponding to the chart type. `NA` if none present.}
#'   \item{top_border}{Top border, in cm.}
#'   \item{bottom_border}{Bottom border, in cm.}
#'   \item{left_border}{Left border, in cm.}
#'   \item{right_border}{Right border, in cm.}
#'
#' }

"chart_types"
