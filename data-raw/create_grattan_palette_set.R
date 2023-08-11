# Create Grattan palette sets

# Assume more/higher is better; and better is yellow (can be reversed with reverse = TRUE)
#' The grattan colours
#'
#' These colours are used in \code{grattan_plot} and form the basis for our
#' palettes.
#'
#' @export
grattan_palette_set <- list(
  `full`  = c(grattan_red,
              grattan_darkorange,
              grattan_lightorange,
              grattan_yellow,
              grattan_lightyellow),

  `full_f`  = c(grattan_red_f,
                grattan_darkorange_f,
                grattan_lightorange_f,
                grattan_yellow_f,
                grattan_lightyellow_f),

  `light`  = c(grattan_lightorange,
               grattan_yellow,
               grattan_lightyellow),

  `dark`   = c(grattan_red, grattan_darkorange,
               grattan_lightorange),

  `diverging` = c(grattan_red, grattan_red_f,
                  "white",
                  grattan_orange_f, grattan_orange),

  `grey`  = c(grattan_grey1,
              grattan_grey2,
              grattan_grey3,
              grattan_grey4,
              grattan_grey5)
)


usethis::use_data(grattan_palette_set, overwrite = TRUE)
