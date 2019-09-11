# This function is called by `theme_grattan` but is not exported.

theme_grattan_scatter <- function(base_size = 18,
                                  base_family = "sans",
                                  background = "white",
                                  legend = "none",
                                  panel_borders = FALSE) {

  ret <- theme_grattan_base(base_size = base_size,
                            base_family = base_family,
                            background = background,
                            legend = legend,
                            panel_borders = panel_borders)

  ret

}
