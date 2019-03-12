#' @importFrom ggplot2 update_geom_defaults .pt

grattanify_geom_defaults <- function() {
    # Define defaults for individual geoms in a style guide-consistent way

    # Note: looks as if update_geom_defaults() may be deprecated in a future ggplot2
    # release (see https://github.com/tidyverse/ggplot2/pull/2749) in favour of a new
    # way to update geom defaults; when this happens, replace the code below
    ggplot2::update_geom_defaults("point", list(colour = grattantheme::grattan_lightorange,
                                                size = 6 / .pt ))
    ggplot2::update_geom_defaults("bar", list(colour = "white",
                                              fill = grattantheme::grattan_lightorange,
                                              size = 0.75 / .pt ))
    ggplot2::update_geom_defaults("col", list(colour = "white",
                                              fill = grattantheme::grattan_lightorange,
                                              size = 0.75 / .pt ))
    ggplot2::update_geom_defaults("line", list(colour = grattantheme::grattan_lightorange,
                                               size = 3 / .pt))
    ggplot2::update_geom_defaults("text", list(colour = "black",
                                               size = 18 / .pt))
    ggplot2::update_geom_defaults("smooth", list(colour = grattantheme::grattan_lightorange,
                                                 fill = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("path", list(colour = grattantheme::grattan_lightorange,
                                               size = 3 / .pt))

    ggplot2::update_geom_defaults(ggrepel::GeomTextRepel, list(size = 18 / .pt,
                                                               colour = "black"))

    ggplot2::update_geom_defaults(ggrepel::GeomLabelRepel, list(size = 18 / .pt,
                                                                fill = "white",
                                                                colour = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("label", list(size = 18 / .pt, fill = "white"))

    ggplot2::update_geom_defaults("area", list(fill = grattantheme::grattan_lightorange,
                                               col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("density", list(fill = grattantheme::grattan_lightorange,
                                                  col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("dotplot", list(fill = grattantheme::grattan_lightorange,
                                                  col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("polygon", list(fill = grattantheme::grattan_lightorange,
                                                  col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("path", list(col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("ribbon", list(fill = grattantheme::grattan_lightorange,
                                                 col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("rect", list(fill = grattantheme::grattan_lightorange,
                                               col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("boxplot", list(fill = grattantheme::grattan_orange_alpha,
                                                  col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("crossbar", list(fill = grattantheme::grattan_lightorange,
                                                   col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("errorbar", list(col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("linerange", list(col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("pointrange", list(col = grattantheme::grattan_lightorange))

    ggplot2::update_geom_defaults("tile", list(col = "white",
                                               fill = grattantheme::grattan_lightorange))
}
