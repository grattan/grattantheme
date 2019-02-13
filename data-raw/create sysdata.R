library(devtools)

load("data-raw/logogrob.Rda")

chart_types <- tibble::tribble(
                         ~type, ~width, ~height,
                      "normal",  22.16,    14.5,
                        "tiny",  22.16,   11.08,
                 "wholecolumn",  22.16,   22.16,
                    "fullpage",  44.32,   22.16,
                   "fullslide",   25.4,   19.05
                 )


use_data(logogrob, chart_types, internal = TRUE, overwrite = TRUE)
