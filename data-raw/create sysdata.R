library(devtools)

load("data-raw/logogrob.Rda")

chart_types <- tibble::tribble(
                         ~type, ~width, ~height, ~caption, ~title,  ~subtitle,
                      "normal",  22.16,    14.5,      120,     70,         75,
                  "normal_169",  30.00,    14.5,      150,     95,        100,
                        "tiny",  22.16,   11.08,      120,     70,         75,
                 "wholecolumn",  22.16,   22.16,      120,     70,         75,
                    "fullpage",  44.32,   22.16,      240,    140,        150,
                   "fullslide",   25.4,   19.05,      140,     55,         70,
               "fullslide_169",   33.87, 19.05,       180,     55,         95,
                "fullslide_44",   25.4,  25.4,        140,     50,         95)


use_data(logogrob, chart_types, internal = TRUE, overwrite = TRUE)
