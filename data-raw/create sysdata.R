library(devtools)

load("data-raw/logogrob.Rda")

chart_types <- tibble::tribble(
                         ~type, ~width, ~height, ~caption, ~title,  ~subtitle, ~class,
                      "normal",  22.16,    14.5,      120,     70,         75, "normal",
                  "normal_169",  30.00,    14.5,      180,     95,        100, "normal",
                        "tiny",  22.16,   11.08,      120,     70,         75, "normal",
                 "wholecolumn",  22.16,   22.16,      120,     70,         75, "normal",
                    "fullpage",  44.32,   22.16,      240,    140,        150, "normal",
                   "fullslide",   25.4,   19.05,      140,     55,         70, "fullslide",
               "fullslide_169",   33.87, 19.05,       180,     55,         95, "fullslide",
                "fullslide_44",   25.4,  25.4,        140,     50,         95, "fullslide",
                        "blog",   25.4,   19.05,      165,     62,         85, "fullslide")


use_data(logogrob, chart_types, internal = TRUE, overwrite = TRUE)
