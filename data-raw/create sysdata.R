library(devtools)

logo <- magick::image_read_pdf("data-raw/GrattanSVGLogo.pdf")

logogrob <- grid::rasterGrob(logo)

chart_types <- tibble::tribble(
                         ~type, ~width, ~height, ~caption, ~title,  ~subtitle, ~class, ~pptx_template,
                      "normal",  22.16,    14.5,      120,     70,         75, "normal", "template_normal.pptx",
                  "normal_169",  30.00,    14.5,      180,     95,        100, "normal", NA_character_,
                        "tiny",  22.16,   11.08,      120,     70,         75, "normal", NA_character_,
                 "wholecolumn",  22.16,   22.16,      120,     70,         75, "normal", "template_wholecolumn.pptx",
                    "fullpage",  44.32,   22.16,      240,    140,        150, "normal", "template_fullpage.pptx",
                   "fullslide",   25.4,   19.05,      140,     55,         70, "fullslide", "template_43.pptx",
               "fullslide_169",   33.87, 19.05,       175,     55,         95, "fullslide", "template_169.pptx",
                "fullslide_44",   25.4,  25.4,        140,     50,         95, "fullslide", NA_character_,
                        "blog",   25.4,   19.05,      155,     62,         85, "fullslide", NA_character_)


blog_border <- 0.15

chart_types <- chart_types %>%
  mutate(top_border = case_when(class == "normal" ~ 0,
                                type == "blog" ~ blog_border,
                                TRUE ~ 0.7),
         bottom_border = case_when(class == "normal" ~ 0,
                                   type == "blog" ~ 0.05,
                                   type %in% c("fullslide_169",
                                               "fullslide_44") ~ 0.24,
                                   TRUE ~ 0.05),
         left_border = case_when(class == "normal" ~ 0,
                                 type %in% c("fullslide",
                                             "fullslide_44") ~ (width - 22.16) / 2,
                                 type == "fullslide_169" ~ (width - 30) / 2,
                                 type == "blog" ~ blog_border),
         right_border = left_border)

all_chart_types <- chart_types$type

fullslide_chart_types <- chart_types$type[chart_types$class == "fullslide"]

use_data(logogrob,
         all_chart_types,
         fullslide_chart_types,
         chart_types,
         internal = TRUE,
         overwrite = TRUE)
