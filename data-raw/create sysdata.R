library(devtools)
library(dplyr)

logo <- magick::image_read_pdf("data-raw/GrattanSVGLogo.pdf")

logogrob <- grid::rasterGrob(logo)

chart_types <- tibble::tribble(
                         ~type, ~status,      ~width, ~height, ~caption, ~title,  ~subtitle, ~class,     ~pptx_template,
                      "normal", "active",        22.16,   14.50,    120,     70,         75, "normal",    "template_normal.pptx",
                 "wholecolumn", "active",        22.16,   22.16,    120,     70,         75, "normal",    "template_wholecolumn.pptx",
                    "fullpage", "active",        44.32,   22.16,    240,    140,        150, "normal",    "template_fullpage.pptx",
                  "normal_169", "active",        30.00,   14.50,    180,     95,        100, "normal",    NA_character_,
                        "tiny", "active",        22.16,   11.08,    120,     70,         75, "normal",    NA_character_,

               "fullslide_old", "deprecated",        33.87,   19.05,    175,     55,         95, "fullslide", "template_169.pptx",
                "fullslide_43", "deprecated",        25.40,   19.05,    140,     55,         70, "fullslide", "template_43.pptx",
                        "blog", "deprecated",    25.40,   19.05,    155,     62,         85, "fullslide", "template_blog.pptx",
                          "a4", "active",        21.00,   29.70,    114,     66,         62, "fullslide", NA_character_,
                   "fullslide", "active",        31.5,       11.9,    240,     55,         95, "fullslide", "template_fullslide.pptx",
            "fullslide_narrow", "active",        23.0,       11.9,    175,     55,         95, "fullslide", "template_fullslide_narrow.pptx",

                "fullslide_44", "deprecated",   25.40,   25.40,    140,     55,         95, "fullslide", NA_character_,
                   "blog_half", "deprecated",   25.4/2,  19.05,    155,     62,         85, "fullslide", "template_blog_half.pptx",
            "fullslide_old169", "deprecated",   25.40,   14.29,    140,     55,         70, "fullslide", "template_old_169.pptx",
)


blog_border <- 0.15

chart_types <- chart_types %>%
  dplyr::mutate(top_border = dplyr::case_when(class == "normal" ~ 0,
                                grepl("blog", type) ~ blog_border,
                                type == "fullslide_old169" ~ 0.5,
                                TRUE ~ 0.7),
         bottom_border = dplyr::case_when(class == "normal" ~ 0,
                                   grepl("blog", type) ~ 0.05,
                                   type %in% c("fullslide",
                                               "fullslide_44") ~ 0.24,
                                   TRUE ~ 0.05),
         left_border = dplyr::case_when(class == "normal" ~ 0,
                                 type %in% c("fullslide_43",
                                             "fullslide_44") ~ (width - 22.16) / 2,
                                 type == "fullslide" ~ (width - 30) / 2,
                                 type == "a4" ~ (width - 19) / 2,
                                 type == "fullslide_old169" ~ (width - 22.64) / 2,
                                 grepl("blog", type) ~ blog_border),
         right_border = left_border)

chart_types_inc_deprecated <- chart_types
chart_types <- chart_types[chart_types$status == "active", ]

all_chart_types <- chart_types$type[chart_types$status == "active"]
all_chart_types_inc_deprecated <- chart_types$type

fullslide_chart_types <- chart_types$type[chart_types$class == "fullslide" & chart_types$status == "active"]
fullslide_chart_types_inc_deprecated <- chart_types$type[chart_types$class == "fullslide"]

usethis::use_data(logogrob,
         chart_types,
         chart_types_inc_deprecated,
         all_chart_types,
         all_chart_types_inc_deprecated,
         fullslide_chart_types,
         fullslide_chart_types_inc_deprecated,
         internal = TRUE,
         overwrite = TRUE)

chart_types_ext <- chart_types
usethis::use_data(chart_types_ext,
         internal = FALSE,
         overwrite = TRUE)

