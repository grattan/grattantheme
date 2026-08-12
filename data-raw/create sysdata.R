library(devtools)
library(dplyr)

# Keep the logo's transparency so it sits on whatever background it is placed
# on (the grey header in blog/fullslide charts) rather than a white box.
logo <- magick::image_read("data-raw/GrattanPNGlogo.png")

logogrob <- grid::rasterGrob(logo)

chart_types <- tibble::tribble(
                         ~type,      ~status,  ~width, ~height, ~caption, ~title,  ~subtitle, ~class,     ~pptx_template,
                      "normal",     "active",   22.16,   14.50,    120,     70,         75, "normal",    "template_normal.pptx",
                 "wholecolumn",     "active",   22.16,   22.16,    120,     70,         75, "normal",    "template_wholecolumn.pptx",
                    "fullpage",     "active",   44.32,   22.16,    240,    140,        150, "normal",    "template_fullpage.pptx",
                   "fullslide",     "active",    31.7,    11.9,    240,     50,         95, "fullslide", "template_fullslide.pptx",
            "fullslide_narrow",     "active",    23.0,    11.9,    175,     50,         95, "fullslide", "template_fullslide_narrow.pptx",
              "fullslide_half",     "active",    15.3,    11.9,    120,     50,         47, "fullslide", "template_fullslide_half.pptx",
                        "blog",     "active",   23.16,   23.16,    120,     40,         65, "blog",      "template_blog.pptx",

# Following the `lifecycle` convention, 'deprecated' types still work but warn:
# they have a surviving .pptx template in inst/extdata, so `grattan_save_pptx()`
# accepts them and an old deck can be regenerated in its original format.
               "fullslide_old", "deprecated",   33.87,   19.05,    175,     55,         95, "fullslide", "template_169.pptx",
                "fullslide_43", "deprecated",   25.40,   19.05,    140,     55,         70, "fullslide", "template_43.pptx",

# 'defunct' types are not accepted anywhere. Their Powerpoint templates were
# deleted in 1.0.0; nothing stops an image being drawn at these dimensions, but
# a chart type that can't reach a slide isn't worth maintaining, so support was
# withdrawn rather than removed on technical grounds. They are kept here so
# their dimensions stay on the record, and so an old script that names one gets
# a useful error rather than a bare "not a valid chart type".
                  "normal_169",   "defunct",   30.00,   14.50,    180,     95,        100, "normal",    NA_character_,
                        "tiny",   "defunct",   22.16,   11.08,    120,     70,         75, "normal",    NA_character_,
                          "a4",   "defunct",   21.00,   29.70,    114,     66,         62, "fullslide", NA_character_,
                "fullslide_44",   "defunct",   25.40,   25.40,    140,     55,         95, "fullslide", NA_character_,
                   "blog_half",   "defunct",   25.4/2,  19.05,    155,     62,         85, "fullslide", NA_character_,
            "fullslide_old169",   "defunct",   25.40,   14.29,    140,     55,         70, "fullslide", NA_character_,
)


blog_half_border <- 0.15

chart_types <- chart_types %>%
  dplyr::mutate(top_border = dplyr::case_when(class == "normal" ~ 0,
                                class == "blog" ~ 0,
                                type == "blog_half" ~ blog_half_border,
                                type == "fullslide_old169" ~ 0.5,
                                type %in% c("fullslide",
                                            "fullslide_narrow",
                                            "fullslide_half") ~ 0,
                                TRUE ~ 0.7),
         bottom_border = dplyr::case_when(class == "normal" ~ 0,
                                  class == "blog" ~ 0,
                                  type %in% c("fullslide",
                                              "fullslide_narrow",
                                              "fullslide_half") ~ 0,
                                   type == "blog_half" ~ 0.05,
                                   type %in% c("fullslide",
                                               "fullslide_44") ~ 0.24,
                                   TRUE ~ 0.05),
         left_border = dplyr::case_when(class == "normal" ~ 0,
                                 class == "blog" ~ 0,
                                 type %in% c("fullslide_43",
                                             "fullslide_44") ~ (width - 22.16) / 2,
                                 type %in% c("fullslide", "fullslide_narrow") ~ (33.87 - width) / 2,
                                 type ==  "fullslide_half" ~ 1.085,
                                 type == "a4" ~ (width - 19) / 2,
                                 type == "fullslide_old169" ~ (width - 22.64) / 2,
                                 type == "blog_half" ~ blog_half_border),
         right_border = if_else(
           type == "fullslide_half", 17.485, # Position toward left of chart
           left_border
         ))

# The full table, used only by `grattan_save_pptx()` and the label helpers it
# calls. Everything else validates against the active-only `chart_types`.
chart_types_all <- chart_types
chart_types <- chart_types[chart_types$status == "active", ]

all_chart_types <- chart_types$type

# Deprecated types can still be exported to Powerpoint, and nothing else
pptx_legacy_types <- chart_types_all$type[chart_types_all$status == "deprecated"]

# Defunct types cannot be used anywhere, and are named only in error messages
defunct_chart_types <- chart_types_all$type[chart_types_all$status == "defunct"]

fullslide_chart_types <- chart_types$type[chart_types$class == "fullslide"]

# Types exported as web-ready PNGs by `grattan_save(save_web = TRUE)`: the
# 'normal' class plus 'blog', i.e. everything that isn't a Powerpoint slide
web_chart_types <- chart_types$type[chart_types$class != "fullslide"]

# Define standard fullslide slide dimensions (16:9 PowerPoint slide)
fullslide_slide_width <- 33.87   # cm
fullslide_slide_height <- 19.05  # cm

usethis::use_data(logogrob,
         chart_types,
         chart_types_all,
         all_chart_types,
         pptx_legacy_types,
         defunct_chart_types,
         fullslide_chart_types,
         web_chart_types,
         fullslide_slide_width,
         fullslide_slide_height,
         internal = TRUE,
         overwrite = TRUE)

chart_types_ext <- chart_types
usethis::use_data(chart_types_ext,
         internal = FALSE,
         overwrite = TRUE)

