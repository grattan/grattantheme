

context("vdiffr-tests")

# Create writer function for fullslide plots with specific dimensions
write_svg_fullslide <- function(plot, file, title = "") {
  # Convert cm to inches (1 cm = 0.393701 inches)
  width_inches <- fullslide_slide_width * 0.393701
  height_inches <- fullslide_slide_height * 0.393701

  # Use vdiffr's internal svglite function with custom dimensions
  vdiffr:::svglite(file, width = width_inches, height = height_inches)
  print(plot)
  grDevices::dev.off()
  invisible()
}

base_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here and it's very long",
       caption = "Notes: Blah Source: somewhere")

normal_plot <- base_plot +
  theme_grattan() +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

scatter_plot <- base_plot +
  theme_grattan(chart_type = "scatter") +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")


border_plot <- base_plot +
  facet_wrap( ~ cyl) +
  theme_grattan(panel_borders = TRUE) +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

coloured_plot <- base_plot +
  geom_point(aes(col = factor(cyl))) +
  theme_grattan() +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

no_subtitle_plot <- base_plot +
  theme_grattan()

short_subtitle_plot <- base_plot +
  theme_grattan() +
  labs(subtitle = "This is a short subtitle")

# Use font = "normal" for consistent tests across platforms
# (slide fonts may differ based on font availability)
fullslide_plot <- normal_plot %>%
  wrap_labs("fullslide") %>%
  create_fullslide("fullslide", font = "normal")

fullslide_narrow_plot <- normal_plot %>%
  wrap_labs("fullslide_narrow") %>%
  create_fullslide("fullslide_narrow", font = "normal")

fullslide_half_plot <- normal_plot %>%
  wrap_labs("fullslide_half") %>%
  create_fullslide("fullslide_half", font = "normal")

orange_plot <- base_plot +
  theme_grattan(background = "orange")


test_that("normal plot looks correct", {
  vdiffr::expect_doppelganger("normal plot", normal_plot)

})

test_that("scatter plot looks correct", {
  vdiffr::expect_doppelganger("scatter plot", scatter_plot)

})


test_that("faceted plot with borders looks correct", {
  vdiffr::expect_doppelganger("border plot", border_plot)

})

test_that("plot with discrete colours looks correct", {
  vdiffr::expect_doppelganger("coloured plot", coloured_plot)

})

test_that("plot with no subtitle fills the blank space", {
  vdiffr::expect_doppelganger("no subtitle plot", no_subtitle_plot)

})

test_that("plot with short subtitle fills the blank space", {
  vdiffr::expect_doppelganger("short subtitle plot", short_subtitle_plot)

})


test_that("fullslide plot looks correct", {
  vdiffr::expect_doppelganger("fullslide plot", fullslide_plot,
                              writer = write_svg_fullslide)
})

test_that("fullslide_narrow plot looks correct", {
  vdiffr::expect_doppelganger("fullslide narrow plot", fullslide_narrow_plot,
                              writer = write_svg_fullslide)
})

test_that("fullslide_half plot looks correct", {
  vdiffr::expect_doppelganger("fullslide half plot", fullslide_half_plot,
                              writer = write_svg_fullslide)
})

test_that("orange background returned when requested", {
  vdiffr::expect_doppelganger("orange background plot", orange_plot)
})
