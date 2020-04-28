
context("vdiffr-tests")

base_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       caption = "Notes: Blah Source: somewhere")

normal_plot <- base_plot +
  theme_grattan() +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

scatter_plot <- base_plot +
  theme_grattan(chart_type = "scatter") +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")


border_plot <- base_plot +
  facet_wrap(~cyl) +
  theme_grattan(panel_borders = TRUE) +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

coloured_plot <- base_plot +
  geom_point(aes(col = factor(cyl))) +
  grattan_colour_manual(n = 3) +
  theme_grattan() +
  labs(subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs")

no_subtitle_plot <- base_plot +
  theme_grattan()

short_subtitle_plot <- base_plot +
  theme_grattan() +
  labs(subtitle = "This is a short subtitle")

fullslide_plot <- normal_plot %>%
  wrap_labs("fullslide") %>%
  create_fullslide("fullslide")

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

  vdiffr::expect_doppelganger("fullslide plot", fullslide_plot)
})

