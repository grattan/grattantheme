
context("vdiffr-tests")

base_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs",
       caption = "Notes: Blah Source: somewhere")

normal_plot <- base_plot +
  theme_grattan()

border_plot <- base_plot +
  facet_wrap(~cyl) +
  theme_grattan(panel_borders = TRUE)

coloured_plot <- base_plot +
  geom_point(aes(col = factor(cyl))) +
  grattan_colour_manual(n = 3) +
  theme_grattan()

# fullslide_plot <- grattantheme:::create_fullslide(normal_plot,
#                                                   type = "fullslide",
#                                                   height = NULL,
#                                                   warn_labs = FALSE) %>%
#   gridExtra::grid.arrange() %>%
#   ggplotify::as.ggplot()

test_that("normal plot looks correct", {

  vdiffr::expect_doppelganger("normal plot", normal_plot)

})

test_that("faceted plot with borders looks correct", {

  vdiffr::expect_doppelganger("border plot", border_plot)

})

test_that("plot with discrete colours looks correct", {

  vdiffr::expect_doppelganger("coloured plot", coloured_plot)

})


# test_that("fullslide plot looks correct", {
#
#   vdiffr::expect_doppelganger("fullslide plot", fullslide_plot)
# })

