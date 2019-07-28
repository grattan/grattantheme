
context("vdiffr-tests")

normal_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  grattantheme::theme_grattan() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs",
       caption = "Notes: Blah Source: somewhere")

test_that("normal plot looks correct", {

  vdiffr::expect_doppelganger("normal plot", normal_plot)

})

# test_that("fullslide plot looks correct", {
#
#   fullslide_plot <- grattantheme:::create_fullslide(normal_plot,
#                                                     type = "fullslide",
#                                                     height = NULL,
#                                                     warn_labs = FALSE) %>%
#       gridExtra::grid.arrange() %>%
#       ggplotify::as.ggplot()
#
#
#   vdiffr::expect_doppelganger("fullslide plot", fullslide_plot)
# })
#
