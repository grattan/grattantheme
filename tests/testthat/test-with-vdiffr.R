
context("vdiffr-tests")

test_that("normal plot looks correct", {

  normal_plot <- mtcars %>%
    ggplot(aes(x = wt,
               y = mpg)) +
    geom_point() +
    grattantheme::theme_grattan() +
    labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
         subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs",
         caption = "Notes: Blah Source: somewhere")

  vdiffr::expect_doppelganger("normal plot", normal_plot)

})

# test_that("fullslide plot looks correct", {
#
#   fullslide_plot <- function() {
#
#     normal_plot <- mtcars %>%
#       ggplot(aes(x = wt,
#                  y = mpg)) +
#       geom_point() +
#       grattantheme::theme_grattan() +
#       labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
#            subtitle = "Either put units here or jam an elaborate thing here that describes both axes, whatevs",
#            caption = "Notes: Blah Source: somewhere")
#
#     grattantheme:::create_fullslide(normal_plot, type = "fullslide", height = NULL, warn_labs = FALSE) %>%
#       gridExtra::grid.arrange() %>%
#       ggpubr::as_ggplot()
#
#   }
#
#   vdiffr::expect_doppelganger("fullslide plot", fullslide_plot)
#
#
# })
#
