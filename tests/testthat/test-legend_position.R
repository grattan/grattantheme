context("test legend placement")

base_plot <- ggplot(mtcars,
                    aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  scale_colour_manual(values = make_grattan_pal_discrete(3))

test_that("legend is in expected place (or absent)", {

  default_plot <- base_plot +
    theme_grattan()

  expect_equal(default_plot$theme$legend.position, "none")

  legend_top <- base_plot +
    theme_grattan(legend = "top")

  expect_equal(legend_top$theme$legend.position, "top")

  legend_within <- base_plot +
    theme_grattan(legend = c(0.5, 0.5))

  expect_identical(legend_within$theme$legend.position.inside, c(0.5, 0.5))

})
