
base_plot <- ggplot2::msleep %>%
  ggplot(aes(x = reorder(name, sleep_total),
             y = sleep_total)) +
  geom_col() +
  theme_grattan()

grattan_axis_plot <- base_plot +
  grattan_y_continuous()

built_base_plot <- ggplot_build(base_plot)

built_grattan_axis_plot <- ggplot_build(grattan_axis_plot)

test_that("y-axis on base plot is below zero", {

  expect_lt(built_base_plot$layout$panel_params[[1]]$y.range[1], 0)

})

test_that("y-axis on plot with grattan scale is zero", {

  expect_equal(built_grattan_axis_plot$layout$panel_params[[1]]$y.range[1], 0)

})
