
base_y_plot <- ggplot2::msleep %>%
  ggplot(aes(x = reorder(name, sleep_total),
             y = sleep_total)) +
  geom_col() +
  theme_grattan()

base_x_plot <- ggplot(mtcars,
                      aes(x = wt, y = mpg)) +
  geom_point() +
  theme_grattan()

grattan_y_axis_plot <- base_y_plot +
  grattan_y_continuous()

grattan_x_axis_plot <- base_x_plot +
  grattan_x_continuous()

built_base_y_plot <- ggplot_build(base_y_plot)
built_base_x_plot <- ggplot_build(base_x_plot)

built_grattan_y_axis_plot <- ggplot_build(grattan_y_axis_plot)
built_grattan_x_axis_plot <- ggplot_build(grattan_x_axis_plot)

test_that("axis on base plot is below the data minimum", {

  expect_lt(built_base_y_plot$layout$panel_params[[1]]$y.range[1], 0)
  expect_lt(built_base_x_plot$layout$panel_params[[1]]$x.range[1], min(mtcars$wt))

})

test_that("axis on plot with grattan scale is equal to the dataminimum", {

  expect_equal(built_grattan_y_axis_plot$layout$panel_params[[1]]$y.range[1],
               0)
  expect_equal(built_grattan_x_axis_plot$layout$panel_params[[1]]$x.range[1],
               min(mtcars$wt))

})




test_that("manual expansion works", {

  expand_amount <- 0.2
  plot_with_expansion <- base_x_plot +
    scale_x_continuous_grattan(expand_left = expand_amount, expand_right = expand_amount) +
    scale_y_continuous_grattan(expand_top = expand_amount, expand_bottom = expand_amount)

  mtcars_x_range <- max(mtcars$wt) - min(mtcars$wt)
  mtcars_y_range <- max(mtcars$mpg) - min(mtcars$mpg)

  built_plot_with_expansion <- ggplot_build(plot_with_expansion)

  x_limits <- built_plot_with_expansion$layout$panel_params[[1]]$x.range
  x_min <- x_limits[[1]]
  x_max <- x_limits[[2]]

  y_limits <- built_plot_with_expansion$layout$panel_params[[1]]$y.range
  y_min <- y_limits[[1]]
  y_max <- y_limits[[2]]

  expect_equal(x_min, min(mtcars$wt) - (mtcars_x_range * expand_amount))
  expect_equal(x_max, max(mtcars$wt) + (mtcars_x_range * expand_amount))
  expect_equal(y_min, min(mtcars$mpg) - (mtcars_y_range * expand_amount))
  expect_equal(y_max, max(mtcars$mpg) + (mtcars_y_range * expand_amount))

})
