
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

test_that("grattan_y_continuous hides zero label when axis starts at 0", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    grattan_y_continuous(limits = c(0, NA)) +
    theme_grattan()

  built <- ggplot2::ggplot_build(p)

  expect_true(!is.null(p$scales$get_scales("y")))

  y_labels <- built$layout$panel_params[[1]]$y$get_labels()
  zero_positions <- built$layout$panel_params[[1]]$y$get_breaks() == 0

  if (any(zero_positions)) {
    expect_equal(y_labels[zero_positions], "")
  }
})

test_that("grattan_x_continuous hides zero label when axis starts at 0", {
  p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point() +
    grattan_x_continuous(limits = c(0, NA)) +
    theme_grattan()

  built <- ggplot2::ggplot_build(p)

  expect_true(!is.null(p$scales$get_scales("x")))

  x_labels <- built$layout$panel_params[[1]]$x$get_labels()
  zero_positions <- built$layout$panel_params[[1]]$x$get_breaks() == 0

  if (any(zero_positions)) {
    expect_equal(x_labels[zero_positions], "")
  }
})

test_that("custom labels parameter is respected", {
  custom_labels <- function(x) paste0("$", x)

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    grattan_y_continuous(limits = c(0, NA), labels = custom_labels) +
    theme_grattan()

  built <- ggplot2::ggplot_build(p)

  y_labels <- built$layout$panel_params[[1]]$y$get_labels()
  zero_positions <- built$layout$panel_params[[1]]$y$get_breaks() == 0

  if (any(zero_positions)) {
    expect_equal(y_labels[zero_positions], "$0")
  }
})

test_that("non-zero starting axes show all labels including zero if present", {
  df <- data.frame(x = 1:10, y = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5))
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    grattan_y_continuous() +
    theme_grattan()

  built <- ggplot2::ggplot_build(p)

  y_breaks <- built$layout$panel_params[[1]]$y$get_breaks()
  y_labels <- built$layout$panel_params[[1]]$y$get_labels()

  expect_true(any(y_breaks < 0, na.rm = TRUE))

  zero_positions <- !is.na(y_breaks) & y_breaks == 0
  if (any(zero_positions)) {
    zero_label <- y_labels[zero_positions]
    expect_true(zero_label != "")
  }
})

test_that("abbreviated label argument is matched to labels", {
  y_scale <- grattan_y_continuous(label = scales::label_percent(scale = 1),
                                  limits = c(0, 100))
  x_scale <- grattan_x_continuous(label = scales::label_percent(scale = 1))

  expect_equal(y_scale$get_labels(c(0, 50, 100)), c("0%", "50%", "100%"))
  expect_equal(x_scale$get_labels(c(0, 50, 100)), c("0%", "50%", "100%"))
})

test_that("scale_*_continuous_grattan pass labels through", {
  y_scale <- scale_y_continuous_grattan(label = scales::label_percent(scale = 1))
  x_scale <- scale_x_continuous_grattan(labels = scales::label_percent(scale = 1))

  expect_equal(y_scale$get_labels(c(0, 50)), c("0%", "50%"))
  expect_equal(x_scale$get_labels(c(0, 50)), c("0%", "50%"))
})

test_that("expand argument sets the axis expansion", {
  custom_expand <- ggplot2::expansion(mult = c(0.1, 0.2))

  expect_equal(grattan_y_continuous(expand = custom_expand)$expand,
               custom_expand)
  expect_equal(grattan_x_continuous(expand = custom_expand)$expand,
               custom_expand)
  expect_equal(scale_y_continuous_grattan(expand = custom_expand)$expand,
               custom_expand)
  expect_equal(scale_x_continuous_grattan(expand = custom_expand)$expand,
               custom_expand)
})

test_that("expand_* arguments still work and are used by default", {
  expect_equal(grattan_y_continuous()$expand,
               ggplot2::expansion(mult = c(0, 0.015)))
  expect_equal(scale_x_continuous_grattan(expand_left = 0.1)$expand,
               ggplot2::expansion(mult = c(0.1, 0.015)))
})

test_that("supplying expand with expand_* warns, and expand wins", {
  custom_expand <- ggplot2::expansion(mult = c(0.1, 0.2))

  expect_warning(y_scale <- grattan_y_continuous(expand_top = 0.5,
                                                 expand = custom_expand),
                 "ignored")
  expect_equal(y_scale$expand, custom_expand)

  expect_warning(scale_x_continuous_grattan(expand_left = 0.5,
                                            expand = custom_expand),
                 "ignored")

  # The aliases shouldn't warn just because they pass expand_* along
  expect_no_warning(scale_y_continuous_grattan(expand = custom_expand))
  expect_no_warning(scale_x_continuous_grattan(expand = custom_expand))
})
