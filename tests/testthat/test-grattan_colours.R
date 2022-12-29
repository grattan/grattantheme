context("check Grattan palette functions work")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()



test_that("grattan colour functions work as expected", {

  expect_length(make_grattan_pal()(6), 6)
  expect_length(make_grattan_pal(reverse = TRUE)(6), 6)
  expect_length(make_grattan_pal(palette = "graph")(6), 6)
  expect_length(make_grattan_pal(palette = "diverging")(6), 6)

  expect_length(make_grattan_pal_discrete(6), 6)
  expect_length(make_grattan_pal_discrete(10), 10)

  expect_error(make_grattan_pal_discrete(11))

  expect_equal(make_grattan_pal_discrete(n = 5),
               c(grattan_orange,
                 grattan_lightred,
                 grattan_darkyellow,
                 grattan_darkorange,
                 grattan_darkred
                ))

  expect_s3_class(scale_colour_grattan(discrete = TRUE), "ScaleDiscrete")
  expect_s3_class(scale_colour_grattan(), "ScaleContinuous")
  expect_s3_class(scale_fill_grattan(discrete = TRUE), "ScaleDiscrete")
  expect_s3_class(scale_fill_grattan(), "ScaleContinuous")

  plot_w_col <- base_plot +
    geom_point(aes(col = factor(cyl))) +
    scale_colour_grattan(discrete = TRUE)

  expect_is(plot_w_col, "gg")

  plot_w_col_built <- ggplot_build(plot_w_col)

  expect_equal(plot_w_col_built$data[[2]]$colour[1], "#A1253E")

  expect_equal(length(unique(plot_w_col_built$data[[2]]$colour)), 3)

  vdiffr::expect_doppelganger("plot with three colours",
                              plot_w_col)
})


test_that("grattan continuous palette functions work as expected (colour)", {

  plot_cont <- ggplot(mtcars, aes(x = mpg, y = cyl)) +
    geom_point(aes(col = hp)) +
    scale_colour_grattan(discrete = FALSE)

  expect_is(plot_cont, "gg")

  plot_cont_built <- ggplot_build(plot_cont)

  expect_equal(length(unique(plot_cont_built$data[[1]]$colour)), 22)

  expect_equal(plot_cont_built$data[[1]]$colour[1], "#B04E37")

  vdiffr::expect_doppelganger("plot with continuous palette",
                              plot_cont)

})




test_that("grattan continuous palette functions work as expected (fill)", {

  plot_fill <-  ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Sepal.Length)) +
    geom_col() +
    scale_fill_grattan(discrete = FALSE)

  expect_is(plot_fill, "gg")

  plot_fill_built <- ggplot_build(plot_fill)

  expect_equal(length(unique(plot_fill_built$data[[1]]$fill)), 35)

  expect_equal(plot_fill_built$data[[1]]$fill[35], "#A34437")

  vdiffr::expect_doppelganger("plot with fill palette",
                              plot_fill)

})
