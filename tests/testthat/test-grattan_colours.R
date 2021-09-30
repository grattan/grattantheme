context("check Grattan palette functions work")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

test_that("grattan colour functions work as expected", {

  expect_length(grattan_pal(), 5)
  expect_length(grattan_pal(faded_level = 1), 5)
  expect_length(grattan_pal(faded_level = 2), 5)
  expect_length(grattan_pal(faded_level = 3), 5)
  expect_length(grattan_pal(faded_level = 4), 5)
  expect_length(grattan_pal(faded_level = 5), 5)
  expect_length(grattan_pal(faded_level = 6), 5)
  expect_length(grattan_pal(faded_level = 7), 5)
  expect_length(grattan_pal(faded_level = 8), 5)

  expect_identical(grattan_pal(), grattan_pal(faded_level = 0))
  expect_false(identical(grattan_pal(), grattan_pal(faded_level = 1)))

  expect_length(grattan_pal(n = 1), 1)
  expect_length(grattan_pal(n = 2), 2)
  expect_length(grattan_pal(n = "2a"), 2)
  expect_length(grattan_pal(n = 3), 3)
  expect_length(grattan_pal(n = 4), 4)
  expect_length(grattan_pal(n = 5), 5)
  expect_length(grattan_pal(n = 6), 6)
  expect_length(suppressWarnings(grattan_pal(n = 7)), 7)
  expect_length(suppressWarnings(grattan_pal(n = 8)), 8)
  expect_length(suppressWarnings(grattan_pal(n = 9)), 9)
  expect_length(suppressWarnings(grattan_pal(n = 10)), 10)
  expect_length(grattan_pal(n = 3, reverse = TRUE), 3)

  expect_length(grattan_pal(n = 1, faded_level = 1), 1)
  expect_length(grattan_pal(n = 2, faded_level = 1), 2)
  expect_length(grattan_pal(n = "2a", faded_level = 1), 2)
  expect_length(grattan_pal(n = 3, faded_level = 1), 3)
  expect_length(grattan_pal(n = 4, faded_level = 1), 4)
  expect_length(grattan_pal(n = 5, faded_level = 1), 5)
  expect_length(grattan_pal(n = 6, faded_level = 1), 6)
  expect_length(suppressWarnings(grattan_pal(n = 7, faded_level = 1)), 7)
  expect_length(suppressWarnings(grattan_pal(n = 8, faded_level = 1)), 8)
  expect_length(suppressWarnings(grattan_pal(n = 9, faded_level = 1)), 9)
  expect_length(suppressWarnings(grattan_pal(n = 10, faded_level = 1)), 10)

  expect_warning(grattan_pal(n = 10, faded = TRUE))

  expect_error(grattan_pal(n = 11))
  expect_error(grattan_pal(n = 11, faded_level = 6))

  expect_equal(grattan_pal(n = 5),
               c(grattantheme::grattan_darkyellow,
                 grattantheme::grattan_lightorange,
                 grattantheme::grattan_darkorange,
                 grattantheme::grattan_lightred,
                 grattantheme::grattan_darkred))

  expect_equal(grattan_pal(n = 5, faded_level = 4),
               c(grattantheme::grattan_darkyellow4,
                 grattantheme::grattan_lightorange4,
                 grattantheme::grattan_darkorange4,
                 grattantheme::grattan_lightred4,
                 grattantheme::grattan_darkred4))

  expect_s3_class(grattan_colour_manual(faded_level = 2), "ScaleDiscrete")
  expect_s3_class(grattan_colour_manual(discrete = FALSE), "ScaleContinuous")

  plot_w_col <- base_plot +
    geom_point(aes(col = factor(cyl))) +
    grattan_colour_manual(n = 3)

  expect_is(plot_w_col, "gg")

  plot_w_col_built <- ggplot_build(plot_w_col)

  expect_equal(plot_w_col_built$data[[2]]$colour[1], "#F68B33")

  expect_equal(length(unique(plot_w_col_built$data[[2]]$colour)), 3)

  vdiffr::expect_doppelganger("plot with three colours",
                              plot_w_col)
})


test_that("grattan continuous palette functions work as expected (colour)", {

  plot_cont <- ggplot(mtcars, aes(x = mpg, y = cyl)) +
    geom_point(aes(col = hp)) +
    grattan_colour_manual(discrete = FALSE)

  expect_is(plot_cont, "gg")

  plot_cont_built <- ggplot_build(plot_cont)

  expect_equal(length(unique(plot_cont_built$data[[1]]$colour)), 22)

  expect_equal(plot_cont_built$data[[1]]$colour[1], "#CA4E29")

  vdiffr::expect_doppelganger("plot with continuous palette",
                              plot_cont)

})




test_that("grattan continuous palette functions work as expected (fill)", {

  plot_fill <-  ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Sepal.Length)) +
    geom_col() +
    grattan_fill_manual(discrete = FALSE, palette = "full_f")

  expect_is(plot_fill, "gg")

  plot_fill_built <- ggplot_build(plot_fill)

  expect_equal(length(unique(plot_fill_built$data[[1]]$fill)), 35)

  expect_equal(plot_fill_built$data[[1]]$fill[150], "#F29F64")

  vdiffr::expect_doppelganger("plot with fill palette",
                              plot_fill)

})
