context("check Grattan palette functions work")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

test_that("grattan colour functions work as expected", {

  expect_length(grattan_pal(), 5)
  expect_length(grattan_pal(faded = TRUE), 5)

  expect_false(identical(grattan_pal(), grattan_pal(faded = TRUE)))

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

  expect_length(grattan_pal(n = 1, faded = TRUE), 1)
  expect_length(grattan_pal(n = 2, faded = TRUE), 2)
  expect_length(grattan_pal(n = "2a", faded = TRUE), 2)
  expect_length(grattan_pal(n = 3, faded = TRUE), 3)
  expect_length(grattan_pal(n = 4, faded = TRUE), 4)
  expect_length(grattan_pal(n = 5, faded = TRUE), 5)
  expect_length(grattan_pal(n = 6, faded = TRUE), 6)
  expect_length(suppressWarnings(grattan_pal(n = 7, faded = TRUE)), 7)
  expect_length(suppressWarnings(grattan_pal(n = 8, faded = TRUE)), 8)
  expect_length(suppressWarnings(grattan_pal(n = 9, faded = TRUE)), 9)
  expect_length(suppressWarnings(grattan_pal(n = 10, faded = TRUE)), 10)

  expect_warning(grattan_pal(n = 10))
  expect_warning(grattan_pal(n = 10, faded = TRUE))

  expect_error(grattan_pal(n = 11))
  expect_error(grattan_pal(n = 11, faded = TRUE))

  expect_equal(grattan_pal(n = 5),
               c(grattantheme::grattan_yellow, grattantheme::grattan_lightorange,
                 grattantheme::grattan_darkorange, grattantheme::grattan_red,
                 grattantheme::grattan_darkred))

  expect_equal(grattan_pal(n = 5, faded = TRUE),
               c(grattantheme::grattan_yellow_f, grattantheme::grattan_lightorange_f,
                 grattantheme::grattan_darkorange_f, grattantheme::grattan_red_f,
                 grattantheme::grattan_darkred_f))

  expect_s3_class(grattan_colour_manual(faded = TRUE), "ScaleDiscrete")

  plot_w_col <- base_plot +
    geom_point(aes(col = factor(cyl))) +
    grattan_colour_manual(n = 3)

  expect_is(plot_w_col, "gg")

  plot_w_col_built <- ggplot_build(plot_w_col)

  expect_equal(plot_w_col_built$data[[2]]$colour[1], "#F68B33")

  expect_equal(length(unique(plot_w_col_built$data[[2]]$colour)), 3)
})


test_that("grattan continuous palette functions work as expected (colour)", {

  plot_cont <- ggplot(mtcars, aes(x = mpg, y = cyl)) +
    geom_point(aes(col = hp)) +
    grattan_colour_manual(discrete = FALSE)

  expect_is(plot_cont, "gg")

  plot_cont_built <- ggplot_build(plot_cont)

  expect_equal(length(unique(plot_cont_built$data[[1]]$colour)), 22)

  expect_equal(plot_cont_built$data[[1]]$colour[1], "#CA4E29")


})




test_that("grattan continuous palette functions work as expected (fill)", {

  plot_fill <-  ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Sepal.Length)) +
    geom_col() +
    grattan_fill_manual(discrete = FALSE, palette = "full_f")

  expect_is(plot_fill, "gg")

  plot_fill_built <- ggplot_build(plot_fill)

  expect_equal(length(unique(plot_fill_built$data[[1]]$fill)), 35)

  expect_equal(plot_fill_built$data[[1]]$fill[150], "#F29F64")

})
