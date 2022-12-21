test_that("check_chart_aspect_ratio works", {
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  expect_silent(check_chart_aspect_ratio())

})
