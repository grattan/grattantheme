test_that("check_chart works", {
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  expect_silent(check_chart())
  expect_silent(check_chart("normal"))
})

test_that("check_chart_aspect_ratio is soft-deprecated", {
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  lifecycle::expect_deprecated(check_chart_aspect_ratio())
})
