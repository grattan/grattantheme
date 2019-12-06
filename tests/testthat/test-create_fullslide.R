test_that("create_fullslide works", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan() +
    labs(title = "Title goes here",
         subtitle = "Subtitle here",
         caption = "Notes: some notes. Source: source information")

  expect_error(create_fullslide(p, "normal"))

  full_p <- create_fullslide(p, "fullslide")

  expect_is(full_p, "gtable")
  expect_length(full_p, 3)

})
