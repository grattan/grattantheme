context("Check that theme_grattan() and associated functions work")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

p_scatter <- base_plot + theme_grattan(chart_type = "scatter")

test_that("theme_grattan() is a theme object", {
  expect_is(theme_grattan(), "theme")
})

test_that("plot with theme_grattan() is a ggplot2 object",{
  expect_is(p, "gg")
})

test_that("theme conforms with style guide in key ways",{
  expect_equal(p$theme$text$size, 18)

  expect_equal(p$theme$line$colour, "#C3C7CB")

  expect_equal(p$theme$rect$fill, "white")

})

test_that("theme_grattan() arguments work",{
  p_flipped <- base_plot + theme_grattan(flipped = TRUE)

  expect_null(p_flipped$theme$panel.grid.major.x)
  expect_is(p_flipped$theme$panel.grid.major.y, "ggplot2::element_blank")

  p_orange <- base_plot + theme_grattan(background = "orange")

  expect_equal(p_orange$theme$rect$fill, "#FEF0DE")

  expect_equal(p$theme$legend.position, "none")

  p_legend <- base_plot + theme_grattan(legend = "top")

  expect_equal(p_legend$theme$legend.position, "top")

  # Normal plot:
  expect_equal(class(p$theme$axis.line.y)[1], "ggplot2::element_blank")

  # Scatter plot:
  expect_null(p_scatter$theme$axis.line.y)

})


test_that("theme_grattan() sends the right messages",{
  expect_message(base_plot +
                  theme_grattan(chart_type = "scatter", flipped = TRUE))

  expect_warning(base_plot +
                   theme_grattan(chart_type = "nah mate"))

})
