context("Powerpoint slide creation")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

# Tests
test_that("theme_grattan() creates ggplot2 object",{
  expect_is(p, "gg")
})

test_that("make_slide() saves .pptx object",{

  p_labs <- p + labs(title = "Title", subtitle = "Subtitle", caption = "Notes: Source:")

  make_slide(graph = p_labs, filename = "testslide.pptx")

  expect_that(file.exists("./testslide.pptx"), equals(TRUE))

  file.remove("./testslide.pptx")

})

