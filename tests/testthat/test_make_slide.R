context("Powerpoint slide creation")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

p_labs <- p + labs(title = "Title", subtitle = "Subtitle", caption = "Notes: Source:")

p_labs_2 <- p_labs + labs(title = "This is my second slide")

plot_list <- list(p_labs, p_labs_2)

# Tests
test_that("theme_grattan() creates ggplot2 object",{
  expect_is(p, "gg")
})

test_that("make_slide() saves .pptx object",{

  skip_if_not(pandoc_version() >= 2.1)

  make_slide(graph = p_labs, filename = "testslide.pptx")

  expect_true(file.exists("./testslide.pptx"))

  file.remove("./testslide.pptx")

})

test_that("make_presentation() saves .pptx object",{

  skip_if_not(pandoc_version() >= 2.1)

  make_presentation(plot_list, filename = "testpresentation.pptx",
                    title = "Test presentation", subtitle = "Subtitle")

  expect_true(file.exists("./testpresentation.pptx"))

  file.remove("./testpresentation.pptx")

})

