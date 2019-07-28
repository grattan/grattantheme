
base_plot <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point()

test_that("long title fails", {
  p <-  base_plot +
    labs(title = "This is a really long title that should fail wrap_labs with an error because it flows onto more than two lines, so it should definitely fail blah blah blah")

  expect_error(wrap_labs(p, type = "fullslide"))

  p <- base_plot +
    labs(title = "Regular title",
         subtitle = "Extremely long subtitle that should fail wrap_labs with an error, it'll take up too many lines even for a monster chart etc etc etc etc lorem ipsum lorem ipsum")

  expect_error(wrap_labs(p, type = "fullslide"))

})


test_that("two line title wraps", {

  p <- base_plot +
    labs(title = "This is a slightly long title that should wrap onto two lines but work blah blah blah")

  p <- wrap_labs(p, type = "fullslide")

  expect_true(grepl("\n", p$labels$title))

})

test_that("caption wraps onto two lines", {

  p <- base_plot +
    labs(caption = "Notes: notes go here. Source: source goes here.")

  p <- wrap_labs(p, type = "fullslide")

  expect_true(grepl("\n", p$labels$caption))

})

