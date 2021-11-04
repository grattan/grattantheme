
base_plot <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point()

test_that("long title fails", {
  p <-  base_plot +
    labs(title = "This is a really long title that should fail wrap_labs with an error because it flows onto more than two lines, so it should definitely fail blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah")

  expect_error(wrap_labs(p, type = "fullslide", ignore_long_title = FALSE))

  p <- base_plot +
    labs(title = "Regular title",
         subtitle = "Extremely long subtitle that should fail wrap_labs with an error, it'll take up too many lines even for a monster chart etc etc etc etc lorem ipsum lorem ipsum blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah")

  expect_error(wrap_labs(p, type = "fullslide", ignore_long_title = FALSE))

})

test_that("long title doesn't fail when ignored", {
  p <-  base_plot +
    labs(title = "This is a really long title that should fail wrap_labs with an error because it flows onto more than two lines, so it should definitely fail blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah")

  expect_type(wrap_labs(p, type = "fullslide"), "list")


  p <- base_plot +
    labs(title = "Regular title",
         subtitle = "Extremely long subtitle that should fail wrap_labs with an error, it'll take up too many lines even for a monster chart etc etc etc etc lorem ipsum lorem ipsum blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah")

  expect_type(wrap_labs(p, type = "fullslide"), "list")

})



test_that("two line title wraps", {

  p <- base_plot +
    labs(title = "This is a slightly long title that should wrap onto two lines but work blah blah blah")

  p <- wrap_labs(p, type = "fullslide")

  expect_true(grepl("\n", p$labels$title))

})

test_that("caption wraps onto two lines", {

  p <- base_plot +
    labs(caption = "Notes: Some extremely long notes go here, because sometimes (often!) Grattan people like to append extremely long notes to the bottom of their charts, you have probably noticed this already huh. Source: source goes here.")

  p <- wrap_labs(p, type = "fullslide")

  expect_true(grepl("\n", p$labels$caption))

})

test_that("wrap_labs() only wraps the label you want it to", {

  p <- base_plot +
    labs(title = "This is a slightly long title that should wrap onto two lines but work blah blah blah",
         subtitle = "This is a slightly long title that should wrap onto two lines but work blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah",
         caption = "Notes: these are some notes. Source: this is the source.")

  title_only <- wrap_labs(p, "fullslide", "title")
  subtitle_only <- wrap_labs(p, "fullslide", "subtitle")
  caption_only <- wrap_labs(p, "fullslide", "caption")

  expect_true(grepl("\n", title_only$labels$title))
  expect_false(grepl("\n", title_only$labels$subtitle))
  expect_false(grepl("\n", title_only$labels$caption))

  expect_true(grepl("\n", subtitle_only$labels$subtitle))
  expect_false(grepl("\n", subtitle_only$labels$title))
  expect_false(grepl("\n", subtitle_only$labels$caption))

  expect_true(grepl("\n", caption_only$labels$caption))
  expect_false(grepl("\n", caption_only$labels$title))
  expect_false(grepl("\n", caption_only$labels$subtitle))

})
