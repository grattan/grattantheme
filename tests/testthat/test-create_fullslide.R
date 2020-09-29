p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_grattan() +
  labs(title = "Title goes here",
       subtitle = "Subtitle here",
       caption = "Notes: some notes. Source: source information")

test_that("create_fullslide works", {
  expect_error(create_fullslide(p, "normal"))

  full_p <- create_fullslide(p, "fullslide")

  expect_is(full_p, "patchwork")
  expect_length(full_p, 10)
  expect_false(is.null(full_p$patches))

})

test_that("saved fullslides look as expected", {
  fullslide_types <- chart_types$type[chart_types$class == "fullslide"]
  plots <- purrr::map(fullslide_types, create_fullslide, plot = p)
  plots <- setNames(plots, fullslide_types)
  filename <- file.path("test", "saved_plots.rda")

  if (!dir.exists("test")) dir.create("test")

  save(plots, file = filename)

  expect_true(file.exists(filename))

  saved_plots <- plots
  load(filename)

  expect_equal(saved_plots, plots)

  all.equal(saved_plots, plots)
})
