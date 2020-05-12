library(ggplot2)

p1 <- mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "My title",
       subtitle = "My subtitle")

p2 <- mtcars %>%
  ggplot(aes(x = mpg, y = wt)) +
  geom_point() +
  labs(title = "My title",
       subtitle = "My subtitle") +
  theme_grattan() +
  labs(title = "title",
       subtitle = "subtitle",
       caption = "Notes: notes. Source: source")

test_that("grattan_save_pptx_onetype() fails with inappropriate input", {
  temp <- tempfile(fileext = ".pptx")

  expect_true(grattan_save_pptx_onetype(list(p1), temp, type = "fullslide"))
  expect_true(file.exists(temp))
  unlink(temp)

  expect_true(grattan_save_pptx_onetype(list(p1, p2), temp, type = "fullslide"))
  expect_true(file.exists(temp))
  unlink(temp)

  expect_error(grattan_save_pptx_onetype(list(p1, letters), temp, type = "fullslide"))
})

test_that("grattan_save_pptx_onetype() works with patchwork objects", {
  p <- patchwork::wrap_plots(p1, p2)

  temp <- tempfile(fileext = ".pptx")
  expect_true(grattan_save_pptx_onetype(list(p), temp, type = "fullslide"))
  expect_true(file.exists(temp))
  unlink(temp)

})


