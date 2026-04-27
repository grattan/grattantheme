library(ggplot2)

blog_p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_grattan() +
  labs(title = "Blog title",
       subtitle = "Blog subtitle",
       caption = "Notes: some notes. Source: mtcars")

test_that("create_blog() returns a patchwork object", {
  out <- create_blog(blog_p, font = "normal")
  expect_is(out, "patchwork")
})

test_that("create_blog() errors on non-ggplot input", {
  expect_error(create_blog("not a plot", font = "normal"),
               "not a ggplot2 object")
  expect_error(create_blog(42, font = "normal"),
               "not a ggplot2 object")
})

test_that("create_blog() works with subtitle present", {
  out <- create_blog(blog_p, font = "normal")
  expect_is(out, "patchwork")
  # 4 elements: header, subtitle, plot, caption
  expect_length(out, 4)
})
