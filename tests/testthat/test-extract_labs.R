library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars,
             aes(x = wt, y = mpg)) +
  geom_point()

p2 <- mtcars %>%
  rownames_to_column("car") %>%
  ggplot(aes(x = reorder(car, mpg),
             y = mpg)) +
  geom_col() +
  coord_flip()

p <- p1 + p2 +
  patchwork::plot_annotation(title = "Some title",
                             subtitle = "My subtitle",
                             caption = "Notes and source")


test_that("label-less ggplot2 chart gives null list", {
  expect_null(extract_labs(p1)$title)
  expect_null(extract_labs(p1)$subtitle)
  expect_null(extract_labs(p1)$caption)
  expect_null(extract_labs(p2)$title)
})

test_that("labels extracted from patchwork plot", {
  expect_is(extract_labs(p), "list")
  expect_equal(extract_labs(p)$title, "Some title")
  expect_equal(extract_labs(p)$caption, "Notes and source")
})

test_that("warning emitted when labels in wrong place for patchwork", {
  p_labs <- p1 + p2 + labs(title = "A title in the wrong place")

  expect_warning(extract_labs(p_labs))
  expect_null(suppressWarnings(extract_labs(p_labs)$title))
})

test_that("labels in regular ggplot2 plot extracted", {
  p_regular <- p1 + labs(title = "A title goes here")
  expect_equal(extract_labs(p_regular)$title, "A title goes here")

})
