context("test that default font settings remain unchanged")

base_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       subtitle = "My subtitle",
       caption = "Notes: Blah Source: somewhere") +
  theme_grattan()

base_plot$theme$text

test_that("default font settings are unchanged", {
  expect_equal(base_plot$theme$text$family, "sans")
  expect_equal(base_plot$theme$text$size, 18)

})

test_that("apply_font_to_geom_text leaves the original plot's fonts alone", {

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = cyl))

  slide <- apply_font_to_geom_text(p, "Avenir Next")

  expect_identical(slide$layers[[2]]$aes_params$family, "Avenir Next")
  # Saving one chart type must not fix the font for the types saved after it
  expect_null(p$layers[[2]]$aes_params$family)

  normal <- apply_font_to_geom_text(p, "sans")
  expect_identical(normal$layers[[2]]$aes_params$family, "sans")
  expect_identical(slide$layers[[2]]$aes_params$family, "Avenir Next")
})

test_that("apply_font_to_geom_text respects a user-specified font", {

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_text(ggplot2::aes(label = cyl), family = "Times")

  expect_identical(
    apply_font_to_geom_text(p, "Avenir Next")$layers[[1]]$aes_params$family,
    "Times")
})
