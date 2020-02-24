test_that("watermark adds watermark", {
  p <- mtcars %>%
    ggplot(aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan()

  w <- watermark("DRAFT")

  pw <- p + w

  expect_is(w, "LayerInstance")

  expect_is(pw, "gg")

  vdiffr::expect_doppelganger("plot with watermark", pw, path = "vdiffr-tests")

})
