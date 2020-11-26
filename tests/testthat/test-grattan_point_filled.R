test_that("grattan_point_filled() returns expected output", {
  expect_is(grattan_point_filled(), "ggproto")

  expect_equal(grattan_point_filled(),
                   ggplot2::geom_point(
                     size = 3,
                     stroke = 1.5,
                     fill = "white",
                     shape = 21
                   ))

})
