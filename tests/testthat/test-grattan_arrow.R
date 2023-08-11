test_that("grattan_arrow plot appears correct", {

expect_equal(
  grattan_arrow(),
  grid::arrow(type = "closed", angle = 20, length = unit(0.5, "lines"))
)

})
