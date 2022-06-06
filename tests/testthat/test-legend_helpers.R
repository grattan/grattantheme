test_that("colour_text works", {
  text <- colour_text(grattan_red, "fine")

  expect_type(text, "character")
})
