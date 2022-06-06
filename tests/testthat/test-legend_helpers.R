test_that("colour_text works", {
  text <- colour_text(grattan_red, "fine")

  text_not_bolder <- colour_text(grattan_red, "fine", bold_labs = FALSE, is_note = TRUE)

  expect_type(text, "character")

  expect_type(text_not_bolder, "character")
})
