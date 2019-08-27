p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
     geom_point() +
     theme_grattan() +
     labs(title = "Title",
          subtitle = "Subtitle",
          caption = "Caption")

test_that("save_chartdata creates an xlsx workbook", {

  expect_invisible(save_chartdata("save_chartdata_test.xlsx", p))

  expect_true(file.exists("save_chartdata_test.xlsx"))

  unlink("../testthat/save_chartdata_test.xlsx")

  if(file.exists("../testthat/Rplots.pdf")) {
    unlink("../testthat/Rplots.pdf")
  }

})
