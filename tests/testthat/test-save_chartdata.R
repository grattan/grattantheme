
library(patchwork)

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

test_that("save_chartdata works with patchwork plots", {

  # Create two simple plots with the same data
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan()

  p2 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point() +
    theme_grattan()

  # Combine with patchwork
  p_combined <- p1 + p2 +
    plot_annotation(title = "Combined plots",
                    subtitle = "Two charts side by side",
                    caption = "Source: mtcars")

  save_chartdata("patchwork_test.xlsx", p_combined, select_data = FALSE)

  expect_true(file.exists("patchwork_test.xlsx"))

  # Get the data used in each component plot
  p1_data <- p1$data
  p2_data <- p2$data

  # Calculate positions for reading the two data boxes
  # First box starts at row 3, column 2
  first_box_rows <- nrow(p1_data)
  # Second box starts 2 rows after first box ends
  second_box_start <- 3 + first_box_rows + 2

  # Read first data box
  saved_data_1 <- openxlsx::read.xlsx("patchwork_test.xlsx",
                                      rows = 3:(3 + first_box_rows),
                                      cols = 2:(ncol(p1_data)+1))

  # Read second data box
  saved_data_2 <- openxlsx::read.xlsx("patchwork_test.xlsx",
                                      rows = second_box_start:(second_box_start + nrow(p2_data)),
                                      cols = 2:(ncol(p2_data)+1))

  # Make column names lowercase to match
  names(saved_data_1) <- tolower(names(saved_data_1))
  names(saved_data_2) <- tolower(names(saved_data_2))

  # Remove named rows
  rownames(p1_data) <- NULL
  rownames(p2_data) <- NULL

  # Compare data
  expect_equal(saved_data_1, p1_data)
  expect_equal(saved_data_2, p2_data)

  unlink("patchwork_test.xlsx")

  if(file.exists("Rplots.pdf")) {
    unlink("Rplots.pdf")
  }
})

test_that("save_chartdata handles patchwork without proper annotations", {

  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan()

  p2 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point() +
    theme_grattan()

  # Combine without annotations
  p_no_annotations <- p1 + p2

  expect_warning(save_chartdata("patchwork_no_annotations.xlsx", p_no_annotations),
                 "No subtitle found")
  expect_warning(save_chartdata("patchwork_no_annotations.xlsx", p_no_annotations),
                 "No caption found")

  expect_true(file.exists("patchwork_no_annotations.xlsx"))

  unlink("patchwork_no_annotations.xlsx")

  if(file.exists("Rplots.pdf")) {
    unlink("Rplots.pdf")
  }
})
